library(shiny)
library(bslib)
library(DT)
library(shinyjs)
library(xgboost)
library(dplyr)
library(tidyr)
library(ggplot2)

ui <- navbarPage("Kidney Rejection Risk App",
                 
                 tabPanel("Home",
                          fluidPage(
                            titlePanel("Two-Stage Diagnostic App"),
                            
                            h3("🧭 Clinical Overview"),
                            
                            p("This Shiny application provides a clinically-informed, minimally invasive tool for assessing kidney transplant rejection risk using gene expression data derived from patient samples. The diagnostic workflow addresses two core clinical questions:"),
                            
                            tags$div(
                              HTML("<p><strong>1. Is the patient experiencing stable graft function or a rejection episode?</strong></p>"),
                              HTML("<p><strong>2. If rejection is detected, is it classified as acute or chronic?</strong></p>")
                            ),
                            
                            h4("Key Advantages of This Approach:"),
                            
                            tags$ul(
                              tags$li(strong("Minimizes unnecessary blood tests by only advancing to Stage 2 when rejection is suspected.")),
                              tags$li(strong("Reduces patient burden through the use of non-invasive urine-based diagnostics.")),
                              tags$li(strong("Enables early intervention by providing timely, targeted insights."))
                            ),
                            
                            div(
                              style = "margin-top: 30px; text-align: center;",
                              
                              tags$h4("🔬 Gene Expression Highlights"),
                              
                              tags$div(
                                tags$img(src = "pic_ac.jpg", style = "max-width: 90%; height: auto; margin-bottom: 30px;")
                              ),
                              
                              tags$div(
                                tags$img(src = "pic_ch.jpg", style = "max-width: 90%; height: auto;")
                              )
                            )
                            
                            
                          )
                 ),
                 
                 tabPanel("Stage 1: Stable vs Rejection",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("input_stage1", "Upload Gene Expression CSV", accept = ".csv"),
                              hr(),
                              h5("Patient ID"),
                              div(style = "max-height: 300px; overflow-y: auto; border: 1px solid #ccc;",
                                  DT::dataTableOutput("patient_id_table1")
                              ),
                              h6("🔍 Choose Model"),
                              actionButton("predict_rf1", "Predict"),
                              actionButton("clear_output1", "🧹 Clear Predictions"),
                              helpText("Select a patient first, then click a model.")
                            ),
                            mainPanel(
                              h4("Prediction Output"),
                              div(
                                style = "max-height: 250px; overflow-y: auto; border: 1px solid #ddd; padding: 12px; background-color: #f9f9f9; border-radius: 6px;",
                                uiOutput("output_stage1_html")
                              ),
                              h4("📊 Gene Expression Overview"),
                              plotOutput("plot_stage1")
                            )
                          )
                 ),
                 
                 tabPanel("Stage 2: Acute vs Chronic",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("input_stage2", "Upload Gene Expression CSV", accept = ".csv"),
                              hr(),
                              h5("Patient ID"),
                              div(style = "max-height: 300px; overflow-y: auto; border: 1px solid #ccc;",
                                  DT::dataTableOutput("patient_id_table")
                              ),
                              h6("🔍 Choose Model"),
                              actionButton("predict_rf", "Predict"),
                              actionButton("clear_output", "🧹 Clear Predictions"),
                              helpText("Select a patient first, then click a model.")
                            ),
                            mainPanel(
                              h4("Prediction Output"),
                              div(
                                style = "max-height: 250px; overflow-y: auto; border: 1px solid #ddd; padding: 12px; background-color: #f9f9f9; border-radius: 6px;",
                                uiOutput("output_stage2_html")
                              ),
                              h4("📊 Gene Expression Overview"),
                              plotOutput("plot_stage2")
                            )
                          )
                 )
)

# === Load data & model ===
train_data_global <- read.csv("data/blood_train_data.csv", check.names = FALSE)
train_data_global2 <- read.csv("data/urine_train.csv", check.names = FALSE)
glmnet_model_blood <- readRDS("model/blood_model_glm.rds")
rf_model_urine <- readRDS("model/urine_model_rf.rds")

server <- function(input, output, session) {
  
  #=================stage 1==========================
  
  
  test_data_stage1 <- reactiveVal(NULL)
  prediction_log_stage1 <- reactiveVal("")
  used_models_stage1 <- reactiveValues()
  
  observeEvent(input$input_stage1, {
    req(input$input_stage1)
    df_raw <- read.csv(input$input_stage1$datapath, check.names = FALSE)
    rownames(df_raw) <- df_raw$X
    df_clean <- df_raw[, -which(names(df_raw) == "X")]
    test_data_stage1(df_clean)
  })
  
  
  
  output$patient_id_table1 <- renderDataTable({
    req(test_data_stage1())
    id_df <- data.frame(PatientID = rownames(test_data_stage1()))
    DT::datatable(id_df,
                  selection = "single",
                  options = list(dom = 't', pageLength = nrow(id_df), scrollY = "300px", scrollCollapse = TRUE),
                  rownames = FALSE)
  })
  selected_patient_stage1 <- reactive({
    req(test_data_stage1())
    row_idx <- input$patient_id_table1_rows_selected
    if (is.null(row_idx) || length(row_idx) == 0) return(NULL)
    df <- test_data_stage1()
    df[row_idx, , drop = FALSE]
  })
  observeEvent(input$predict_rf1, {
    patient <- selected_patient_stage1()
    if (is.null(patient)) {
      showNotification("Please select a patient row first.", type = "warning")
      return()
    }
    
    pid <- rownames(patient)
    if (!is.null(used_models_stage1[[paste0(pid, "_glmnet")]])) {
      showNotification(paste("Model already applied to", pid), type = "message")
      return()
    }
    
    df_features <- patient[, -1, drop = FALSE]
    
    pred <- predict(glmnet_model_blood, newx = as.matrix(df_features), type = "raw")
    pred_prob <- predict(glmnet_model_blood, newdata = as.matrix(df_features), type = "prob")
    pred_label <- colnames(pred_prob)[which.max(pred_prob)]
    confidence <- max(pred_prob)
    
    used_models_stage1[[paste0(pid, "_glmnet")]] <- TRUE
    
    log_old <- prediction_log_stage1()
    log_new <- paste0(
      log_old,
      "\nID: ", pid,
      "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Outcome: ", pred_label,
      "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Confidence: ", sprintf("%.1f%%", confidence * 100)
    )
    
    prediction_log_stage1(log_new)
    
    # === 替换 Stage 1 的 barplot ===
    output$plot_stage1 <- renderPlot({
      
      req(train_data_global)  # ⬅ 你需要提前定义这个变量（见下方说明）
      patient <- selected_patient_stage1()
      if (is.null(patient)) return(NULL)
      
      # 设置 top10 基因（确保这些列在 train_data 和 test_data 中都存在）
      top10_genes <- c("PTGS2", "YTHDF3.AS1", "CDKN3", "KIF11", "PDE8A", 
                       "CCNB1", "HEY1", "LETM2", "TOP2A", "HELLS")
      
      # 准备绘图数据
      plot_data <- train_data_global[, c("Status", top10_genes)]
      long_data <- tidyr::pivot_longer(plot_data, cols = -Status, names_to = "Gene", values_to = "Expression")
      
      pid <- rownames(patient)
      patient_data <- patient[, top10_genes]
      
      ref_lines <- data.frame(
        Gene = names(patient_data),
        PatientValue = as.numeric(patient_data)
      )
      
      ggplot2::ggplot(long_data, ggplot2::aes(x = Status, y = Expression, fill = Status)) +
        ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.6) +
        ggplot2::geom_hline(data = ref_lines, ggplot2::aes(yintercept = PatientValue),
                            color = "red", linetype = "dashed", linewidth = 0.8) +
        ggplot2::facet_wrap(~ Gene, scales = "free_y", ncol = 5) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::labs(title = paste("Top 10 Gene Expression (Patient:", pid, ")"),
                      subtitle = "Red dashed line = selected patient's expression",
                      x = "Group", y = "Expression") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                       strip.text = ggplot2::element_text(size = 11),
                       legend.position = "none")
    })
    
  })
  observeEvent(input$clear_output1, {
    prediction_log_stage1("")
    output$plot_stage1 <- renderPlot({ NULL })
    for (name in names(used_models_stage1)) {
      used_models_stage1[[name]] <- NULL
    }
  })
  output$output_stage1_html <- renderUI({
    req(prediction_log_stage1())
    
    lines <- unlist(strsplit(prediction_log_stage1(), "\n"))
    colored_lines <- lapply(lines, function(line) {
      if (!grepl("Outcome:", line)) {
        return(sprintf("<span style='font-size: 18px; font-family: Times New Roman;'>%s</span>", line))
      }
      
      parts <- strsplit(line, "Outcome:")[[1]]
      prefix_part <- trimws(parts[1])  # "ID: GSMxxxxx"
      result_full <- trimws(parts[2])  # "Chronic Confidence: 97.8%"
      
      # 提取 Outcome 和 Confidence
      outcome_only <- sub("^(\\w+).*", "\\1", result_full)  # "Chronic"
      confidence_only <- sub(".*Confidence: ", "", result_full)  # "97.8%"
      
      # 提取 ID
      id_value <- trimws(strsplit(prefix_part, "ID:")[[1]][2])
      
      
      result_color <- switch(
        tolower(outcome_only),
        "rejection" = "red",
        "stable"    = "green",
        "#333"
      )
      
      sprintf(
        paste0(
          "<div style='display: flex; justify-content: space-between; font-size: 16px;'>",
          "<span style='font-family: Georgia; font-weight: bold;'>ID:</span> ",
          "<span style='font-family: Times New Roman; font-weight: 650;'>%s</span>",
          
          "<span style='margin-left: 50px; font-family: Georgia; font-weight: bold;'>Outcome:</span> ",
          "<span style='font-family: Times New Roman; font-weight: 650; color: %s;'>%s</span>",
          
          "<span style='margin-left: 50px; font-family: Georgia; font-weight: bold;'>Confidence:</span> ",
          "<span style='font-family: Times New Roman; font-weight: 650;'>%s</span>",
          "</div>"
        ),
        id_value, result_color, outcome_only, confidence_only
      )
    })
    
    HTML(paste(colored_lines, collapse = "<br>"))
  })
  
  # ==== Stage 2 ====
  test_data <- reactiveVal(NULL)
  prediction_log <- reactiveVal("")
  used_models <- reactiveValues()
  
  observeEvent(input$input_stage2, {
    df_raw <- read.csv(input$input_stage2$datapath, check.names = FALSE)
    
    if ("X" %in% colnames(df_raw)) {
      rownames(df_raw) <- df_raw$X
      test_data(df_raw)  # 不删除任何列，保留所有数据
    } else {
      showNotification("Uploaded file is missing 'X' column for rownames.", type = "error")
      return()
    }
  })
  
  output$patient_id_table <- renderDataTable({
    req(test_data())
    id_df <- data.frame(PatientID = rownames(test_data()))
    DT::datatable(id_df, selection = "single", options = list(dom = 't', pageLength = nrow(id_df), scrollY = "300px"), rownames = FALSE)
  })
  
  selected_patient <- reactive({
    req(test_data())
    idx <- input$patient_id_table_rows_selected
    if (is.null(idx)) return(NULL)
    test_data()[idx, , drop = FALSE]
  })
  
  observeEvent(input$predict_rf, {
    patient <- selected_patient()
    if (is.null(patient)) return(showNotification("Please select a patient.", type = "warning"))
    
    pid <- rownames(patient)
    if (!is.null(used_models[[paste0(pid, "_rf")]])) return()
    
    model_features <- setdiff(colnames(train_data_global2), "Status")
    missing_features <- setdiff(model_features, colnames(patient))
    for (feat in missing_features) {
      patient[[feat]] <- 0
    }
    df_features <- patient[, model_features, drop = FALSE]
    
    pred_prob <- predict(rf_model_urine, newdata = df_features, type = "prob")
    pred_label <- colnames(pred_prob)[which.max(pred_prob)]
    confidence <- max(pred_prob)
    used_models[[paste0(pid, "_rf")]] <- TRUE
    log_new <- paste0(prediction_log(), "\nID: ", pid, " Outcome: ", pred_label, " Confidence: ", sprintf("%.1f%%", confidence * 100))
    prediction_log(log_new)
    
    top10_genes <- c("LGSN", "OLFM3", "DNAJC24", "UPRT", "LCORL", "PPP3CA", "HERPUD2", "APELA", "PPAT", "ZFP37")
    available_genes <- intersect(top10_genes, colnames(patient))
    if (length(available_genes) == 0) {
      showNotification("No top10 genes found in uploaded data.", type = "error")
      return()
    }
    
    ref_lines <- data.frame(
      Gene = available_genes,
      PatientValue = as.numeric(patient[, available_genes])
    )
    
    long_data <- pivot_longer(train_data_global2[, c("Label", top10_genes)], cols = -Label, names_to = "Gene", values_to = "Expression")
    
    output$plot_stage2 <- renderPlot({
      ggplot(long_data, aes(x = Label, y = Expression, fill = Label)) +
        geom_boxplot(outlier.shape = NA, alpha = 0.6) +
        geom_hline(data = ref_lines, aes(yintercept = PatientValue), color = "red", linetype = "dashed") +
        facet_wrap(~ Gene, scales = "free_y", ncol = 5) +
        theme_minimal()
    })
  })
  
  observeEvent(input$clear_output, {
    prediction_log("")
    output$plot_stage2 <- renderPlot({ NULL })
    for (name in names(used_models)) used_models[[name]] <- NULL
  })
  
  # 🚀 使用自定义美化后的 renderUI
  output$output_stage2_html <- renderUI({
    req(prediction_log())
    
    lines <- unlist(strsplit(prediction_log(), "\n"))
    colored_lines <- lapply(lines, function(line) {
      if (!grepl("Outcome:", line)) {
        return(sprintf("<span style='font-size: 18px; font-family: Times New Roman;'>%s</span>", line))
      }
      parts <- strsplit(line, "Outcome:")[[1]]
      prefix_part <- trimws(parts[1])
      result_full <- trimws(parts[2])
      outcome_only <- sub("^(\\w+).*", "\\1", result_full)
      confidence_only <- sub(".*Confidence: ", "", result_full)
      id_value <- trimws(strsplit(prefix_part, "ID:")[[1]][2])
      result_color <- switch(tolower(outcome_only), "chronic" = "red", "acute" = "blue", "#333")
      sprintf(
        paste0(
          "<div style='display: flex; justify-content: space-between; font-size: 16px;'>",
          "<span style='font-family: Georgia; font-weight: bold;'>ID:</span> ",
          "<span style='font-family: Times New Roman; font-weight: 650;'>%s</span>",
          "<span style='margin-left: 50px; font-family: Georgia; font-weight: bold;'>Outcome:</span> ",
          "<span style='font-family: Times New Roman; font-weight: 650; color: %s;'>%s</span>",
          "<span style='margin-left: 50px; font-family: Georgia; font-weight: bold;'>Confidence:</span> ",
          "<span style='font-family: Times New Roman; font-weight: 650;'>%s</span>",
          "</div>"
        ),
        id_value, result_color, outcome_only, confidence_only
      )
    })
    HTML(paste(colored_lines, collapse = "<br>"))
  })
  
  
  observeEvent(input$clear_output, {
    prediction_log("")
    output$plot_stage2 <- renderPlot({ NULL })
    for (name in names(used_models)) {
      used_models[[name]] <- NULL
    }
  })
}

shinyApp(ui = ui, server = server)

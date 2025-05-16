pisa_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values to store data
    pisa_data <- reactiveVal(NULL)
    analysis_results <- reactiveVal(NULL)
    
    # Function to read CSV with proper separator
    read_pisa_data <- function(file) {
      if (is.null(file)) return(NULL)
      data <- read.csv(file$datapath, sep = ";", stringsAsFactors = FALSE)
      # Convert comma-separated numbers to proper numeric
      data$control_count <- as.numeric(gsub(",", ".", data$control_count))
      data$treatment_count <- as.numeric(gsub(",", ".", data$treatment_count))
      return(data)
    }
    
    # Read and process data when files are uploaded
    observe({
      req(input$pisa_file)
      pisa_data(read_pisa_data(input$pisa_file))
    })
    
    # Data preview
    output$data_preview <- DT::renderDataTable({
      req(pisa_data())
      DT::datatable(pisa_data(),
                   options = list(scrollX = TRUE, pageLength = 10),
                   rownames = FALSE)
    })
    
    # Analyze button observer
    observeEvent(input$analyze, {
      req(pisa_data())
      data <- pisa_data()
      
      if (input$analysis_type == "qc") {
        # Quality control analysis
        results <- list(
          counts_summary = summary(data$control_count),
          treatment_summary = summary(data$treatment_count)
        )
        analysis_results(results)
        
      } else if (input$analysis_type == "diff") {
        # Differential analysis
        data$log2FC <- log2((data$treatment_count + 1) / (data$control_count + 1))
        data$significant <- abs(data$log2FC) > input$fc_threshold
        analysis_results(data)
        
      } else if (input$analysis_type == "gsea") {
        # Gene set enrichment analysis
        # This will be implemented based on the specific requirements
      }
    })
    
    # Analysis plot
    output$analysis_plot <- renderPlotly({
      req(analysis_results(), input$analysis_type)
      
      if (input$analysis_type == "qc") {
        # QC plot
        data <- pisa_data()
        plot_ly() %>%
          add_boxplot(y = ~control_count, name = "Control", data = data) %>%
          add_boxplot(y = ~treatment_count, name = "Treatment", data = data) %>%
          layout(title = "Count Distribution",
                 yaxis = list(title = "Counts"),
                 showlegend = TRUE)
                 
      } else if (input$analysis_type == "diff") {
        # Differential analysis plot
        data <- analysis_results()
        plot_ly(data = data,
                x = ~log2FC,
                y = ~control_count,
                type = "scatter",
                mode = "markers",
                color = ~significant,
                text = ~Gene) %>%
          layout(title = "PISA Analysis Results",
                 xaxis = list(title = "Log2 Fold Change"),
                 yaxis = list(title = "Base Mean"))
      }
    })
    
    # Analysis table
    output$analysis_table <- DT::renderDataTable({
      req(analysis_results(), input$analysis_type)
      
      if (input$analysis_type == "diff") {
        data <- analysis_results()
        DT::datatable(data[, c("Gene", "sgrna", "control_count", "treatment_count", "log2FC", "significant")],
                     options = list(scrollX = TRUE, pageLength = 10),
                     rownames = FALSE)
      }
    })
  })
} 
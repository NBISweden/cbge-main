de_analysis_server <- function(input, output, session) {
  # Reactive values to store data and results
  crispr_data <- reactiveVal(NULL)
  pisa_data <- reactiveVal(NULL)
  de_results <- reactiveVal(NULL)
  
  # Function to read and validate CSV file
  read_csv_safe <- function(file) {
    if (is.null(file)) return(NULL)
    tryCatch({
      # Read the first few lines to understand the structure
      first_lines <- readLines(file$datapath, n = 5)
      
      # Count the number of columns in the first line
      n_cols <- length(strsplit(first_lines[1], ",")[[1]])
      
      # Read the file with appropriate settings
      data <- read.csv(file$datapath,
                      check.names = FALSE,
                      header = TRUE,
                      stringsAsFactors = FALSE,
                      fill = TRUE)  # fill = TRUE helps with uneven rows
      
      # If we have more columns than names, add generic names
      if (ncol(data) > length(names(data))) {
        missing_cols <- ncol(data) - length(names(data))
        new_names <- paste0("V", seq_along(1:missing_cols))
        names(data)[(length(names(data)) + 1):ncol(data)] <- new_names
      }
      
      # Ensure we have at least 4 columns
      if (ncol(data) < 4) {
        showNotification("File must have at least 4 columns", type = "error")
        return(NULL)
      }
      
      # Rename first 4 columns if needed
      if (any(names(data)[1:4] == "")) {
        names(data)[1:4] <- c("Gene", "Description", "Accession", "Symbol")
      }
      
      return(data)
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      return(NULL)
    })
  }
  
  # Observe CRISPR file uploads
  observe({
    req(input$crispr_wt_file, input$crispr_ko_file)
    
    wt_data <- read_csv_safe(input$crispr_wt_file)
    ko_data <- read_csv_safe(input$crispr_ko_file)
    
    if (!is.null(wt_data) && !is.null(ko_data)) {
      # Check if files have same structure
      if (!all(names(wt_data)[1:4] == names(ko_data)[1:4])) {
        showNotification("CRISPR files have different column structures", type = "error")
        return()
      }
      crispr_data(list(wt = wt_data, ko = ko_data))
      showNotification("CRISPR data loaded successfully", type = "message")
    }
  })
  
  # Observe PISA file upload
  observe({
    req(input$pisa_file)
    data <- read_csv_safe(input$pisa_file)
    if (!is.null(data)) {
      pisa_data(data)
      showNotification("PISA data loaded successfully", type = "message")
    }
  })
  
  # Display data preview
  output$data_preview <- renderDT({
    if (input$dataset == "crispr") {
      data <- crispr_data()
      if (is.null(data)) return(NULL)
      preview_data <- data[[input$crispr_control]]
    } else {
      data <- pisa_data()
      if (is.null(data)) return(NULL)
      preview_data <- data
    }
    
    # Show only first 10 rows and first 10 columns
    preview_data <- head(preview_data[, 1:min(10, ncol(preview_data))], 10)
    datatable(preview_data,
              options = list(scrollX = TRUE))
  })
  
  # Function to perform differential expression analysis
  perform_de_analysis <- function() {
    req(input$dataset)
    
    tryCatch({
      if (input$dataset == "crispr") {
        data <- crispr_data()
        if (is.null(data)) {
          showNotification("Please upload CRISPR data first", type = "error")
          return()
        }
        
        control <- data[[input$crispr_control]]
        treatment <- data[[input$crispr_treatment]]
        
        # Basic data validation
        if (nrow(control) == 0 || nrow(treatment) == 0) {
          showNotification("No data available for selected groups", type = "error")
          return()
        }
        
        # Perform DE analysis for CRISPR data
        # This is a placeholder - implement actual DE analysis
        results <- data.frame(
          Gene = control$Gene,
          Description = control$Description,
          Accession = control$Accession,
          Symbol = control$Symbol,
          logFC = rnorm(nrow(control)),
          PValue = runif(nrow(control)),
          FDR = runif(nrow(control))
        )
        
      } else if (input$dataset == "pisa") {
        data <- pisa_data()
        if (is.null(data)) {
          showNotification("Please upload PISA data first", type = "error")
          return()
        }
        
        # Perform DE analysis for PISA data
        # This is a placeholder - implement actual DE analysis
        results <- data.frame(
          Gene = data$Gene,
          Description = data$Description,
          Accession = data$Accession,
          Symbol = data$Symbol,
          logFC = rnorm(nrow(data)),
          PValue = runif(nrow(data)),
          FDR = runif(nrow(data))
        )
      }
      
      de_results(results)
      showNotification("Analysis completed successfully", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in analysis:", e$message), type = "error")
    })
  }
  
  # Observe run button click
  observeEvent(input$run_de, {
    withProgress(message = 'Performing analysis...', value = 0, {
      perform_de_analysis()
      incProgress(1)
    })
  })
  
  # Generate volcano plot
  output$volcano_plot <- renderPlotly({
    req(de_results())
    results <- de_results()
    
    # Create volcano plot
    plot_ly(data = results,
            x = ~logFC,
            y = ~-log10(PValue),
            text = ~Gene,
            type = 'scatter',
            mode = 'markers',
            marker = list(
              color = ifelse(abs(results$logFC) > input$logfc_cutoff & 
                            results$PValue < input$pval_cutoff,
                           'red', 'gray')
            )) %>%
      layout(title = "Volcano Plot",
             xaxis = list(title = "Log2 Fold Change"),
             yaxis = list(title = "-log10(P-value)"))
  })
  
  # Generate MA plot
  output$ma_plot <- renderPlotly({
    req(de_results())
    results <- de_results()
    
    # Create MA plot
    plot_ly(data = results,
            x = ~(logFC + 1),
            y = ~logFC,
            text = ~Gene,
            type = 'scatter',
            mode = 'markers',
            marker = list(
              color = ifelse(abs(results$logFC) > input$logfc_cutoff & 
                            results$PValue < input$pval_cutoff,
                           'red', 'gray')
            )) %>%
      layout(title = "MA Plot",
             xaxis = list(title = "Average Expression"),
             yaxis = list(title = "Log2 Fold Change"))
  })
  
  # Display results table
  output$de_results <- renderDT({
    req(de_results())
    datatable(de_results(),
              options = list(scrollX = TRUE))
  })
  
  # Generate GSEA plot
  output$gsea_plot <- renderPlotly({
    req(de_results())
    # This is a placeholder - implement actual GSEA analysis
    plot_ly(x = 1:10, y = rnorm(10), type = 'scatter', mode = 'lines') %>%
      layout(title = "Gene Set Enrichment Analysis")
  })
  
  # Download handler for results
  output$download_results <- downloadHandler(
    filename = function() {
      paste("de_results_", input$dataset, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(de_results(), file, row.names = FALSE)
    }
  )
} 
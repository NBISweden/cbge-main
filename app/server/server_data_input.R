data_input_server <- function(input, output, session, merged_data, var_key_merged) {
  # Helper function to read files with different delimiters
  read_file_with_delimiters <- function(file_path) {
    # Try tab first, then semicolon, then comma
    tryCatch({
      read_delim(file_path, delim = "\t", col_types = cols(), show_col_types = FALSE)
    }, error = function(e1) {
      tryCatch({
        read_delim(file_path, delim = ";", col_types = cols(), show_col_types = FALSE)
      }, error = function(e2) {
        read_delim(file_path, delim = ",", col_types = cols(), show_col_types = FALSE)
      })
    })
  }

  observeEvent(input$merge_data, {
    withProgress(message = 'Merging data...', value = 0, {
      req(input$crispr_file, input$meta_file)
      
      tryCatch({
        # Read files using the helper function
        crispr_data <- read_file_with_delimiters(input$crispr_file$datapath)
        pisa_data <- read_file_with_delimiters(input$meta_file$datapath) %>%
        distinct(SUBJID, .keep_all = TRUE)
      
      if("SUBJID" %in% colnames(pisa_data)) {
        pisa_data$SUBJID <- gsub("\n", "", pisa_data$SUBJID)
      }
      
      if (!is.null(input$pisa_file)) {
          key_data <- read_file_with_delimiters(input$pisa_file$datapath) %>%
          distinct(SampleID, .keep_all = TRUE)
        key_data$SampleID <- as.character(key_data$SampleID)
        
        merged <- crispr_data %>%
          left_join(key_data, by = "SampleID") %>%
          left_join(pisa_data, by = "SUBJID")
      } else {
        merged <- crispr_data %>%
          mutate(SUBJID = SampleID) %>%
          left_join(pisa_data, by = "SUBJID")
      }
      
      merged_data(merged)
      
      updateSelectInput(session, "pca_var", choices = colnames(merged))
      updateSelectInput(session, "ttest_var", choices = colnames(merged))
      updateSelectInput(session, "anova_var", choices = colnames(merged))
      updateSelectInput(session, "volcano_var", choices = colnames(merged))
      updateSelectInput(session, "violin_group", choices = colnames(merged))
      updateSelectInput(session, "violin_protein", choices = unique(merged$Assay))
      updateSelectInput(session, "normality_protein", choices = unique(merged$Assay))
        
        showNotification("Data merged successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error merging data:", e$message), type = "error")
      })

      incProgress(1)
    })
  })

  observeEvent(input$merge_var_key, {
    withProgress(message = 'Merging var and key data...', value = 0, {
      req(input$meta_file)
      
      tryCatch({
        pisa_data <- read_file_with_delimiters(input$meta_file$datapath) %>%
        distinct(SUBJID, .keep_all = TRUE)
      
      if("SUBJID" %in% colnames(pisa_data)) {
        pisa_data$SUBJID <- gsub("\n", "", pisa_data$SUBJID)
      }
      
      if (!is.null(input$pisa_file)) {
          key_data <- read_file_with_delimiters(input$pisa_file$datapath) %>%
          distinct(SampleID, .keep_all = TRUE)
        key_data$SampleID <- as.character(key_data$SampleID)
        
        var_key_merged(pisa_data %>% left_join(key_data, by = "SUBJID"))
          showNotification("Data merged successfully!", type = "message")
      } else {
        showNotification("Key file is not provided. Cannot merge.", type = "warning")
        var_key_merged(pisa_data)
      }
      }, error = function(e) {
        showNotification(paste("Error merging data:", e$message), type = "error")
      })
      
      incProgress(1)
    })
  })

  observeEvent(input$crispr_file, {
    req(input$crispr_file)
    tryCatch({
      crispr_data <- read_file_with_delimiters(input$crispr_file$datapath)
    output$crispr_head <- renderDT({
      datatable(head(crispr_data), options = list(scrollX = TRUE))
      })
    }, error = function(e) {
      showNotification(paste("Error reading CRISPR file:", e$message), type = "error")
    })
  })

  observeEvent(input$pisa_file, {
    req(input$pisa_file)
    tryCatch({
      pisa_data <- read_file_with_delimiters(input$pisa_file$datapath)
    output$pisa_head <- renderDT({
      datatable(head(pisa_data), options = list(scrollX = TRUE))
      })
    }, error = function(e) {
      showNotification(paste("Error reading PISA file:", e$message), type = "error")
    })
  })

  observeEvent(input$meta_file, {
    req(input$meta_file)
    tryCatch({
      meta_data <- read_file_with_delimiters(input$meta_file$datapath)
    output$meta_head <- renderDT({
      datatable(head(meta_data), options = list(scrollX = TRUE))
      })
    }, error = function(e) {
      showNotification(paste("Error reading metadata file:", e$message), type = "error")
    })
  })
}
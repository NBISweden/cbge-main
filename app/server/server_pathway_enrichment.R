library(shiny)
library(DT)
library(clusterProfiler)
library(enrichplot)
library(org.Hs.eg.db)

pathway_enrichment_server <- function(input, output, session, merged_data, ttest_results, shared_enrichment_results) {
  
  print("Pathway Enrichment Server: Function started")
  
  observe({
    print("Pathway Enrichment: Checking merged_data and ttest_results")
    print(paste("merged_data is null:", is.null(merged_data())))
    print(paste("ttest_results is null:", is.null(ttest_results())))
  })
  
  output$pathway_enrichment_content <- renderUI({
    req(ttest_results())
    print("Pathway Enrichment: Rendering UI")
    tagList(
      selectInput("enrichment_method", "Enrichment Method:", 
                  choices = c("GSEA", "ORA")),
      selectInput("ontology", "Ontology:",
                  choices = c("GO", "KEGG", "Reactome")),
      numericInput("pvalue_cutoff", "P-value Cutoff:",
                   value = 0.05, min = 0, max = 1, step = 0.01),
      numericInput("estimate_cutoff", "Log2 Fold Change Cutoff:",
                   value = 0, min = -Inf, max = Inf, step = 0.1),
      actionButton("run_enrichment", "Run Enrichment Analysis"),
      uiOutput("enrichment_spinner"),
      DTOutput("enrichment_results"),
      plotOutput("enrichment_plot"),
      downloadButton("download_enrichplot", "Download Plot", class = "btn-success")
    )
  })

  observeEvent(input$run_enrichment, {
    print("Pathway Enrichment: Run enrichment button clicked")
    req(merged_data(), ttest_results())
    
    withProgress(message = 'Running pathway enrichment analysis...', value = 0, {
      tryCatch({
        crispr_data <- merged_data()
        test_results <- ttest_results()
        
        # Prepare gene list for enrichment analysis
        gene_list <- test_results %>%
          filter(abs(log2FoldChange) >= input$estimate_cutoff,
                 padj <= input$pvalue_cutoff) %>%
          pull(log2FoldChange)
        names(gene_list) <- test_results$Gene
        
        # Sort by absolute fold change
        gene_list <- sort(gene_list, decreasing = TRUE)
        
        if (input$enrichment_method == "GSEA") {
          if (input$ontology == "GO") {
            results <- gseGO(
              geneList = gene_list,
              OrgDb = org.Hs.eg.db,
              ont = "BP",
              pvalueCutoff = input$pvalue_cutoff,
              verbose = FALSE
            )
          } else if (input$ontology == "KEGG") {
            results <- gseKEGG(
              geneList = gene_list,
              organism = "hsa",
              pvalueCutoff = input$pvalue_cutoff,
              verbose = FALSE
            )
          } else if (input$ontology == "Reactome") {
            results <- gsePathway(
              geneList = gene_list,
              pvalueCutoff = input$pvalue_cutoff,
              verbose = FALSE
            )
          }
        } else {  # ORA
          if (input$ontology == "GO") {
            results <- enrichGO(
              gene = names(gene_list),
              OrgDb = org.Hs.eg.db,
              ont = "BP",
              pvalueCutoff = input$pvalue_cutoff,
              readable = TRUE
            )
          } else if (input$ontology == "KEGG") {
            results <- enrichKEGG(
              gene = names(gene_list),
              organism = "hsa",
              pvalueCutoff = input$pvalue_cutoff
            )
          } else if (input$ontology == "Reactome") {
            results <- enrichPathway(
              gene = names(gene_list),
              pvalueCutoff = input$pvalue_cutoff,
              readable = TRUE
            )
          }
        }
        
        shared_enrichment_results(results)
        
        incProgress(1)
        print("Pathway Enrichment: Analysis completed successfully")
      }, error = function(e) {
        print(paste("Pathway Enrichment: Full error message:", e$message))
        showNotification(paste("Error in pathway enrichment analysis:", e$message), type = "error")
      })
    })
  })
  
  output$enrichment_results <- renderDT({
    req(shared_enrichment_results())
    if (is.null(shared_enrichment_results())) {
      return(datatable(data.frame(Message = "No results available. Please run the analysis.")))
    }
    
    results_df <- as.data.frame(shared_enrichment_results())
    datatable(results_df, 
              extensions = 'Buttons',
              options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autowidth = TRUE,
                ordering = TRUE,
                dom = 'Bflrtip',
                lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100','All')),
                scrollX = TRUE,
                buttons = list(
                  list(extend = "excel", text = "Download current page", 
                       filename = "Pathway Enrichment Analysis",
                       exportOptions = list(
                         modifier = list(page = "current")
                       ))))
    )
  })
  
  output$enrichment_plot <- renderPlot({
    req(shared_enrichment_results())
    if (input$enrichment_method == "GSEA") {
      enrichplot::gseaplot2(shared_enrichment_results(), 
                           geneSetID = 1:min(5, length(shared_enrichment_results()@result$ID)),
                           pvalue_table = TRUE)
    } else {  # ORA
      enrichplot::dotplot(shared_enrichment_results(), 
                         showCategory = 20,
                         font.size = 8)
    }
  })
  
  output$download_enrichment <- downloadHandler(
    filename = function() {
      paste("enrichment_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(as.data.frame(shared_enrichment_results()), file, row.names = FALSE)
    }
  )

  output$download_enrichplot <- downloadHandler(
    filename = function() { paste("pathway_enrichment_plot_", Sys.Date(), ".png", sep="") },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 10, height = 8)
    }
  )
}
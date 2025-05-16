pisa_analysis_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabsetPanel(
      type = "pills",
      tabPanel("Quality Control",
        div(class = "row g-4",
          div(class = "col-md-4",
            div(class = "well",
              h4("Data Upload", class = "mb-4"),
              fileInput(ns("pisa_file"), "Upload PISA Data (CSV)",
                       accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                       width = "100%"),
              div(class = "d-grid gap-2",
                actionButton(ns("analyze_qc"), "Analyze QC", 
                           class = "btn-primary btn-lg w-100")
              )
            )
          ),
          div(class = "col-md-8",
            div(class = "well",
              h4("Quality Control Results", class = "mb-4"),
              div(class = "mb-4",
                DT::dataTableOutput(ns("qc_data_preview"))
              ),
              div(
                plotlyOutput(ns("qc_analysis_plot"))
              )
            )
          )
        )
      ),
      tabPanel("Differential Analysis",
        div(class = "row g-4",
          div(class = "col-md-4",
            div(class = "well",
              h4("Analysis Parameters", class = "mb-4"),
              div(class = "mb-3",
                numericInput(ns("fc_threshold"), "Log2 Fold Change Threshold", 
                           value = 1, width = "100%")
              ),
              div(class = "mb-4",
                numericInput(ns("pval_threshold"), "P-value Threshold", 
                           value = 0.05, width = "100%")
              ),
              div(class = "d-grid gap-2",
                actionButton(ns("analyze_diff"), "Run Analysis", 
                           class = "btn-primary btn-lg w-100")
              )
            )
          ),
          div(class = "col-md-8",
            div(class = "well",
              h4("Differential Analysis Results", class = "mb-4"),
              div(class = "mb-4",
                DT::dataTableOutput(ns("diff_data_preview"))
              ),
              div(
                plotlyOutput(ns("diff_analysis_plot"))
              )
            )
          )
        )
      ),
      tabPanel("Gene Set Enrichment",
        div(class = "row g-4",
          div(class = "col-md-4",
            div(class = "well",
              h4("GSEA Parameters", class = "mb-4"),
              div(class = "d-grid gap-2",
                actionButton(ns("analyze_gsea"), "Run GSEA", 
                           class = "btn-primary btn-lg w-100")
              )
            )
          ),
          div(class = "col-md-8",
            div(class = "well",
              h4("GSEA Results", class = "mb-4"),
              div(class = "mb-4",
                plotOutput(ns("gsea_plot"))
              ),
              div(
                DT::dataTableOutput(ns("gsea_table"))
              )
            )
          )
        )
      )
    )
  )
} 
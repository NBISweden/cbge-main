de_analysis_ui <- function() {
  fluidPage(
    titlePanel("Differential Expression Analysis"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Data Upload"),
        selectInput("dataset", "Select Dataset",
                   choices = c("CRISPR" = "crispr",
                             "PISA" = "pisa")),
        
        conditionalPanel(
          condition = "input.dataset == 'crispr'",
          h5("Upload CRISPR Data"),
          fileInput("crispr_wt_file", "Upload WT Data (CSV)",
                   accept = c(".csv")),
          fileInput("crispr_ko_file", "Upload KO Data (CSV)",
                   accept = c(".csv")),
          selectInput("crispr_control", "Control Group",
                     choices = c("WT" = "wt",
                               "KO" = "ko")),
          selectInput("crispr_treatment", "Treatment Group",
                     choices = c("WT" = "wt",
                               "KO" = "ko"))
        ),
        
        conditionalPanel(
          condition = "input.dataset == 'pisa'",
          h5("Upload PISA Data"),
          fileInput("pisa_file", "Upload PISA Data (CSV)",
                   accept = c(".csv")),
          selectInput("pisa_control", "Control Group",
                     choices = c("Control" = "control",
                               "Treatment" = "treatment")),
          selectInput("pisa_treatment", "Treatment Group",
                     choices = c("Control" = "control",
                               "Treatment" = "treatment"))
        ),
        
        h4("Analysis Parameters"),
        numericInput("pval_cutoff", "P-value Cutoff", 
                    value = 0.05, min = 0, max = 1, step = 0.01),
        numericInput("logfc_cutoff", "Log2 Fold Change Cutoff", 
                    value = 1, min = 0, step = 0.1),
        
        actionButton("run_de", "Run Analysis", class = "btn-primary"),
        br(),
        br(),
        downloadButton("download_results", "Download Results")
      ),
      
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Data Preview",
                   h4("Uploaded Data Preview"),
                   DTOutput("data_preview")
          ),
          tabPanel("Volcano Plot",
                   plotlyOutput("volcano_plot")
          ),
          tabPanel("MA Plot",
                   plotlyOutput("ma_plot")
          ),
          tabPanel("Results Table",
                   DTOutput("de_results")
          ),
          tabPanel("Gene Set Enrichment",
                   plotlyOutput("gsea_plot")
          )
        )
      )
    )
  )
} 
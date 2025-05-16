library(shiny)
library(bslib)

# Source UI components
source("ui/ui_data_input.R")
source("ui/ui_data_preview.R")
# source("ui/ui_descriptive_stats.R")
# source("ui/ui_normality_test.R")
# source("ui/ui_pca_plot.R")
# source("ui/ui_ttest.R")
# source("ui/ui_violin_plot.R")
# source("ui/ui_volcano_plot.R")
# source("ui/ui_anova.R")
# source("ui/ui_outlier_detection.R")
# source("ui/ui_enhanced_visualization.R")
# source("ui/ui_normalization.R")
# source("ui/ui_pathway_enrichment.R")
# source("ui/ui_lod_integration.R")
# source("ui/ui_heatmap.R")
# source("ui/ui_umap.R")
# source("ui/ui_bridge_sample.R")
# source("ui/ui_wilcox.R")
# source("ui/ui_anova_posthoc.R")
# source("ui/ui_lme.R")
# source("ui/ui_lme_posthoc.R")
# source("ui/ui_lme_stats.R")
# source("ui/ui_boxplot.R")
# source("ui/ui_distribution_plot.R")
# source("ui/ui_lme_plot.R")
# source("ui/ui_pathway_heatmap.R")
# source("ui/ui_qc_plot.R")

single_ui <- function() {
  fluidPage(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    
    # Header
    div(
      class = "header mb-4",
      h1("Multi-Omics Analysis Platform", class = "text-center")
    ),

    # Main navigation tabs
    navset_tab(
      # CRISPR Analysis Tab
      nav_panel(
        "CRISPR Analysis",
        navset_pill(
          nav_panel(
            "Data Input & QC",
            fileInput("crispr_file", "Upload CRISPR Data"),
            DTOutput("crispr_preview"),
            h4("Quality Control"),
            plotOutput("crispr_qc_plot"),
            downloadButton("download_crispr_qc", "Download QC Report")
          ),
          nav_panel(
            "Exploratory Analysis",
            selectInput("crispr_plot_type", "Select Plot Type",
                      choices = c("PCA", "UMAP", "Heatmap", "Sample Clustering")),
            plotOutput("crispr_exploratory_plot"),
            downloadButton("download_crispr_plot", "Download Plot")
      ),
          nav_panel(
            "Differential Analysis",
            selectInput("crispr_de_method", "Select Method",
                      choices = c("DESeq2", "EdgeR", "Limma")),
            plotOutput("crispr_de_plot"),
            DTOutput("crispr_de_table"),
            downloadButton("download_crispr_de", "Download Results")
          ),
          nav_panel(
            "Pathway Analysis",
            selectInput("crispr_pathway_db", "Select Database",
                      choices = c("KEGG", "GO", "Reactome")),
            plotOutput("crispr_pathway_plot"),
            DTOutput("crispr_pathway_table"),
            downloadButton("download_crispr_pathway", "Download Results")
          )
        )
      ),
      
      # PISA Analysis Tab
      nav_panel(
        "PISA Analysis",
        navset_pill(
          nav_panel(
            "Data Input & QC",
            fileInput("pisa_file", "Upload PISA Data"),
            DTOutput("pisa_preview"),
            h4("Quality Control"),
            plotOutput("pisa_qc_plot"),
            downloadButton("download_pisa_qc", "Download QC Report")
          ),
          nav_panel(
            "Exploratory Analysis",
            selectInput("pisa_plot_type", "Select Plot Type",
                      choices = c("PCA", "UMAP", "Heatmap", "Sample Clustering")),
            plotOutput("pisa_exploratory_plot"),
            downloadButton("download_pisa_plot", "Download Plot")
          ),
          nav_panel(
            "Differential Analysis",
            selectInput("pisa_de_method", "Select Method",
                      choices = c("Limma", "t-test", "Wilcoxon")),
            plotOutput("pisa_de_plot"),
            DTOutput("pisa_de_table"),
            downloadButton("download_pisa_de", "Download Results")
      ),
          nav_panel(
            "Pathway Analysis",
            selectInput("pisa_pathway_db", "Select Database",
                      choices = c("KEGG", "GO", "Reactome")),
            plotOutput("pisa_pathway_plot"),
            DTOutput("pisa_pathway_table"),
            downloadButton("download_pisa_pathway", "Download Results")
          )
        )
      ),
      
      # Integration Analysis Tab
      nav_panel(
        "Multi-Omics Integration",
        navset_pill(
          nav_panel(
            "Result Comparison",
            h4("Compare Differential Analysis Results"),
            plotOutput("comparison_plot"),
            DTOutput("comparison_table"),
            downloadButton("download_comparison", "Download Results")
          ),
          nav_panel(
            "Cross-Platform Correlation",
            h4("Correlation Analysis"),
            selectInput("correlation_method", "Select Method",
                      choices = c("Pearson", "Spearman")),
            plotOutput("correlation_plot"),
            downloadButton("download_correlation", "Download Results")
          ),
          nav_panel(
            "Pathway Integration",
            h4("Integrated Pathway Analysis"),
            selectInput("integrated_pathway_db", "Select Database",
                      choices = c("KEGG", "GO", "Reactome")),
            plotOutput("integrated_pathway_plot"),
            DTOutput("integrated_pathway_table"),
            downloadButton("download_integrated_pathway", "Download Results")
          ),
          nav_panel(
            "Network Analysis",
            h4("Multi-Omics Network"),
            selectInput("network_type", "Select Network Type",
                      choices = c("Correlation Network", "Regulatory Network", "Pathway Network")),
            plotOutput("network_plot"),
            downloadButton("download_network", "Download Results")
          )
        )
      )
    ),
    
    # Footer
    tags$footer(
      class = "footer mt-auto py-3 bg-light",
      div(
        class = "container footer-content",
        div(
          class = "footer-section footer-left",
          paste0("©", format(Sys.Date(), "%Y"), ", Developed & maintained by Rasool Saghaleyni, NBIS, Scilifelab, Chalmers University of Technology")
        ),
        div(
          class = "footer-section",
          a("GitHub repo:", href = "https://github.com/NBISweden/nbis-cbge-tdp", target = "_blank")
        ),
        div(
          class = "footer-section footer-right",
          "Version 1.0.0"
        )
      )
    )
  )
}

ui <- single_ui()
server <- function(input, output, session) {}

shinyApp(ui, server)
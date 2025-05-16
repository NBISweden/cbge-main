start_page_ui <- function() {
  div(
    class = "start-page-content",
    h2("Welcome to the Multi-Omics Analysis Platform"),
    br(),
    div(
      class = "radio-buttons-center",
      radioButtons(
        "analysis_type",
        "Select Analysis Type:",
        choices = c(
          "CRISPR Analysis" = "crispr",
          "PISA Analysis" = "pisa",
          "Multi-Omics Integration" = "integration"
        ),
        selected = character(0)
      )
    ),
    conditionalPanel(
      condition = "input.analysis_type == 'crispr' || input.analysis_type == 'pisa'",
      selectInput(
        "analysis_method",
        "Select Analysis Method:",
        choices = c(
          "Quality Control" = "qc",
          "Exploratory Analysis" = "exploratory",
          "Differential Analysis" = "differential",
          "Pathway Analysis" = "pathway"
        )
      )
    ),
    conditionalPanel(
      condition = "input.analysis_type == 'integration'",
      selectInput(
        "integration_method",
        "Select Integration Method:",
        choices = c(
          "Result Comparison" = "comparison",
          "Cross-Platform Correlation" = "correlation",
          "Pathway Integration" = "pathway_integration",
          "Network Analysis" = "network"
        )
      )
    ),
    br(),
    actionButton("start_analysis", "Start Analysis", class = "btn-primary btn-lg")
  )
}

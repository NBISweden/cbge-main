library(shiny)
library(bslib)
library(tidyverse)
library(msigdbr)
library(plotly)
library(viridis)
library(writexl)
library(shinyjs)
library(DT)

options(shiny.maxRequestSize=500*1024^2)

# Source utility functions
source("utilities.R")

# Source UI modules
source("ui/ui_main.R")
source("ui/ui_startpage.R")
source("ui/ui_data_input.R")
source("ui/ui_data_preview.R")
source("ui/ui_de_analysis.R")
source("ui/ui_crispr_analysis.R")
source("ui/ui_pisa_analysis.R")
source("ui/ui_omics_integration.R")

# Source server modules
source("server/server_main.R")
source("server/server_startpage.R")
source("server/server_data_input.R")
source("server/server_data_preview.R")
source("server/server_de_analysis.R")
source("server/server_pathway_enrichment.R")
source("server/server_crispr_analysis.R")
source("server/server_pisa_analysis.R")

# Load required DT extensions
dtmod <- "
$(document).ready(function() {
  $.extend(true, $.fn.dataTable.defaults, {
    'searchHighlight': true
  });
});"

# Define the main UI
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0077be",
    "navbar-bg" = "#ffffff"
  ),
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .nav-tabs {
        margin-bottom: 20px;
        border-bottom: 2px solid #dee2e6;
      }
      .nav-tabs .nav-link {
        margin-bottom: -2px;
        border: none;
        color: #495057;
        font-weight: 500;
      }
      .nav-tabs .nav-link.active {
        color: #0077be;
        border-bottom: 2px solid #0077be;
      }
      .nav-tabs .nav-link:hover {
        border-color: transparent;
        color: #0077be;
      }
      .well {
        background-color: #f8f9fa;
        border-radius: 8px;
        padding: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .btn-primary {
        background-color: #0077be;
        border-color: #0077be;
        padding: 8px 16px;
        border-radius: 4px;
        font-weight: 500;
      }
      .btn-primary:hover {
        background-color: #005c91;
        border-color: #005c91;
      }
      .form-control {
        border-radius: 4px;
        border: 1px solid #ced4da;
      }
      .form-control:focus {
        border-color: #0077be;
        box-shadow: 0 0 0 0.2rem rgba(0,119,190,0.25);
      }
      .content-wrapper {
        padding: 20px;
        background-color: #ffffff;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .tab-content {
        padding-top: 20px;
      }
      .plotly {
        border-radius: 4px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .dataTables_wrapper {
        padding: 20px 0;
      }
      .dataTables_filter {
        margin-bottom: 10px;
      }
      .dt-buttons {
        margin-bottom: 10px;
      }
      .dt-button {
        background-color: #f8f9fa !important;
        border: 1px solid #dee2e6 !important;
        border-radius: 4px !important;
        color: #495057 !important;
        padding: 4px 8px !important;
      }
      .dt-button:hover {
        background-color: #e9ecef !important;
        color: #0077be !important;
      }
      .dataTable thead th {
        background-color: #f8f9fa;
        border-bottom: 2px solid #dee2e6;
      }
      .dataTable tbody tr:hover {
        background-color: rgba(0,119,190,0.05) !important;
      }
      .dataTables_info {
        color: #6c757d;
      }
      .alert-secondary {
        background-color: #f8f9fa;
        border-color: #dee2e6;
        color: #495057;
      }
    "))
  ),
  titlePanel(
    div(class = "d-flex align-items-center",
      h2("CBGE Omics Data Analysis", class = "mb-0"),
      style = "border-bottom: 2px solid #dee2e6; padding-bottom: 15px; margin-bottom: 20px;"
    )
  ),
  
  tabsetPanel(
    id = "main_tabs",
    tabPanel("CRISPR Analysis",
      div(class = "content-wrapper",
        crispr_analysis_ui("crispr")
      )
    ),
    tabPanel("PISA Analysis",
      div(class = "content-wrapper",
        pisa_analysis_ui("pisa")
      )
    ),
    tabPanel("Omics Integration",
      div(class = "content-wrapper",
        omics_integration_ui("omics")
      )
    )
  ),
  verbatimTextOutput("project_vol_files")
)

# Define the main server
server <- function(input, output, session) {
  # Reactive values to store data
  crispr_data <- reactiveVal(NULL)
  pisa_data <- reactiveVal(NULL)
  integration_results <- reactiveVal(NULL)
  
  # Reactive value to store the current page and analysis state
  current_page <- reactiveVal("start")
  analysis_state <- reactiveValues(
    type = NULL,
    method = NULL,
    results = NULL
  )
  
  # Render the appropriate page
  output$page_content <- renderUI({
    print(paste("Rendering page:", current_page()))
    if (current_page() == "start") {
      start_page_ui()
    } else {
      switch(analysis_state$type,
        "crispr" = render_crispr_page(analysis_state$method),
        "pisa" = render_pisa_page(analysis_state$method),
        "integration" = render_integration_page(analysis_state$method)
      )
    }
  })
  
  # Render the background style
  output$background_style <- renderUI({
    if (current_page() == "start") {
      tags$style(HTML("
        body {
          background-image: url('ppin.gif');
          background-size: cover;
          background-repeat: no-repeat;
          background-attachment: fixed;
          height: 100vh;
          display: flex;
          justify-content: center;
          align-items: center;
        }
        .start-page-content {
          background-color: rgba(255, 255, 255, 0.8);
          padding: 30px;
          border-radius: 10px;
          text-align: center;
        }
        .radio-buttons-center {
          display: flex;
          flex-direction: column;
          align-items: center;
        }
        .radio-buttons-center .shiny-options-group {
          width: auto;
          padding-left: 80px;
        }
        .radio-buttons-center .radio {
          text-align: left;
        }
        .radio-buttons-center .radio label {
          font-size: 18px;
          padding: 5px 0;
        }
      "))
    } else {
      tags$style(HTML("
        body {
          background-image: none;
          background-color: #ffffff;
        }
      "))
    }
  })
  
  # Start page server logic
  observeEvent(input$start_analysis, {
    print("Start analysis button clicked")
    analysis_state$type <- input$analysis_type
    
    if (input$analysis_type %in% c("crispr", "pisa")) {
      analysis_state$method <- input$analysis_method
    } else if (input$analysis_type == "integration") {
      analysis_state$method <- input$integration_method
    }
    
    current_page("analysis")
  })
  
  # Render appropriate analysis page based on type and method
  render_crispr_page <- function(method) {
    switch(method,
      "qc" = crispr_qc_ui(),
      "exploratory" = crispr_exploratory_ui(),
      "differential" = crispr_differential_ui(),
      "pathway" = crispr_pathway_ui()
    )
  }
  
  render_pisa_page <- function(method) {
    switch(method,
      "qc" = pisa_qc_ui(),
      "exploratory" = pisa_exploratory_ui(),
      "differential" = pisa_differential_ui(),
      "pathway" = pisa_pathway_ui()
    )
  }
  
  render_integration_page <- function(method) {
    switch(method,
      "comparison" = integration_comparison_ui(),
      "correlation" = integration_correlation_ui(),
      "pathway_integration" = integration_pathway_ui(),
      "network" = integration_network_ui()
    )
  }
  
  # Data upload handlers
  observeEvent(input$crispr_file, {
    req(input$crispr_file)
    tryCatch({
      data <- read_file_with_delimiters(input$crispr_file$datapath)
      crispr_data(data)
      showNotification("CRISPR data loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading CRISPR data:", e$message), type = "error")
    })
  })
  
  observeEvent(input$pisa_file, {
    req(input$pisa_file)
    tryCatch({
      data <- read_file_with_delimiters(input$pisa_file$datapath)
      pisa_data(data)
      showNotification("PISA data loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading PISA data:", e$message), type = "error")
    })
  })
  
  # Integration analysis handler
  observe({
    req(crispr_data(), pisa_data())
    if (analysis_state$type == "integration") {
      results <- switch(analysis_state$method,
        "comparison" = compare_results(crispr_data(), pisa_data()),
        "correlation" = compute_correlations(crispr_data(), pisa_data()),
        "pathway_integration" = integrate_pathways(crispr_data(), pisa_data()),
        "network" = build_network(crispr_data(), pisa_data())
      )
      integration_results(results)
    }
  })
  
  # Data preview handlers
  output$crispr_preview <- renderDT({
    req(input$crispr_file)
    tryCatch({
      data <- read_file_with_delimiters(input$crispr_file$datapath)
      crispr_data(data)
      datatable(head(data), options = list(scrollX = TRUE))
    }, error = function(e) {
      showNotification(paste("Error loading CRISPR data:", e$message), type = "error")
    })
  })
  
  output$pisa_preview <- renderDT({
    req(input$pisa_file)
    tryCatch({
      data <- read_file_with_delimiters(input$pisa_file$datapath)
      pisa_data(data)
      datatable(head(data), options = list(scrollX = TRUE))
    }, error = function(e) {
      showNotification(paste("Error loading PISA data:", e$message), type = "error")
    })
  })
  
  # CRISPR Analysis Outputs
  output$crispr_qc_plot <- renderPlot({
    req(crispr_data())
    # Add QC plot logic here
  })
  
  output$crispr_exploratory_plot <- renderPlot({
    req(crispr_data(), input$crispr_plot_type)
    # Add exploratory plot logic here
  })
  
  output$crispr_de_plot <- renderPlot({
    req(crispr_data(), input$crispr_de_method)
    # Add DE plot logic here
  })
  
  output$crispr_de_table <- renderDT({
    req(crispr_data(), input$crispr_de_method)
    # Add DE table logic here
  })
  
  output$crispr_pathway_plot <- renderPlot({
    req(crispr_data(), input$crispr_pathway_db)
    # Add pathway plot logic here
  })
  
  output$crispr_pathway_table <- renderDT({
    req(crispr_data(), input$crispr_pathway_db)
    # Add pathway table logic here
  })
  
  # PISA Analysis Outputs
  output$pisa_qc_plot <- renderPlot({
    req(pisa_data())
    # Add QC plot logic here
  })
  
  output$pisa_exploratory_plot <- renderPlot({
    req(pisa_data(), input$pisa_plot_type)
    # Add exploratory plot logic here
  })
  
  output$pisa_de_plot <- renderPlot({
    req(pisa_data(), input$pisa_de_method)
    # Add DE plot logic here
  })
  
  output$pisa_de_table <- renderDT({
    req(pisa_data(), input$pisa_de_method)
    # Add DE table logic here
  })
  
  output$pisa_pathway_plot <- renderPlot({
    req(pisa_data(), input$pisa_pathway_db)
    # Add pathway plot logic here
  })
  
  output$pisa_pathway_table <- renderDT({
    req(pisa_data(), input$pisa_pathway_db)
    # Add pathway table logic here
  })
  
  # Integration Analysis Outputs
  output$comparison_plot <- renderPlot({
    req(crispr_data(), pisa_data())
    # Add comparison plot logic here
  })
  
  output$comparison_table <- renderDT({
    req(crispr_data(), pisa_data())
    # Add comparison table logic here
  })
  
  output$correlation_plot <- renderPlot({
    req(crispr_data(), pisa_data(), input$correlation_method)
    # Add correlation plot logic here
  })
  
  output$integrated_pathway_plot <- renderPlot({
    req(crispr_data(), pisa_data(), input$integrated_pathway_db)
    # Add integrated pathway plot logic here
  })
  
  output$integrated_pathway_table <- renderDT({
    req(crispr_data(), pisa_data(), input$integrated_pathway_db)
    # Add integrated pathway table logic here
  })
  
  output$network_plot <- renderPlot({
    req(crispr_data(), pisa_data(), input$network_type)
    # Add network plot logic here
  })
  
  # Download handlers
  output$download_crispr_qc <- downloadHandler(
    filename = function() {
      paste("crispr_qc_report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Add QC report generation logic here
    }
  )
  
  output$download_pisa_qc <- downloadHandler(
    filename = function() {
      paste("pisa_qc_report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Add QC report generation logic here
    }
  )
  
  # Add more download handlers for other outputs...
  
  # Initialize CRISPR analysis module
  crispr_analysis_server("crispr")
  
  # Initialize PISA analysis module
  pisa_analysis_server("pisa")
  
  # Sample data download handlers
  output$download_crispr_sample <- downloadHandler(
    filename = function() {
      "sample_crispr_data.zip"
    },
    content = function(file) {
      file.copy("data/crispr/CRISPR-ko.csv", "ko.csv")
      file.copy("data/crispr/CRISPR-wt.csv", "wt.csv")
      zip(file, c("ko.csv", "wt.csv"))
      file.remove(c("ko.csv", "wt.csv"))
    }
  )
  
  output$download_pisa_sample <- downloadHandler(
    filename = function() {
      "sample_pisa_data.zip"
    },
    content = function(file) {
      # Placeholder for PISA sample data
    }
  )
  
  # Omics Integration logic placeholder
  observeEvent(input$integrate, {
    # Placeholder for integrating CRISPR and PISA data
  })
  
  output$project_vol_files <- renderPrint({
    list.files("/project-vol", recursive = TRUE)
  })
}

shinyApp(ui, server)
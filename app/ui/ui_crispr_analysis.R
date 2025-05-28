crispr_analysis_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabsetPanel(
      type = "pills",
      tabPanel("Data",
        div(class = "row g-4",
          div(class = "col-md-4",
            div(class = "well",
              h4("Project Upload", class = "mb-4"),
              div(class = "mb-4",
                h5("Main Data Folder"),
                div(class = "alert alert-info",
                  tags$b("Note:"),
                  "Files should be in the project-vol folder:",
                  tags$ul(
                    tags$li("report.reads"),
                    tags$li("results/FastQC/samples")
                  )
                ),
                textInput(ns("project_path"), "Project Path", 
                         value = "/project-vol",
                         placeholder = "Enter path in project-vol folder"),
                actionButton(ns("browse_project"), "Browse Files", 
                           class = "btn-secondary"),
                div(id = ns("file_browser"),
                    style = "margin-top: 10px;",
                    verbatimTextOutput(ns("current_path")),
                    uiOutput(ns("file_list"))
                )
              ),
              div(class = "mb-4",
                h5("Configuration Folder"),
                textInput(ns("config_path"), "Config Path", 
                         value = "/project-vol",
                         placeholder = "Enter path in project-vol folder"),
                actionButton(ns("browse_config"), "Browse Files", 
                           class = "btn-secondary"),
                div(id = ns("config_browser"),
                    style = "margin-top: 10px;",
                    verbatimTextOutput(ns("config_current_path")),
                    uiOutput(ns("config_file_list"))
                )
              ),
              div(class = "mt-3",
                actionButton(ns("load_project"), "Load Project",
                           class = "btn-primary btn-lg w-100")
              )
            )
          ),
          div(class = "col-md-8",
            div(class = "well",
              tabsetPanel(
                type = "tabs",
                tabPanel("Project Structure",
                  h4("Project Structure", class = "mb-4"),
                  div(class = "alert alert-secondary mb-4",
                    textOutput(ns("project_summary"))
                  ),
                  div(class = "mb-4",
                    uiOutput(ns("project_tree")),
                    uiOutput(ns("config_tree"))
                  ),
                  tags$style(HTML('
                    .folder { cursor: pointer; font-weight: bold; }
                    .file { margin-left: 20px; }
                    .hidden { display: none; }
                    ul { margin-left: 20px; }
                  ')),
                  tags$script(HTML('
                    function closeAllFolders() {
                      $(".folder").each(function() {
                        var ul = $(this).siblings("ul");
                        if (ul.length) {
                          ul.addClass("hidden");
                        }
                      });
                    }
                    $(document).on("shiny:value", function() {
                      closeAllFolders();
                    });
                    $(document).on("click", ".folder", function() {
                      $(this).siblings("ul").toggleClass("hidden");
                      $(this).toggleClass("open");
                    });
                  '))
                ),
                tabPanel("Experimental Design",
                  h4("Experimental Design", class = "mb-4"),
                  DT::dataTableOutput(ns("design_table"))
                ),
                tabPanel("Library Summary",
                  h4("Library Summary", class = "mb-4"),
                  plotlyOutput(ns("library_summary_barplot"), height = "400px"),
                  DT::dataTableOutput(ns("library_summary_table"))
                ),
                tabPanel("CRISPR Report",
                  h4("CRISPR Report", class = "mb-4"),
                  uiOutput(ns("crispr_report_html"))
                )
              )
            )
          )
        )
      ),
      tabPanel("Quality Control",
        div(class = "row g-4",
          div(class = "col-md-4",
            div(class = "well",
              h4("Select a FastQC Sample", class = "mb-4"),
              selectInput(ns("qc_sample"), "Sample HTML Report", choices = NULL, width = "100%")
            )
          ),
          div(class = "col-md-8",
            div(class = "well",
              uiOutput(ns("qc_report"))
            )
          )
        )
      ),
      tabPanel("Statistical Analysis",
        div(class = "row g-4",
          div(class = "col-md-4",
            div(class = "well",
              h4("Analysis Parameters", class = "mb-4"),
              selectInput(ns("comparison"), "Select Comparison",
                        choices = NULL)
            )
          ),
          div(class = "col-md-8",
            div(class = "well",
              tabsetPanel(
                type = "tabs",
                tabPanel("Results Table",
                  div(class = "mb-4",
                    DTOutput(ns("diff_data_preview"))
                  )
                ),
                tabPanel("Volcano Plot",
                  div(class = "mb-4",
                    plotlyOutput(ns("volcano_plot"), height = "600px")
                  )
                ),
                tabPanel("Rank Plot",
                  div(class = "mb-4",
                    plotlyOutput(ns("rank_plot"), height = "600px")
                  )
                )
              )
            )
          )
        )
      ),
      tabPanel("Functional Analysis",
        div(class = "row g-4",
          div(class = "col-md-4",
            div(class = "well",
              h4("Analysis Parameters", class = "mb-4"),
              selectInput(ns("fa_comparison"), "Select Comparison", choices = NULL)
            )
          ),
          div(class = "col-md-8",
            div(class = "well",
              navlistPanel(
                widths = c(2, 10),
                tabPanel("Negative Selection",
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Gene Ontology",
                      h4("GO Enrichment Table"),
                      DT::dataTableOutput(ns("fa_neg_go_table")),
                      h4("Dotplot"),
                      plotlyOutput(ns("fa_neg_go_dotplot"), height = "350px"),
                      h4("Heatmap"),
                      plotlyOutput(ns("fa_neg_go_heatmap"), height = "350px"),
                      h4("Treeplot"),
                      plotlyOutput(ns("fa_neg_go_treeplot"), height = "350px"),
                      h4("GSEA Plots"),
                      uiOutput(ns("fa_neg_go_gsea_plots"))
                    ),
                    tabPanel("Reactome",
                      h4("Reactome Enrichment Table"),
                      DT::dataTableOutput(ns("fa_neg_reactome_table")),
                      h4("Dotplot"),
                      plotlyOutput(ns("fa_neg_reactome_dotplot"), height = "350px"),
                      h4("Heatmap"),
                      plotlyOutput(ns("fa_neg_reactome_heatmap"), height = "350px"),
                      h4("Treeplot"),
                      plotlyOutput(ns("fa_neg_reactome_treeplot"), height = "350px"),
                      h4("GSEA Plots"),
                      uiOutput(ns("fa_neg_reactome_gsea_plots"))
                    )
                  )
                ),
                tabPanel("Positive Selection",
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Gene Ontology",
                      h4("GO Enrichment Table"),
                      DT::dataTableOutput(ns("fa_pos_go_table")),
                      h4("Dotplot"),
                      plotlyOutput(ns("fa_pos_go_dotplot"), height = "350px"),
                      h4("Heatmap"),
                      plotlyOutput(ns("fa_pos_go_heatmap"), height = "350px"),
                      h4("Treeplot"),
                      plotlyOutput(ns("fa_pos_go_treeplot"), height = "350px"),
                      h4("GSEA Plots"),
                      uiOutput(ns("fa_pos_go_gsea_plots"))
                    ),
                    tabPanel("Reactome",
                      h4("Reactome Enrichment Table"),
                      DT::dataTableOutput(ns("fa_pos_reactome_table")),
                      h4("Dotplot"),
                      plotlyOutput(ns("fa_pos_reactome_dotplot"), height = "350px"),
                      h4("Heatmap"),
                      plotlyOutput(ns("fa_pos_reactome_heatmap"), height = "350px"),
                      h4("Treeplot"),
                      plotlyOutput(ns("fa_pos_reactome_treeplot"), height = "350px"),
                      h4("GSEA Plots"),
                      uiOutput(ns("fa_pos_reactome_gsea_plots"))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
} 
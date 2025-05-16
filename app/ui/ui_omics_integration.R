omics_integration_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabsetPanel(
      type = "pills",
      tabPanel("Integration Method 1",
        div(class = "row g-4",
          div(class = "col-md-4",
            div(class = "well",
              h4("Integration Parameters", class = "mb-4"),
              div(class = "d-grid gap-2",
                actionButton(ns("integrate_method1"), "Run Integration", 
                           class = "btn-primary btn-lg w-100")
              )
            )
          ),
          div(class = "col-md-8",
            div(class = "well",
              h4("Integration Results", class = "mb-4"),
              div(class = "mb-4",
                plotOutput(ns("integration_plot1"))
              ),
              div(
                DT::dataTableOutput(ns("integration_table1"))
              )
            )
          )
        )
      ),
      tabPanel("Integration Method 2",
        div(class = "row g-4",
          div(class = "col-md-4",
            div(class = "well",
              h4("Integration Parameters", class = "mb-4"),
              div(class = "d-grid gap-2",
                actionButton(ns("integrate_method2"), "Run Integration", 
                           class = "btn-primary btn-lg w-100")
              )
            )
          ),
          div(class = "col-md-8",
            div(class = "well",
              h4("Integration Results", class = "mb-4"),
              div(class = "mb-4",
                plotOutput(ns("integration_plot2"))
              ),
              div(
                DT::dataTableOutput(ns("integration_table2"))
              )
            )
          )
        )
      )
    )
  )
} 
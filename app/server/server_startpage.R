start_page_server <- function(input, output, session) {
  output$analysis_description <- renderText({
    if (input$analysis_type == "single") {
      "Single panel analysis is suitable for data from one Olink panel."
    } else {
      "Multi-panel analysis is suitable for data from multiple Olink panels. (Not yet implemented)"
    }
  })
}
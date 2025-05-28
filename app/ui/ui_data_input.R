data_input_ui <- function() {
  sidebar(
    title = "Data Input",
    selectInput("crispr_server_file", "Select CRISPR Data (CSV) from server", choices = NULL),
    selectInput("pisa_server_file", "Select PISA File (CSV, Optional) from server", choices = NULL),
    selectInput("meta_server_file", "Select Metadata File (CSV) from server", choices = NULL),
    actionButton("merge_data", "Merge Data", class = "btn-primary"),
    actionButton("merge_var_key", "Merge Var and Key Data", class = "btn-secondary"),
    HTML("<hr> <a href='Shinycbge.txt' target='_blank'> <i class='fa fa-download'> </i> HOW TO USE</a>"),
    br(),
    br(),
    HTML(
      "<strong>Cite:</strong> <a href='https://github.com/NBISweden/nbis-cbge-tdp'><img src='https://zenodo.org/badge/DOI/.../zenodo..svg' alt='DOI'></a>"
    ),
    br(),
    HTML(
      "<small><strong>Disclaimer:</strong> Please remember this Shiny app works based on the data from
      <a href='https://www.scilifelab.se/units/crispr-functional-genomics/' target='_blank'>Scilifelab Chemical biology & Genome Engineering (CBGE) facility.</a> 
      For designing assays please consult platform directly.</small>"
    )
  )
}
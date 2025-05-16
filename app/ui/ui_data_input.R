data_input_ui <- function() {
  sidebar(
    title = "Data Input",
    fileInput("crispr_file", "Upload CRISPR Data (CSV)", accept = c(".csv"), multiple = TRUE),
    fileInput("pisa_file", "Upload PISA File (CSV, Optional)", accept = c(".csv"), multiple = TRUE),
    fileInput("meta_file", "Upload Metadata File (CSV)", accept = c(".csv"), multiple = TRUE),
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
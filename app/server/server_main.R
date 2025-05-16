print("All server modules sourced")

server_main <- function(input, output, session, merged_data, var_key_merged, ttest_results) {
  print("Server function started")
  options(shiny.maxRequestSize=500*1024^2)
  
  # Reactive values to store data and results
  shared_enrichment_results <- reactiveVal()

  print("About to call data_input_server")
  safe_call(data_input_server, input, output, session, merged_data, var_key_merged)
  print("Finished calling data_input_server")
  
  # Call individual server modules
  safe_call(data_preview_server, input, output, session, merged_data, var_key_merged)
  safe_call(pathway_enrichment_server, input, output, session, merged_data, ttest_results, shared_enrichment_results)
}
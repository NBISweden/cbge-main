data_preview_server <- function(input, output, session, merged_data, var_key_merged) {
  output$data_preview <- renderDT({
    req(merged_data())
    summary_list <- list(
      "Merged Data" = list(
        "Rows" = nrow(merged_data()),
        "Columns" = ncol(merged_data())
      ),
      "Var-Key Merged Data" = if (!is.null(var_key_merged())) list(
        "Rows" = nrow(var_key_merged()),
        "Columns" = ncol(var_key_merged())
      ) else list(
        "Rows" = "N/A",
        "Columns" = "N/A"
      )
    )
    summary_df <- do.call(rbind, lapply(summary_list, as.data.frame))
    datatable(summary_df, extensions = c('Buttons'), options = list(
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE,
      dom = 't',
      scrollX = TRUE
    ))
  })

  output$download_full_data <- downloadHandler(
    filename = function() {
      paste("full_dataset_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(merged_data(), file, row.names = FALSE)
    }
  )

  output$download_var_key_data <- downloadHandler(
    filename = function() {
      paste("var_key_merged_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(var_key_merged())
      write.csv(var_key_merged(), file, row.names = FALSE)
    }
  )
}
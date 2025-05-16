crispr_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values to store data
    ko_data <- reactiveVal(NULL)
    wt_data <- reactiveVal(NULL)
    analysis_results <- reactiveVal(NULL)
    gsea_results <- reactiveVal(NULL)
    project_data <- reactiveVal(NULL)
    config_data <- reactiveVal(NULL)
    show_trees <- reactiveVal(FALSE)
    qc_html_files <- reactiveVal(NULL)
    design_data <- reactiveVal(NULL)
    filtered_data <- reactiveVal(NULL)  # New reactive value for filtered data
    
    # Function to read CSV with proper separator
    read_crispr_data <- function(file) {
      if (is.null(file)) return(NULL)
      tryCatch({
        data <- read.csv(file$datapath, sep = ";", stringsAsFactors = FALSE)
        # Convert comma-separated numbers to proper numeric
        data$control_count <- as.numeric(gsub(",", ".", data$control_count))
        data$treatment_count <- as.numeric(gsub(",", ".", data$treatment_count))
        return(data)
      }, error = function(e) {
        showNotification(
          paste("Error reading file:", e$message),
          type = "error",
          duration = NULL
        )
        return(NULL)
      })
    }
    
    # Function to handle file upload with progress
    handle_file_upload <- function(file) {
      if (is.null(file)) return(NULL)
      
      # Show progress bar
      shinyjs::show("upload_progress")
      shinyjs::hide("upload_error")
      
      tryCatch({
        # Create temporary directory for extraction
        temp_dir <- tempfile("crispr_upload_")
        dir.create(temp_dir)
        
        # Extract the archive
        if (grepl("\\.zip$", file$datapath)) {
          unzip(file$datapath, exdir = temp_dir)
        } else if (grepl("\\.tar\\.gz$", file$datapath)) {
          untar(file$datapath, exdir = temp_dir)
        } else if (grepl("\\.tar$", file$datapath)) {
          untar(file$datapath, exdir = temp_dir)
        }
        
        # Update progress
        shinyjs::html("upload_progress .progress-bar", "100%")
        shinyjs::html("upload_progress .progress-bar", "style", "width: 100%")
        
        # Return the extracted directory path
        return(temp_dir)
      }, error = function(e) {
        # Show error message
        shinyjs::show("upload_error")
        shinyjs::html("upload_error", paste("Error:", e$message))
        return(NULL)
      }, finally = {
        # Hide progress bar after a delay
        shinyjs::delay(1000, shinyjs::hide("upload_progress"))
      })
    }
    
    # Read and process KO data
    observe({
      req(input$ko_file)
      ko_data(read_crispr_data(input$ko_file))
    })
    
    # Read and process WT data
    observe({
      req(input$wt_file)
      wt_data(read_crispr_data(input$wt_file))
    })
    
    # Handle project folder upload
    observeEvent(input$project_folder, {
      req(input$project_folder)
      project_data(handle_file_upload(input$project_folder))
      
      # Get list of comparisons from rra_mageck directory
      if (!is.null(project_data())) {
        # Debug message
        cat("Project data path:", project_data(), "\n")
        
        # List all directories to find rra_mageck
        all_dirs <- list.dirs(project_data(), recursive = TRUE, full.names = TRUE)
        rra_dir <- all_dirs[grepl("rra_mageck$", all_dirs)]
        
        cat("Found rra_mageck directories:", paste(rra_dir, collapse = ", "), "\n")
        
        if (length(rra_dir) > 0) {
          comparisons <- list.dirs(rra_dir[1], full.names = FALSE, recursive = FALSE)
          cat("Found comparisons:", paste(comparisons, collapse = ", "), "\n")
          updateSelectInput(session, "comparison", choices = comparisons)
        } else {
          cat("No rra_mageck directory found!\n")
          updateSelectInput(session, "comparison", choices = NULL)
        }
      }
      
      # Populate Functional Analysis comparison dropdown (use config_data)
      observeEvent(config_data(), {
        if (!is.null(config_data())) {
          all_files <- list.files(config_data(), pattern = "comparisons_.*\\.txt$", recursive = TRUE, full.names = TRUE)
          if (length(all_files) > 0) {
            comp_df <- tryCatch({
              read.table(all_files[1], header = TRUE, sep = "\t", stringsAsFactors = FALSE)
            }, error = function(e) NULL)
            if (!is.null(comp_df) && "name" %in% names(comp_df)) {
              updateSelectInput(session, "fa_comparison", choices = comp_df$name)
            }
          }
        }
      })
    })
    
    # Handle config folder upload
    observeEvent(input$config_folder, {
      req(input$config_folder)
      config_data(handle_file_upload(input$config_folder))
      showNotification("Config folder uploaded and extracted!", type = "message")
    })
    
    # KO data preview
    output$ko_preview <- DT::renderDataTable({
      req(ko_data())
      DT::datatable(
        ko_data(),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          lengthMenu = list(c(5, 10, 25, 50, -1), c('5', '10', '25', '50', 'All')),
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        filter = 'top',
        selection = 'multiple',
        extensions = 'Buttons'
      ) %>%
      DT::formatRound(
        columns = c('control_count', 'treatment_count'),
        digits = 2
      )
    })
    
    # WT data preview
    output$wt_preview <- DT::renderDataTable({
      req(wt_data())
      DT::datatable(
        wt_data(),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          lengthMenu = list(c(5, 10, 25, 50, -1), c('5', '10', '25', '50', 'All')),
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        filter = 'top',
        selection = 'multiple',
        extensions = 'Buttons'
      ) %>%
      DT::formatRound(
        columns = c('control_count', 'treatment_count'),
        digits = 2
      )
    })
    
    # KO data summary
    output$ko_summary <- renderText({
      req(ko_data())
      data <- ko_data()
      paste(
        "Summary of Knockout data:",
        sprintf("Total rows: %d", nrow(data)),
        sprintf("Unique genes: %d", length(unique(data$Gene))),
        sprintf("Control count range: %.1f - %.1f", min(data$control_count), max(data$control_count)),
        sprintf("Treatment count range: %.1f - %.1f", min(data$treatment_count), max(data$treatment_count)),
        sep = " | "
      )
    })
    
    # WT data summary
    output$wt_summary <- renderText({
      req(wt_data())
      data <- wt_data()
      paste(
        "Summary of Wildtype data:",
        sprintf("Total rows: %d", nrow(data)),
        sprintf("Unique genes: %d", length(unique(data$Gene))),
        sprintf("Control count range: %.1f - %.1f", min(data$control_count), max(data$control_count)),
        sprintf("Treatment count range: %.1f - %.1f", min(data$treatment_count), max(data$treatment_count)),
        sep = " | "
      )
    })
    
    # Differential Analysis
    observeEvent(input$analyze_diff, {
      req(ko_data(), wt_data())
      ko <- ko_data()
      wt <- wt_data()
      
      # Calculate log2FC and other statistics for each guide
      process_data <- function(data) {
        data %>%
          dplyr::mutate(
            log2FC = log2((treatment_count + 1) / (control_count + 1)),
            total_reads = control_count + treatment_count,
            z_score = scale(log2FC)
          )
      }
      
      ko_processed <- process_data(ko)
      wt_processed <- process_data(wt)
      
      # Calculate gene-level statistics
      summarize_gene <- function(data) {
        data %>%
          dplyr::group_by(Gene) %>%
          dplyr::summarise(
            median_log2FC = median(log2FC),
            mean_log2FC = mean(log2FC),
            mad = mad(log2FC),
            n_guides = n(),
            total_reads = sum(total_reads),
            guide_consistency = mean(abs(z_score)),  # Lower means more consistent
            .groups = 'drop'
          )
      }
      
      ko_stats <- summarize_gene(ko_processed)
      wt_stats <- summarize_gene(wt_processed)
      
      # Merge and calculate differential statistics
      gene_stats <- dplyr::full_join(
        ko_stats %>% dplyr::rename_with(~paste0("ko_", .), -Gene),
        wt_stats %>% dplyr::rename_with(~paste0("wt_", .), -Gene),
        by = "Gene"
      )
      
      # Handle missing values
      for(col in names(gene_stats)) {
        if(any(is.na(gene_stats[[col]]))) {
          gene_stats[[col]][is.na(gene_stats[[col]])] <- 0
        }
      }
      
      # Calculate final statistics
      gene_stats <- gene_stats %>%
        dplyr::mutate(
          diff_effect = ko_median_log2FC - wt_median_log2FC,
          n_guides = pmax(ko_n_guides, wt_n_guides),
          combined_consistency = (ko_guide_consistency + wt_guide_consistency) / 2,
          
          # Calculate robust score
          score = abs(diff_effect) * sqrt(n_guides) / (1 + combined_consistency),
          
          # Calculate p-value using robust statistics
          effect_se = sqrt((ko_mad^2/ko_n_guides + wt_mad^2/wt_n_guides)),
          pvalue = 2 * pnorm(-abs(diff_effect / effect_se)),
          
          # Adjusted p-value
          padj = p.adjust(pvalue, method = "BH"),
          
          # Significance
          significant = padj < input$pval_threshold & abs(diff_effect) > input$fc_threshold,
          
          # Rank
          rank = rank(padj)
        ) %>%
        dplyr::arrange(padj, desc(abs(diff_effect)))
      
      # Store results
      analysis_results(gene_stats)
      
      # Print debugging information
      cat("Total genes analyzed:", nrow(gene_stats), "\n")
      cat("Significant genes:", sum(gene_stats$significant), "\n")
      cat("Genes with multiple guides:", sum(gene_stats$n_guides > 1), "\n")
    })
    
    # Update the volcano plot
    output$diff_analysis_plot <- renderPlotly({
      req(analysis_results())
      data <- analysis_results()
      
      plot_ly(data = data,
              x = ~diff_effect,
              y = ~-log10(padj),
              type = "scatter",
              mode = "markers",
              color = ~significant,
              colors = c("#808080", "#E41A1C"),
              text = ~paste("Gene:", Gene,
                          "<br>KO Log2FC:", round(ko_median_log2FC, 2),
                          "<br>WT Log2FC:", round(wt_median_log2FC, 2),
                          "<br>Differential effect:", round(diff_effect, 2),
                          "<br>Score:", round(score, 3),
                          "<br>Rank:", rank,
                          "<br>Number of sgRNAs:", n_guides,
                          "<br>padj:", format(padj, scientific = TRUE, digits = 3))) %>%
        layout(title = "Volcano Plot (KO vs WT Differential Effect)",
               xaxis = list(title = "Differential Effect (KO - WT Log2FC)"),
               yaxis = list(title = "-log10(adjusted p-value)"),
               shapes = list(
                 list(type = "line", x0 = -input$fc_threshold, x1 = -input$fc_threshold,
                      y0 = 0, y1 = max(-log10(data$padj)), line = list(dash = "dot")),
                 list(type = "line", x0 = input$fc_threshold, x1 = input$fc_threshold,
                      y0 = 0, y1 = max(-log10(data$padj)), line = list(dash = "dot")),
                 list(type = "line", x0 = -max(abs(data$diff_effect)), 
                      x1 = max(abs(data$diff_effect)),
                      y0 = -log10(input$pval_threshold), 
                      y1 = -log10(input$pval_threshold),
                      line = list(dash = "dot"))
               ))
    })
    
    # Shared reactive for gene summary data
    gene_summary_data <- reactive({
      req(input$comparison, project_data())
      all_dirs <- list.dirs(project_data(), recursive = TRUE, full.names = TRUE)
      rra_dir <- all_dirs[grepl("rra_mageck$", all_dirs)]
      summary_file <- file.path(
        rra_dir[1],
        input$comparison,
        paste0(input$comparison, ".gene_summary.txt")
      )
      if (file.exists(summary_file)) {
        data <- read.delim(summary_file, stringsAsFactors = FALSE)
        prepare_plot_data(data)
      } else {
        NULL
      }
    })

    # Results Table
    output$diff_data_preview <- DT::renderDataTable({
      data <- gene_summary_data()
      if (is.null(data)) return(NULL)
      DT::datatable(
        data,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        filter = 'top',
        selection = 'single',
        extensions = 'Buttons'
      ) %>%
      DT::formatRound(
        columns = c('neg.fdr', 'pos.fdr', 'neg.lfc', 'pos.lfc'),
        digits = 3
      )
    })

    # Volcano Plot
    output$volcano_plot <- renderPlotly({
      data <- gene_summary_data()
      if (is.null(data)) return(NULL)
      # For each gene, use the smaller FDR and corresponding LFC
      data$fdr <- pmin(data$neg.fdr, data$pos.fdr)
      use_neg <- data$neg.fdr < data$pos.fdr
      data$effect <- ifelse(use_neg, data$neg.lfc, data$pos.lfc)
      # Significance: either FDR < 0.05
      data$significant <- data$fdr < 0.05
      # Color: blue for significant neg, red for significant pos, gray otherwise
      data$color <- ifelse(data$significant,
                           ifelse(use_neg, "#377EB8", "#E41A1C"),
                           "#BDBDBD")
      data$hover <- paste0(
        data$id,
        " ; log2FC ", round(data$effect, 3),
        " ; FDR ", format(data$fdr, scientific = FALSE, digits = 3)
      )
      plot_ly(
        data = data,
        x = ~effect,
        y = ~-log10(fdr),
        type = "scatter",
        mode = "markers",
        marker = list(color = data$color, size = 8, opacity = 0.7),
        text = ~hover,
        hoverinfo = "text"
      ) %>%
        layout(
          title = NULL,
          xaxis = list(title = "Effect size (log2-fold change)"),
          yaxis = list(title = "-log10(FDR)"),
          showlegend = FALSE
        )
    })

    # Rank Plot
    output$rank_plot <- renderPlotly({
      data <- gene_summary_data()
      if (is.null(data)) return(NULL)
      data$fdr <- pmin(data$neg.fdr, data$pos.fdr)
      use_neg <- data$neg.fdr < data$pos.fdr
      data$effect <- ifelse(use_neg, data$neg.lfc, data$pos.lfc)
      data$significant <- data$fdr < 0.05
      data$color <- ifelse(data$significant,
                           ifelse(use_neg, "#377EB8", "#E41A1C"),
                           "#BDBDBD")
      data <- data[order(data$effect), ]
      data$rank <- seq_len(nrow(data))
      data$hover <- paste0(
        data$id,
        " ; log2FC ", round(data$effect, 3),
        " ; FDR ", format(data$fdr, scientific = FALSE, digits = 3)
      )
      plot_ly(
        data = data,
        x = ~effect,
        y = ~rank,
        type = "scatter",
        mode = "markers",
        marker = list(color = data$color, size = 8, opacity = 0.7),
        text = ~hover,
        hoverinfo = "text"
      ) %>%
        layout(
          title = NULL,
          xaxis = list(title = "Effect size (log2-fold change)"),
          yaxis = list(title = "Rank (by effect size)"),
          showlegend = FALSE
        )
    })

    # Update gene choices for box plot
    observe({
      req(ko_data(), wt_data())
      genes <- unique(ko_data()$Gene)
      updateSelectInput(session, "boxplot_gene",
                       choices = genes)
    })
    
    # Box Plot
    output$box_plot <- renderPlotly({
      req(ko_data(), wt_data(), input$boxplot_gene)
      ko <- ko_data()
      wt <- wt_data()
      selected_gene <- input$boxplot_gene
      
      # Prepare data for plotting
      ko_data_gene <- ko[ko$Gene == selected_gene, ]
      wt_data_gene <- wt[wt$Gene == selected_gene, ]
      
      # Create long format data for box plot
      plot_data <- data.frame(
        Condition = rep(c("KO Control", "KO Treatment", "WT Control", "WT Treatment"), 
                       c(length(ko_data_gene$control_count),
                         length(ko_data_gene$treatment_count),
                         length(wt_data_gene$control_count),
                         length(wt_data_gene$treatment_count))),
        Counts = c(ko_data_gene$control_count,
                  ko_data_gene$treatment_count,
                  wt_data_gene$control_count,
                  wt_data_gene$treatment_count)
      )
      
      # Create box plot
      plot_ly(plot_data, x = ~Condition, y = ~Counts, type = "box",
              color = ~Condition,
              colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")) %>%
        layout(title = paste("Expression of", selected_gene),
               xaxis = list(title = "Condition"),
               yaxis = list(title = "Counts"),
               showlegend = FALSE)
    })

    # Heatmap
    output$heatmap_plot <- renderPlotly({
      req(analysis_results(), input$top_n_genes, input$top_genes_metric, 
          input$heatmap_scale, ko_data(), wt_data())
      data <- analysis_results()
      ko <- ko_data()
      wt <- wt_data()
      
      # Select top genes based on chosen metric
      top_genes_idx <- switch(input$top_genes_metric,
        "diff_effect" = order(abs(data$diff_effect), decreasing = TRUE),
        "ko_effect" = order(abs(data$ko_median_log2FC), decreasing = TRUE),
        "wt_effect" = order(abs(data$wt_median_log2FC), decreasing = TRUE),
        "padj" = order(data$padj)  # For p-values, we want ascending order
      )[1:min(input$top_n_genes, nrow(data))]
      
      top_genes <- data$Gene[top_genes_idx]
      
      # Add selection metric to title
      title_suffix <- switch(input$top_genes_metric,
        "diff_effect" = "by KO vs WT Difference",
        "ko_effect" = "by KO Treatment vs Control Effect",
        "wt_effect" = "by WT Treatment vs Control Effect",
        "padj" = "by Statistical Significance"
      )
      
      # Function to get mean counts for each gene
      get_mean_counts <- function(data, genes) {
        data %>%
          dplyr::filter(Gene %in% genes) %>%
          dplyr::group_by(Gene) %>%
          dplyr::summarise(
            control = mean(control_count),
            treatment = mean(treatment_count),
            .groups = 'drop'
          )
      }
      
      # Get mean counts for KO and WT
      ko_counts <- get_mean_counts(ko, top_genes)
      wt_counts <- get_mean_counts(wt, top_genes)
      
      # Create matrix for heatmap
      matrix_data <- matrix(
        c(log2(ko_counts$control + 1),
          log2(ko_counts$treatment + 1),
          log2(wt_counts$control + 1),
          log2(wt_counts$treatment + 1)),
        ncol = 4,
        byrow = FALSE,
        dimnames = list(
          ko_counts$Gene,
          c("KO Control", "KO Treatment", "WT Control", "WT Treatment")
        )
      )
      
      # Choose between raw and Z-scored data
      plot_data <- if(input$heatmap_scale == "zscore") {
        # Scale rows (genes) to show relative changes across conditions
        t(scale(t(matrix_data)))
      } else {
        matrix_data
      }
      
      # Set color scale limits and title based on display type
      if(input$heatmap_scale == "zscore") {
        color_title <- "Relative Expression\n(standard deviations from mean)"
        color_limits <- max(abs(plot_data)) * c(-1, 1)
      } else {
        color_title <- "Expression Level\n(log2 counts)"
        color_limits <- range(plot_data)
      }
      
      # Add metric values to hover text
      metric_values <- switch(input$top_genes_metric,
        "diff_effect" = data$diff_effect[top_genes_idx],
        "ko_effect" = data$ko_median_log2FC[top_genes_idx],
        "wt_effect" = data$wt_median_log2FC[top_genes_idx],
        "padj" = data$padj[top_genes_idx]
      )
      
      metric_name <- switch(input$top_genes_metric,
        "diff_effect" = "KO vs WT Difference",
        "ko_effect" = "KO Log2FC",
        "wt_effect" = "WT Log2FC",
        "padj" = "Adjusted P-value"
      )
      
      # Create hover text based on display type
      hover_text <- if(input$heatmap_scale == "zscore") {
        paste(
          "Gene:", rownames(plot_data),
          "<br>Condition:", colnames(plot_data),
          "<br>Relative Expression:", round(plot_data, 2), "SD from mean",
          "<br>Actual log2 count:", round(matrix_data, 2),
          sprintf("<br>%s: %s", metric_name, 
                 ifelse(input$top_genes_metric == "padj",
                        format(metric_values, scientific = TRUE, digits = 3),
                        round(metric_values, 3)))
        )
      } else {
        paste(
          "Gene:", rownames(plot_data),
          "<br>Condition:", colnames(plot_data),
          "<br>log2 count:", round(plot_data, 2),
          sprintf("<br>%s: %s", metric_name, 
                 ifelse(input$top_genes_metric == "padj",
                        format(metric_values, scientific = TRUE, digits = 3),
                        round(metric_values, 3)))
        )
      }
      
      # Create heatmap
      plot_ly(
        x = colnames(plot_data),
        y = rownames(plot_data),
        z = plot_data,
        type = "heatmap",
        colors = colorRamp(c("#0000FF", "#FFFFFF", "#FF0000")),
        text = hover_text,
        hoverinfo = "text",
        colorbar = list(
          title = color_title,
          titleside = "right",
          limits = color_limits
        )
      ) %>%
        layout(
          title = paste("Expression Heatmap", title_suffix),
          xaxis = list(
            title = "Condition",
            tickangle = 45
          ),
          yaxis = list(
            title = "Genes",
            automargin = TRUE
          ),
          margin = list(l = 150, b = 100)  # Increase margins for labels
        )
    })

    # GSEA Analysis
    observeEvent(input$run_gsea, {
      req(analysis_results())
      data <- analysis_results()
      
      # Print debugging info
      cat("Starting enrichment analysis...\n")
      cat("Number of genes in data:", nrow(data), "\n")
      
      # Get significant genes based on user parameters
      pval_col <- input$pvalue_type
      sig_genes <- if(input$use_fc) {
        data$Gene[data[[pval_col]] < input$pvalue_cutoff & abs(data$log2FC) > input$fc_cutoff]
      } else {
        data$Gene[data[[pval_col]] < input$pvalue_cutoff]
      }
      
      cat("Number of significant genes:", length(sig_genes), "\n")
      
      # Convert gene symbols to ENTREZ IDs using org.Hs.eg.db
      gene_ids <- tryCatch({
        cat("Attempting to map gene symbols to ENTREZ IDs...\n")
        mapped_genes <- AnnotationDbi::select(
          x = org.Hs.eg.db::org.Hs.eg.db,
          keys = as.character(data$Gene),
          columns = c("ENTREZID", "SYMBOL"),
          keytype = "SYMBOL"
        )
        cat("Mapped", nrow(mapped_genes), "genes to ENTREZ IDs\n")
        # Remove duplicates if any
        mapped_genes <- mapped_genes[!duplicated(mapped_genes$SYMBOL), ]
        mapped_genes
      }, error = function(e) {
        cat("Error in gene ID mapping:", e$message, "\n")
        cat("Available packages:\n")
        print((.packages()))
        NULL
      })
      
      if (is.null(gene_ids)) {
        showNotification("Error: Could not map gene symbols to ENTREZ IDs", type = "error")
        return(NULL)
      }
      
      # Load appropriate gene set collection
      gene_sets <- tryCatch({
        sets <- switch(input$gsea_collection,
          "GO_BP" = msigdbr::msigdbr(species = "Homo sapiens", collection = "C5", subcollection = "GO:BP"),
          "GO_MF" = msigdbr::msigdbr(species = "Homo sapiens", collection = "C5", subcollection = "GO:MF"),
          "GO_CC" = msigdbr::msigdbr(species = "Homo sapiens", collection = "C5", subcollection = "GO:CC"),
          "KEGG" = msigdbr::msigdbr(species = "Homo sapiens", collection = "C2", subcollection = "CP:KEGG"),
          "REACTOME" = msigdbr::msigdbr(species = "Homo sapiens", collection = "C2", subcollection = "CP:REACTOME"),
          "HALLMARK" = msigdbr::msigdbr(species = "Homo sapiens", collection = "H")
        )
        cat("Loaded", nrow(sets), "gene sets from", input$gsea_collection, "\n")
        cat("Available columns in gene sets:\n")
        print(colnames(sets))
        sets
      }, error = function(e) {
        cat("Error loading gene sets:", e$message, "\n")
        NULL
      })
      
      if (is.null(gene_sets)) {
        showNotification("Error: Could not load gene sets", type = "error")
        return(NULL)
      }
      
      # Map significant genes to ENTREZ IDs
      sig_entrez <- gene_ids$ENTREZID[gene_ids$SYMBOL %in% sig_genes]
      cat("Mapped", length(sig_entrez), "significant genes to ENTREZ IDs\n")
      
      # Process gene sets with proper size filtering
      gene_sets_processed <- gene_sets %>%
        dplyr::select(gs_name, ncbi_gene) %>%
        dplyr::rename(entrez_gene = ncbi_gene) %>%
        # Count unique genes per set
        dplyr::group_by(gs_name) %>%
        dplyr::summarise(
          genes = list(unique(entrez_gene)),
          n_genes = length(unique(entrez_gene)),
          .groups = "drop"
        ) %>%
        # Filter based on size
        dplyr::filter(n_genes >= input$min_genes, n_genes <= input$max_genes)
      
      # Print debug info about gene set sizes
      cat("\nGene set size distribution:\n")
      print(summary(gene_sets_processed$n_genes))
      cat("Number of gene sets after size filtering:", nrow(gene_sets_processed), "\n")
      
      if (nrow(gene_sets_processed) == 0) {
        showNotification(
          paste("No gene sets found with size between", input$min_genes, "and", input$max_genes, "genes."),
          type = "warning"
        )
        return(NULL)
      }
      
      # Expand gene sets back to gene-set pairs for enrichment analysis
      gene_sets_final <- gene_sets_processed %>%
        dplyr::select(gs_name, genes) %>%
        tidyr::unnest(cols = c(genes)) %>%
        dplyr::rename(entrez_gene = genes)
      
      # Print final stats
      cat("\nFinal gene set statistics:\n")
      cat("Number of unique gene sets:", length(unique(gene_sets_final$gs_name)), "\n")
      cat("Total number of gene-set pairs:", nrow(gene_sets_final), "\n")
      
      # Perform enrichment analysis
      results <- tryCatch({
        if (input$enrichment_method == "gsea") {
          # For GSEA, prepare ranked gene list
          all_entrez <- gene_ids$ENTREZID
          gene_list <- -log10(data[[pval_col]]) * sign(data$log2FC)
          names(gene_list) <- all_entrez
          gene_list <- sort(gene_list, decreasing = TRUE)
          
          # Run GSEA
          clusterProfiler::GSEA(
            geneList = gene_list,
            TERM2GENE = gene_sets_final,
            minGSSize = input$min_genes,
            maxGSSize = input$max_genes,
            pvalueCutoff = 1,  # No filtering here, we'll filter in visualization
            verbose = TRUE
          )
        } else {
          # For ORA and other methods
          cat("Running ORA with", length(sig_entrez), "significant genes\n")
          cat("Universe size:", length(gene_ids$ENTREZID), "genes\n")
          cat("Gene sets size:", nrow(gene_sets_final), "rows\n")
          
          # Run enricher
          result <- clusterProfiler::enricher(
            gene = sig_entrez,
            TERM2GENE = gene_sets_final,
            minGSSize = input$min_genes,
            maxGSSize = input$max_genes,
            pvalueCutoff = 1,  # No filtering here, we'll filter in visualization
            universe = gene_ids$ENTREZID
          )
          
          if (is.null(result)) {
            cat("No enrichment results found\n")
            return(NULL)
          }
          
          cat("Enrichment completed with", nrow(result@result), "terms\n")
          result
        }
      }, error = function(e) {
        cat("Error in enrichment analysis:", e$message, "\n")
        print(traceback())
        NULL
      })
      
      if (!is.null(results)) {
        cat("Enrichment analysis completed successfully\n")
        cat("Number of enriched terms:", length(results@result$ID), "\n")
      }
      
      # Store results
      gsea_results(results)
    })
    
    # Results Table
    output$gsea_table <- DT::renderDataTable({
      req(gsea_results())
      results <- gsea_results()
      
      # Convert to data frame and check if we have results
      results_df <- as.data.frame(results@result)
      
      if (nrow(results_df) == 0) {
        showNotification("No significant enrichment results found", type = "warning")
        return(data.frame(
          ID = character(),
          Description = character(),
          GeneRatio = character(),
          BgRatio = character(),
          `P-value` = numeric(),
          `Adjusted P-value` = numeric(),
          `Q-value` = numeric(),
          Count = integer(),
          Genes = character(),
          stringsAsFactors = FALSE
        ))
      }
      
      # Format GeneRatio and BgRatio to maintain fraction format
      results_df$GeneRatio <- paste0(sapply(strsplit(results_df$GeneRatio, "/"), `[`, 1), "/",
                                   sapply(strsplit(results_df$GeneRatio, "/"), `[`, 2))
      results_df$BgRatio <- paste0(sapply(strsplit(results_df$BgRatio, "/"), `[`, 1), "/",
                                  sapply(strsplit(results_df$BgRatio, "/"), `[`, 2))
      
      # Convert ENTREZ IDs to gene symbols
      all_genes <- unique(unlist(strsplit(paste(results_df$geneID, collapse = "/"), "/")))
      gene_mapping <- AnnotationDbi::select(
        org.Hs.eg.db::org.Hs.eg.db,
        keys = all_genes,
        columns = "SYMBOL",
        keytype = "ENTREZID"
      )
      
      # Function to convert ENTREZ IDs to symbols for each row
      convert_to_symbols <- function(gene_string) {
        entrez_ids <- unlist(strsplit(gene_string, "/"))
        symbols <- gene_mapping$SYMBOL[match(entrez_ids, gene_mapping$ENTREZID)]
        symbols <- symbols[!is.na(symbols)]  # Remove any unmapped genes
        paste(symbols, collapse = "/")
      }
      
      # Convert genes column to symbols
      results_df$geneID <- sapply(results_df$geneID, convert_to_symbols)
      
      # Reorder and select columns
      results_df <- results_df[, c("ID", "Description", "GeneRatio", "BgRatio", 
                                 "pvalue", "p.adjust", "qvalue", "Count", "geneID")]
      
      # Rename columns for better display
      colnames(results_df) <- c("ID", "Description", "GeneRatio", "BgRatio", 
                              "P-value", "Adjusted P-value", "Q-value", "Count", "Genes")
      
      DT::datatable(
        results_df,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        filter = 'top',
        selection = 'multiple',
        extensions = 'Buttons'
      ) %>%
      DT::formatSignif(
        columns = c("P-value", "Adjusted P-value", "Q-value"),
        digits = 3
      )
    })
    
    # GSEA Plot
    output$gsea_plot <- renderPlotly({
      req(gsea_results())
      results <- gsea_results()
      
      # Convert to data frame and check if we have results
      results_df <- as.data.frame(results@result)
      
      if (nrow(results_df) == 0) {
        # Return an empty plot with a message
        plot_ly() %>%
          add_annotations(
            text = "No significant enrichment results found",
            showarrow = FALSE,
            font = list(size = 14)
          ) %>%
          layout(
            title = "Enrichment Results",
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          )
      } else if (input$enrichment_method == "gsea") {
        # GSEA enrichment plot
        enrichplot::gseaplot2(results, 
                           geneSetID = 1:min(5, length(results@result$ID)), 
                           pvalue_table = TRUE) %>%
          plotly::ggplotly()
      } else {
        # Bubble plot for other methods
        # Sort by adjusted p-value and take top 20 terms
        results_df <- results_df[order(results_df$p.adjust)[1:min(20, nrow(results_df))], ]
        
        # Convert GeneRatio to numeric for plotting
        results_df$GeneRatio_num <- sapply(strsplit(results_df$GeneRatio, "/"), 
                                       function(x) as.numeric(x[1])/as.numeric(x[2]))
        
        # Calculate rich factor and ensure it's numeric
        results_df$RichFactor <- sapply(1:nrow(results_df), function(i) {
          gene_ratio <- as.numeric(strsplit(results_df$GeneRatio[i], "/")[[1]])
          bg_ratio <- as.numeric(strsplit(results_df$BgRatio[i], "/")[[1]])
          as.numeric((gene_ratio[1]/gene_ratio[2])/(bg_ratio[1]/bg_ratio[2]))
        })
        
        # Ensure Description is a factor for proper ordering
        results_df$Description <- factor(results_df$Description,
                                     levels = rev(results_df$Description[order(results_df$p.adjust)]))
        
        # Create the plot
        p <- plot_ly()
        
        # Add markers
        p <- add_trace(p,
                      data = results_df,
                      x = ~GeneRatio_num,
                      y = ~Description,
                      type = "scatter",
                      mode = "markers",
                      marker = list(
                        size = ~(-log10(p.adjust) * 5),
                        color = ~RichFactor,
                        colorscale = list(
                          c(0, 0.5, 1),
                          c("#4575B4", "#FFFFBF", "#D73027")
                        ),
                        colorbar = list(
                          title = "Rich Factor",
                          titleside = "right"
                        ),
                        sizemode = "diameter",
                        sizeref = max(-log10(results_df$p.adjust))/20
                      ),
                      text = ~paste(
                        "Description:", Description,
                        "<br>Gene Ratio:", GeneRatio,
                        "<br>Rich Factor:", sprintf("%.2f", RichFactor),
                        "<br>Adjusted p-value:", format(p.adjust, scientific = TRUE, digits = 3),
                        "<br>Gene Count:", Count
                      ),
                      hoverinfo = "text"
        )
        
        # Update layout
        p %>% layout(
          title = list(
            text = "Top Enriched Terms",
            font = list(size = 16)
          ),
          xaxis = list(
            title = "Gene Ratio",
            tickfont = list(size = 12),
            titlefont = list(size = 14)
          ),
          yaxis = list(
            title = "",
            tickfont = list(size = 12),
            automargin = TRUE
          ),
          margin = list(l = 200),  # Increase left margin for term names
          showlegend = FALSE
        )
      }
    })
    
    # Network Plot
    output$gsea_network <- renderPlotly({
      req(gsea_results())
      results <- gsea_results()
      
      # Convert results to data frame
      plot_data <- as.data.frame(results@result)
      
      # Get top pathways (more than before to ensure we have enough connected nodes)
      plot_data$neglog10p <- -log10(plot_data$p.adjust)
      top_pathways <- plot_data[order(plot_data$p.adjust)[1:min(50, nrow(plot_data))], ]
      
      # Create similarity matrix based on shared genes
      n_paths <- nrow(top_pathways)
      sim_matrix <- matrix(0, nrow = n_paths, ncol = n_paths)
      rownames(sim_matrix) <- colnames(sim_matrix) <- top_pathways$Description
      
      # Calculate Jaccard similarity
      for(i in 1:n_paths) {
        for(j in 1:n_paths) {
          if(i != j) {
            genes_i <- unique(unlist(strsplit(top_pathways$geneID[i], "/")))
            genes_j <- unique(unlist(strsplit(top_pathways$geneID[j], "/")))
            intersection <- length(intersect(genes_i, genes_j))
            union <- length(union(genes_i, genes_j))
            sim_matrix[i,j] <- if(union > 0) intersection/union else 0
          }
        }
      }
      
      # Create network from similarity matrix with a lower threshold
      net <- igraph::graph_from_adjacency_matrix(
        sim_matrix > 0.2,  # Lower similarity threshold to show more connections
        mode = "undirected",
        weighted = TRUE
      )
      
      # Get layout with more spread
      set.seed(42)  # For reproducibility
      layout <- igraph::layout_with_fr(net, dim = 2, niter = 1000)
      
      # Scale layout to fit plot
      layout[,1] <- scales::rescale(layout[,1], to = c(-1, 1))
      layout[,2] <- scales::rescale(layout[,2], to = c(-1, 1))
      
      # Create edge list for visualization
      edges <- igraph::as_edgelist(net)
      edge_trace <- list()
      
      # Create edges
      if(nrow(edges) > 0) {
        for(i in 1:nrow(edges)) {
          from_idx <- match(edges[i,1], rownames(sim_matrix))
          to_idx <- match(edges[i,2], rownames(sim_matrix))
          edge_trace[[i]] <- list(
            x = layout[c(from_idx, to_idx), 1],
            y = layout[c(from_idx, to_idx), 2],
            type = 'scatter',
            mode = 'lines',
            line = list(color = 'rgba(190, 190, 190, 0.5)', width = 1),
            hoverinfo = 'none',
            showlegend = FALSE
          )
        }
      }
      
      # Create the plot starting with edges
      p <- plot_ly()
      
      # Add edges
      if(length(edge_trace) > 0) {
        for(trace in edge_trace) {
          p <- add_trace(p,
                        x = trace$x, y = trace$y,
                        type = trace$type, mode = trace$mode,
                        line = trace$line, hoverinfo = trace$hoverinfo,
                        showlegend = trace$showlegend)
        }
      }
      
      # Add nodes
      p <- add_trace(p,
                    x = layout[,1],
                    y = layout[,2],
                    type = 'scatter',
                    mode = 'markers+text',
                    marker = list(
                      size = top_pathways$Count/2,  # Size based on gene count
                      color = top_pathways$neglog10p,  # Color based on significance
                      colorscale = 'RdBu',
                      colorbar = list(
                        title = '-log10(adj.P)',
                        titleside = 'right'
                      ),
                      line = list(color = 'black', width = 1)
                    ),
                    text = ~top_pathways$Description,
                    textposition = 'top center',
                    textfont = list(size = 10),
                    hovertext = ~paste(
                      "Pathway:", top_pathways$Description,
                      "<br>P-value:", format(top_pathways$p.adjust, scientific = TRUE, digits = 3),
                      "<br>Gene Count:", top_pathways$Count
                    ),
                    hoverinfo = 'text'
      )
      
      # Update layout
      p %>% layout(
        title = "Pathway Similarity Network",
        showlegend = FALSE,
        hovermode = 'closest',
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE,
          title = ""
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE,
          title = ""
        ),
        margin = list(t = 50, b = 20, l = 20, r = 20)
      )
    })

    # Enrichment Map
    output$gsea_map <- renderPlotly({
      req(gsea_results())
      results <- gsea_results()
      results_df <- as.data.frame(results@result)
      
      if (nrow(results_df) == 0) {
        return(plot_ly() %>% 
          add_annotations(text = "No significant enrichment results found",
                        showarrow = FALSE))
      }
      
      # Get top terms
      top_terms <- head(results_df[order(results_df$p.adjust), ], 30)
      
      # Create hierarchical clustering based on gene overlap
      gene_matrix <- matrix(0, nrow = nrow(top_terms), ncol = nrow(top_terms))
      rownames(gene_matrix) <- colnames(gene_matrix) <- top_terms$Description
      
      for(i in 1:nrow(top_terms)) {
        for(j in 1:nrow(top_terms)) {
          genes_i <- unlist(strsplit(top_terms$geneID[i], "/"))
          genes_j <- unlist(strsplit(top_terms$geneID[j], "/"))
          overlap <- length(intersect(genes_i, genes_j))
          gene_matrix[i,j] <- overlap / sqrt(length(genes_i) * length(genes_j))
        }
      }
      
      # Create heatmap
      plot_ly(
        x = rownames(gene_matrix),
        y = rownames(gene_matrix),
        z = gene_matrix,
        type = "heatmap",
        colorscale = "RdBu",
        reversescale = TRUE,
        hoverongaps = FALSE,
        hovertemplate = paste(
          "Term 1: %{x}<br>",
          "Term 2: %{y}<br>",
          "Similarity: %{z:.2f}<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          title = "Enrichment Map (Term Similarity)",
          xaxis = list(
            tickangle = 45,
            tickfont = list(size = 10)
          ),
          yaxis = list(
            tickfont = list(size = 10)
          )
        )
    })
    
    # Term-Gene Network
    output$gene_term_network <- renderPlotly({
      req(gsea_results())
      results <- gsea_results()
      results_df <- as.data.frame(results@result)
      
      if (nrow(results_df) == 0) {
        return(plot_ly() %>% 
          add_annotations(text = "No significant enrichment results found",
                        showarrow = FALSE))
      }
      
      # Get top terms and their genes
      top_terms <- head(results_df[order(results_df$p.adjust), ], 10)
      
      # Get gene ID mapping
      gene_mapping <- AnnotationDbi::select(
        org.Hs.eg.db::org.Hs.eg.db,
        keys = unique(unlist(strsplit(paste(top_terms$geneID, collapse = "/"), "/"))),
        columns = "SYMBOL",
        keytype = "ENTREZID"
      )
      
      # Create nodes and edges data
      nodes <- data.frame()
      edges <- data.frame()
      
      for(i in 1:nrow(top_terms)) {
        term <- top_terms$Description[i]
        entrez_genes <- unlist(strsplit(top_terms$geneID[i], "/"))
        
        # Map ENTREZ IDs to symbols
        genes <- gene_mapping$SYMBOL[match(entrez_genes, gene_mapping$ENTREZID)]
        genes <- genes[!is.na(genes)]  # Remove any unmapped genes
        
        # Add term node
        nodes <- rbind(nodes, data.frame(
          id = term,
          label = term,
          type = "term",
          size = -log10(top_terms$p.adjust[i]),
          stringsAsFactors = FALSE
        ))
        
        # Add gene nodes and edges
        for(gene in genes) {
          if(!(gene %in% nodes$id)) {
            nodes <- rbind(nodes, data.frame(
              id = gene,
              label = gene,
              type = "gene",
              size = 1,
              stringsAsFactors = FALSE
            ))
          }
          edges <- rbind(edges, data.frame(
            from = term,
            to = gene,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Create layout using igraph
      set.seed(42)
      g <- igraph::graph_from_data_frame(edges, vertices = nodes)
      layout <- igraph::layout_with_fr(g, dim = 2)
      
      # Scale layout
      layout[,1] <- scales::rescale(layout[,1], to = c(-1, 1))
      layout[,2] <- scales::rescale(layout[,2], to = c(-1, 1))
      
      # Create plot
      p <- plot_ly()
      
      # Add edges
      for(i in 1:nrow(edges)) {
        from_idx <- match(edges$from[i], nodes$id)
        to_idx <- match(edges$to[i], nodes$id)
        p <- add_trace(p,
          x = layout[c(from_idx, to_idx), 1],
          y = layout[c(from_idx, to_idx), 2],
          mode = "lines",
          line = list(color = 'rgba(190, 190, 190, 0.5)', width = 0.5),
          hoverinfo = "none",
          showlegend = FALSE
        )
      }
      
      # Add term nodes
      term_nodes <- nodes[nodes$type == "term",]
      term_idx <- match(term_nodes$id, nodes$id)
      p <- add_trace(p,
        x = layout[term_idx, 1],
        y = layout[term_idx, 2],
        mode = "markers+text",
        marker = list(
          size = term_nodes$size * 10,
          color = "red",
          line = list(color = "black", width = 1)
        ),
        text = term_nodes$label,
        textposition = "top center",
        name = "Terms",
        hoverinfo = "text"
      )
      
      # Add gene nodes
      gene_nodes <- nodes[nodes$type == "gene",]
      gene_idx <- match(gene_nodes$id, nodes$id)
      p <- add_trace(p,
        x = layout[gene_idx, 1],
        y = layout[gene_idx, 2],
        mode = "markers+text",
        marker = list(
          size = 5,
          color = "blue",
          line = list(color = "black", width = 1)
        ),
        text = gene_nodes$label,
        textposition = "bottom center",
        name = "Genes",
        hoverinfo = "text"
      )
      
      p %>% layout(
        title = "Term-Gene Network",
        showlegend = TRUE,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        hovermode = "closest"
      )
    })

    # Report Generation Logic
    report_status <- reactiveVal("Ready to generate report")
    
    # Function to generate report content
    generate_report_content <- reactive({
      req(ko_data(), wt_data())
      
      # Create report content
      report_content <- list()
      
      # Create temporary directory for plots
      temp_dir <- file.path(tempdir(), paste0("report_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Add title and author
      report_content$title <- input$report_title
      report_content$author <- input$report_author
      report_content$date <- Sys.Date()
      
      # Add data summary if selected
      if ("data_summary" %in% input$report_sections) {
        ko <- ko_data()
        wt <- wt_data()
        report_content$data_summary <- list(
          ko_genes = length(unique(ko$Gene)),
          ko_guides = nrow(ko),
          wt_genes = length(unique(wt$Gene)),
          wt_guides = nrow(wt),
          ko_reads = sum(ko$control_count) + sum(ko$treatment_count),
          wt_reads = sum(wt$control_count) + sum(wt$treatment_count)
        )
      }
      
      # Add differential analysis results if selected
      if ("diff_analysis" %in% input$report_sections) {
        req(analysis_results())
        results <- analysis_results()
        
        # Save plots as HTML widgets
        volcano_plot <- plotly::plot_ly(data = results,
                x = ~diff_effect,
                y = ~-log10(padj),
                type = "scatter",
                mode = "markers",
                color = ~significant,
                colors = c("#808080", "#E41A1C"),
                text = ~paste("Gene:", Gene,
                            "<br>KO Log2FC:", round(ko_median_log2FC, 2),
                            "<br>WT Log2FC:", round(wt_median_log2FC, 2),
                            "<br>Differential effect:", round(diff_effect, 2),
                            "<br>Score:", round(score, 3),
                            "<br>Rank:", rank,
                            "<br>Number of sgRNAs:", n_guides,
                            "<br>padj:", format(padj, scientific = TRUE, digits = 3))) %>%
          layout(title = "Volcano Plot (KO vs WT Differential Effect)",
                 xaxis = list(title = "Differential Effect (KO - WT Log2FC)"),
                 yaxis = list(title = "-log10(adjusted p-value)"))
        
        ma_plot <- plotly::plot_ly(data = results,
                x = ~(ko_median_log2FC + wt_median_log2FC)/2,
                y = ~diff_effect,
                type = "scatter",
                mode = "markers",
                color = ~significant,
                colors = c("#808080", "#E41A1C"),
                text = ~paste("Gene:", Gene,
                            "<br>KO Log2FC:", round(ko_median_log2FC, 2),
                            "<br>WT Log2FC:", round(wt_median_log2FC, 2),
                            "<br>Differential effect:", round(diff_effect, 2),
                            "<br>padj:", format(padj, scientific = TRUE, digits = 3))) %>%
          layout(title = "MA Plot (KO vs WT)",
                 xaxis = list(title = "Average Log2 Fold Change"),
                 yaxis = list(title = "Differential Effect (KO - WT)"))
        
        # Save plots as HTML files
        volcano_file <- file.path(temp_dir, "volcano_plot.html")
        ma_file <- file.path(temp_dir, "ma_plot.html")
        
        htmlwidgets::saveWidget(volcano_plot, volcano_file, selfcontained = TRUE)
        htmlwidgets::saveWidget(ma_plot, ma_file, selfcontained = TRUE)
        
        report_content$diff_analysis <- list(
          total_genes = nrow(results),
          significant_genes = sum(results$significant),
          up_regulated = sum(results$significant & results$diff_effect > 0),
          down_regulated = sum(results$significant & results$diff_effect < 0),
          volcano_plot_file = volcano_file,
          ma_plot_file = ma_file,
          results_table = head(results[results$significant, ], 50)
        )
      }
      
      # Add GSEA results if selected
      if ("gsea" %in% input$report_sections) {
        req(gsea_results())
        results <- gsea_results()
        
        # Create enrichment plot
        if (input$enrichment_method == "gsea") {
          enrichment_plot <- enrichplot::gseaplot2(results, 
                                               geneSetID = 1:min(5, length(results@result$ID)), 
                                               pvalue_table = TRUE) %>%
                           plotly::ggplotly()
        } else {
          results_df <- head(as.data.frame(results@result)[order(results@result$p.adjust), ], 20)
          results_df$GeneRatio_num <- sapply(strsplit(results_df$GeneRatio, "/"), 
                                         function(x) as.numeric(x[1])/as.numeric(x[2]))
          results_df$Description <- factor(results_df$Description,
                                       levels = rev(results_df$Description))
          
          enrichment_plot <- plotly::plot_ly(data = results_df,
                     x = ~GeneRatio_num,
                     y = ~Description,
                     type = "scatter",
                     mode = "markers",
                     marker = list(
                       size = ~(-log10(p.adjust) * 5),
                       color = ~-log10(p.adjust),
                       colorscale = "RdBu"
                     ),
                     text = ~paste("Term:", Description,
                                 "<br>Gene Ratio:", GeneRatio,
                                 "<br>Adjusted p-value:", format(p.adjust, scientific = TRUE, digits = 3),
                                 "<br>Gene Count:", Count)) %>%
            layout(title = "Top Enriched Terms",
                   xaxis = list(title = "Gene Ratio"),
                   yaxis = list(title = ""))
        }
        
        # Save enrichment plot as HTML file
        enrichment_file <- file.path(temp_dir, "enrichment_plot.html")
        htmlwidgets::saveWidget(enrichment_plot, enrichment_file, selfcontained = TRUE)
        
        report_content$gsea <- list(
          method = input$enrichment_method,
          collection = input$gsea_collection,
          significant_terms = sum(results@result$p.adjust < input$pvalue_cutoff),
          enrichment_plot_file = enrichment_file,
          results_table = head(as.data.frame(results@result), 50)
        )
      }
      
      # Add methods description if selected
      if ("methods" %in% input$report_sections) {
        report_content$methods <- list(
          differential_analysis = paste(
            "Differential analysis was performed comparing knockout (KO) and wildtype (WT) conditions.",
            "For each gene, the median log2 fold change was calculated across all guides.",
            "Statistical significance was assessed using a robust test based on median absolute deviation,",
            "with p-values adjusted for multiple testing using the Benjamini-Hochberg method."
          ),
          enrichment_analysis = paste(
            "Gene set enrichment analysis was performed using", input$enrichment_method,
            "with", input$gsea_collection, "gene sets.",
            "Significance threshold was set at", input$pvalue_cutoff,
            "for adjusted p-values."
          )
        )
      }
      
      # Add session info if selected
      if (input$include_session_info) {
        report_content$session_info <- sessionInfo()
      }
      
      # Store temp_dir in report content for cleanup
      report_content$temp_dir <- temp_dir
      
      return(report_content)
    })
    
    # Download handler
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("crispr_analysis_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), 
               ".", input$report_format)
      },
      content = function(file) {
        report_status("Generating report...")
        
        # Get absolute paths
        root_dir <- dirname(getwd())  # Go up one level from 'app' directory
        template_path <- file.path(root_dir, "app", "reports", "crispr_report.Rmd")
        
        # Debug path
        report_status(paste("Looking for template at:", template_path))
        
        # Verify template exists
        if (!file.exists(template_path)) {
          report_status(paste("Error: Template file not found at", template_path))
          return()
        }
        
        # Create temporary directory for report generation
        temp_report_dir <- file.path(tempdir(), paste0("report_", format(Sys.time(), "%Y%m%d_%H%M%S")))
        dir.create(temp_report_dir, recursive = TRUE, showWarnings = FALSE)
        temp_rmd <- file.path(temp_report_dir, "report.Rmd")
        
        # Copy template
        file.copy(from = template_path, to = temp_rmd, overwrite = TRUE)
        
        if (!file.exists(temp_rmd)) {
          report_status(paste("Error: Failed to copy template to", temp_rmd))
          return()
        }
        
        # Generate report content
        content <- generate_report_content()
        
        # Generate report
        tryCatch({
          rmarkdown::render(
            input = temp_rmd,
            output_file = file,
            output_format = switch(input$report_format,
                                 "html" = rmarkdown::html_document(
                                   toc = TRUE,
                                   toc_float = TRUE,
                                   theme = "united",
                                   code_folding = if(input$include_code) "show" else "hide"
                                 ),
                                 "pdf" = rmarkdown::pdf_document(toc = TRUE),
                                 "docx" = rmarkdown::word_document(toc = TRUE)
            ),
            params = list(
              content = content,
              interactive = input$include_plots && input$report_format == "html"
            ),
            envir = new.env(),
            quiet = TRUE
          )
          report_status("Report generated successfully!")
        }, error = function(e) {
          report_status(paste("Error generating report:", e$message))
        }, finally = {
          # Clean up temporary directories
          if (!is.null(content$temp_dir)) {
            unlink(content$temp_dir, recursive = TRUE)
          }
          unlink(temp_report_dir, recursive = TRUE)
        })
      },
      contentType = switch(input$report_format,
                         "html" = "text/html",
                         "pdf" = "application/pdf",
                         "docx" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
      )
    )
    
    # Report status text
    output$report_status <- renderText({
      report_status()
    })
    
    # Report preview is no longer needed since we're not saving the file
    output$report_preview <- renderUI({
      div(
        class = "alert alert-info",
        "Click the download button to generate and download the report."
      )
    })

    # Load project button handler
    observeEvent(input$load_project, {
      req(project_data(), config_data())
      show_trees(TRUE)
      # Find the FastQC directory anywhere under project_data()
      fastqc_dirs <- list.dirs(project_data(), recursive = TRUE, full.names = TRUE)
      fastqc_dir <- fastqc_dirs[grepl("results[/\\\\]FastQC$", fastqc_dirs)]
      cat('Project data path:', project_data(), '\n')
      cat('Detected FastQC directories:', fastqc_dir, '\n')
      if (length(fastqc_dir) > 0 && dir.exists(fastqc_dir[1])) {
        htmls <- list.files(fastqc_dir[1], pattern = "(?i)\\.html$", recursive = TRUE, full.names = TRUE)
        rel_htmls <- list.files(fastqc_dir[1], pattern = "(?i)\\.html$", recursive = TRUE, full.names = FALSE)
        cat('Found HTML files (full):', htmls, '\n')
        cat('Found HTML files (relative):', rel_htmls, '\n')
        qc_html_files(setNames(htmls, rel_htmls))
        updateSelectInput(session, "qc_sample",
          choices = rel_htmls,
          selected = if (length(rel_htmls) > 0) rel_htmls[1] else NULL
        )
      } else {
        cat('FastQC directory does not exist!\n')
        qc_html_files(NULL)
        updateSelectInput(session, "qc_sample", choices = NULL)
      }
    })
    
    # Project structure display
    output$project_files <- renderDT({
      req(project_data())
      
      # List files in the project directory
      files <- list.files(project_data(), recursive = TRUE)
      
      # Create a data frame with file information
      file_info <- data.frame(
        File = files,
        Size = sapply(file.path(project_data(), files), function(f) {
          size <- file.size(f)
          if (size < 1024) paste0(size, " B")
          else if (size < 1024^2) paste0(round(size/1024, 1), " KB")
          else paste0(round(size/1024^2, 1), " MB")
        })
      )
      
      datatable(file_info,
                options = list(
                  pageLength = 10,
                  scrollX = TRUE
                ))
    })
    
    # Helper function to recursively build folder tree as HTML
    build_folder_tree <- function(path) {
      items <- list.files(path, full.names = TRUE)
      if (length(items) == 0) return(NULL)
      tags$ul(
        lapply(items, function(item) {
          name <- basename(item)
          if (dir.exists(item)) {
            subtree <- build_folder_tree(item)
            tags$li(
              span(class = "folder", paste0("📁 ", name)),
              if (!is.null(subtree))
                tags$ul(class = "hidden", subtree$children)
              else NULL,
              style = "list-style-type: none;"
            )
          } else {
            tags$li(span(class = "file", paste0("📄 ", name)), style = "list-style-type: none;")
          }
        })
      )
    }
    
    output$project_tree <- renderUI({
      req(show_trees())
      req(project_data())
      tree <- build_folder_tree(project_data())
      if (is.null(tree)) {
        div("No files found.")
      } else {
        div(tree)
      }
    })
    
    output$config_tree <- renderUI({
      req(show_trees())
      req(config_data())
      tree <- build_folder_tree(config_data())
      if (is.null(tree)) {
        div("No files found.")
      } else {
        div(tree)
      }
    })
    
    # Project summary
    output$project_summary <- renderText({
      req(show_trees())
      req(project_data())
      paste("Project loaded successfully with", 
            length(list.files(project_data(), recursive = TRUE)),
            "files")
    })
    
    # Clean up temporary files when session ends
    session$onSessionEnded(function() {
      if (!is.null(project_data())) {
        unlink(project_data(), recursive = TRUE)
      }
      if (!is.null(config_data())) {
        unlink(config_data(), recursive = TRUE)
      }
    })

    observeEvent(config_data(), {
      all_files <- list.files(config_data(), pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
      parent_dirs <- basename(dirname(all_files))
      print("All txt files found:")
      print(all_files)
      print("Parent dirs:")
      print(parent_dirs)
      matches <- all_files[
        grepl("comparisons", basename(all_files), ignore.case = TRUE) &
        grepl("_config_files", parent_dirs, ignore.case = TRUE)
      ]
      print("Matched files:")
      print(matches)
      if (length(matches) > 0) {
        df <- tryCatch({
          read.table(matches[1], header = TRUE, sep = "\t", stringsAsFactors = FALSE)
        }, error = function(e) {
          tryCatch({
            read.csv(matches[1], header = TRUE, stringsAsFactors = FALSE)
          }, error = function(e2) {
            tryCatch({
              read.delim(matches[1], header = TRUE, sep = "", stringsAsFactors = FALSE)
            }, error = function(e3) NULL)
          })
        })
        print("Experimental design data frame:")
        print(df)
        design_data(df)
      } else {
        design_data(NULL)
      }
    })

    output$qc_report <- renderUI({
      req(qc_html_files())
      req(input$qc_sample)
      # Use the relative path to get the full path
      file_path <- qc_html_files()[[input$qc_sample]]
      if (!is.null(file_path) && file.exists(file_path)) {
        temp_html <- tempfile(fileext = ".html")
        file.copy(file_path, temp_html, overwrite = TRUE)
        tags$iframe(src = sprintf("/custom_qc_html/%s", basename(temp_html)), width = "100%", height = "600px", frameborder = 0)
      } else {
        div("No HTML report found for this sample.")
      }
    })

    # Custom handler to serve temp HTML files
    shiny::addResourcePath("custom_qc_html", tempdir())

    output$design_table <- DT::renderDataTable({
      req(show_trees())
      req(design_data())
      n <- nrow(design_data())
      DT::datatable(
        design_data(),
        options = list(
          scrollX = TRUE,
          pageLength = n,
          paging = FALSE,
          searching = FALSE
        )
      )
    })

    output$design_table_with_header <- renderUI({
      req(show_trees())
      req(design_data())
      tagList(
        div(style = 'font-weight: bold; font-size: 18px; margin-bottom: 10px;', 'Experimental Design'),
        DT::dataTableOutput('design_table')
      )
    })

    # Function to prepare data for plotting
    prepare_plot_data <- function(data, filtered_data = NULL) {
      if (is.null(data)) return(NULL)
      
      # Calculate the most significant effect (positive or negative)
      data$effect <- ifelse(
        abs(data$neg.lfc) > abs(data$pos.lfc),
        data$neg.lfc,
        data$pos.lfc
      )
      
      # Get the corresponding FDR
      data$fdr <- ifelse(
        abs(data$neg.lfc) > abs(data$pos.lfc),
        data$neg.fdr,
        data$pos.fdr
      )
      
      # Add significance column
      data$significant <- data$fdr <= 0.05
      
      # Add filtered status if filtered data is provided
      if (!is.null(filtered_data)) {
        data$filtered <- data$id %in% filtered_data$id
      } else {
        data$filtered <- FALSE
      }
      
      return(data)
    }

    # Library Summary Table
    output$library_summary_table <- DT::renderDataTable({
      req(project_data())
      # Recursively find all .countsummary.txt files under the project folder
      all_files <- list.files(project_data(), pattern = "\\.countsummary\\.txt$", recursive = TRUE, full.names = TRUE)
      if (length(all_files) == 0) return(NULL)
      df <- tryCatch({
        read.delim(all_files[1], header = TRUE, stringsAsFactors = FALSE)
      }, error = function(e) NULL)
      if (is.null(df)) return(NULL)
      DT::datatable(
        df,
        options = list(
          scrollX = TRUE,
          pageLength = nrow(df),
          paging = FALSE,
          lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        filter = 'top',
        selection = 'single',
        extensions = 'Buttons'
      )
    })

    # Library Summary Barplot
    output$library_summary_barplot <- renderPlotly({
      req(project_data())
      all_files <- list.files(project_data(), pattern = "\\.countsummary\\.txt$", recursive = TRUE, full.names = TRUE)
      if (length(all_files) == 0) return(NULL)
      df <- tryCatch({
        read.delim(all_files[1], header = TRUE, stringsAsFactors = FALSE)
      }, error = function(e) NULL)
      if (is.null(df)) return(NULL)
      # Try to find the right columns for sample and reads
      sample_col <- grep("label|sample", names(df), ignore.case = TRUE, value = TRUE)[1]
      reads_col <- grep("reads", names(df), ignore.case = TRUE, value = TRUE)[1]
      if (is.null(sample_col) || is.null(reads_col)) return(NULL)
      plot_ly(
        data = df,
        x = ~get(sample_col),
        y = ~get(reads_col),
        type = 'bar',
        marker = list(color = ~get(sample_col)),
        text = ~round(get(reads_col), 0),
        textposition = 'auto',
        hoverinfo = 'x+y'
      ) %>%
        layout(
          xaxis = list(title = sample_col, tickangle = 45),
          yaxis = list(title = reads_col),
          showlegend = FALSE,
          margin = list(b = 120)
        )
    })

    # CRISPR Report HTML
    output$crispr_report_html <- renderUI({
      req(project_data())
      # Recursively find the CRISPR report HTML file
      all_files <- list.files(project_data(), pattern = "CRISPR_Screen_report.*reads.html$", recursive = TRUE, full.names = TRUE)
      if (length(all_files) == 0) return(div("No CRISPR report found."))
      # Copy to temp for serving
      temp_html <- tempfile(fileext = ".html")
      file.copy(all_files[1], temp_html, overwrite = TRUE)
      tags$iframe(src = sprintf("/custom_crispr_report/%s", basename(temp_html)), width = "100%", height = "600px", frameborder = 0)
    })
    shiny::addResourcePath("custom_crispr_report", tempdir())

    # Helper to find the rra_annotation folder for a comparison
    find_annotation_folder <- function(project_path, comparison) {
      all_dirs <- list.dirs(project_path, recursive = TRUE, full.names = TRUE)
      match <- grep(paste0("rra_annotation.*", comparison, "$"), all_dirs, value = TRUE)
      if (length(match) > 0) return(match[1])
      return(NULL)
    }

    # Helper to find a file by pattern in a folder
    find_file <- function(folder, pattern) {
      if (is.null(folder)) return(NULL)
      files <- list.files(folder, pattern = pattern, full.names = TRUE)
      if (length(files) > 0) return(files[1])
      return(NULL)
    }

    # Helper to find a plot PDF in plots/ subfolder
    find_plot_pdf <- function(folder, pattern) {
      plot_dir <- file.path(folder, "plots")
      if (!dir.exists(plot_dir)) return(NULL)
      files <- list.files(plot_dir, pattern = pattern, full.names = TRUE)
      if (length(files) > 0) return(files[1])
      return(NULL)
    }

    # Helper to find all GSEAplot PDFs in subfolder
    find_gseaplot_pdfs <- function(folder, subdir, pattern) {
      plot_dir <- file.path(folder, "plots", subdir)
      if (!dir.exists(plot_dir)) return(NULL)
      files <- list.files(plot_dir, pattern = pattern, full.names = TRUE)
      if (length(files) > 0) return(files)
      return(NULL)
    }

    # Render table and plots for all subtabs
    observe({
      req(project_data(), input$fa_comparison)
      comp <- input$fa_comparison
      ann_folder <- find_annotation_folder(project_data(), comp)
      if (is.null(ann_folder)) return()
      # Table file patterns
      tab_patterns <- list(
        fa_neg_go_table = paste0("GSEA.GO.*", comp, ".negative.tsv$"),
        fa_pos_go_table = paste0("GSEA.GO.*", comp, ".positive.tsv$"),
        fa_neg_reactome_table = paste0("GSEA.reactome.*", comp, ".negative.tsv$"),
        fa_pos_reactome_table = paste0("GSEA.reactome.*", comp, ".positive.tsv$")
      )
      # Plot file patterns
      plot_patterns <- list(
        dotplot = "dotplot",
        heatplot = "heatplot",
        treeplot = "treeplot",
        ESplot = "ESplot"
      )
      # GSEAplots subdirs
      gseaplot_subdirs <- list(
        fa_neg_go_gsea_plots = "GSEAplots.GO.negative",
        fa_pos_go_gsea_plots = "GSEAplots.GO.positive",
        fa_neg_reactome_gsea_plots = "GSEAplots.reactome.negative",
        fa_pos_reactome_gsea_plots = "GSEAplots.reactome.positive"
      )
      # Render tables
      for (nm in names(tab_patterns)) {
        local({
          n <- nm; pat <- tab_patterns[[nm]]
          output[[n]] <- DT::renderDataTable({
            f <- find_file(ann_folder, pat)
            if (is.null(f)) return(NULL)
            df <- tryCatch(read.delim(f, header = TRUE, stringsAsFactors = FALSE), error = function(e) NULL)
            if (is.null(df)) return(NULL)
            DT::datatable(df, options = list(scrollX = TRUE, pageLength = 20))
          })
        })
      }
      # Render plots (dotplot, heatplot, treeplot, ESplot)
      for (sel in c("neg", "pos")) {
        for (ann in c("go", "reactome")) {
          prefix <- paste0("fa_", sel, "_", ann, "_")
          for (plt in names(plot_patterns)) {
            local({
              n <- paste0(prefix, plt); pat <- paste0(plot_patterns[[plt]], ".*", comp, ".", ifelse(sel=="neg","negative","positive"), ".pdf$")
              output[[n]] <- renderUI({
                f <- find_plot_pdf(ann_folder, pat)
                if (is.null(f)) return(div("No plot found."))
                temp_png <- tempfile(fileext = ".png")
                # Convert PDF to PNG (requires system 'convert' from ImageMagick)
                system2("convert", c("-density", "150", shQuote(f), shQuote(temp_png)))
                tags$img(src = base64enc::dataURI(file = temp_png, mime = "image/png"), style = "width:100%;height:auto;")
              })
            })
          }
        }
      }
      # Render GSEAplots (multiple PDFs)
      for (nm in names(gseaplot_subdirs)) {
        local({
          n <- nm; subdir <- gseaplot_subdirs[[nm]]
          output[[n]] <- renderUI({
            files <- find_gseaplot_pdfs(ann_folder, subdir, paste0(comp, ".*pdf$"))
            if (is.null(files)) return(div("No GSEA plots found."))
            imgs <- lapply(files, function(f) {
              temp_png <- tempfile(fileext = ".png")
              system2("convert", c("-density", "150", shQuote(f), shQuote(temp_png)))
              tags$img(src = base64enc::dataURI(file = temp_png, mime = "image/png"), style = "width:100%;height:auto;margin-bottom:20px;")
            })
            do.call(tagList, imgs)
          })
        })
      }
    })
  })
} 
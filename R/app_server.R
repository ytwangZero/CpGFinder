#' CpGFinder App Server
#'
#' Server logic for the CpGFinder Shiny application
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @return Server function for Shiny app
#' @export
#'
#' @importFrom shiny reactiveValues renderText observeEvent updateSelectInput
#' @importFrom shiny req showNotification renderUI withProgress incProgress HTML
#' @importFrom shiny modalDialog showModal modalButton h4 p hr tags div icon
#' @importFrom shiny downloadHandler outputOptions actionButton reactive
#' @importFrom shinydashboard updateTabItems
#' @importFrom DT renderDT datatable formatStyle styleInterval
#' @importFrom readxl read_excel
#' @importFrom writexl write_xlsx
#' @importFrom dplyr distinct left_join count mutate arrange desc
#' @importFrom magrittr %>%
#' @importFrom tools file_ext
#' @examples
#' NULL


app_server <- function(input, output, session) {

  # Reactive values to store results
  rv <- reactiveValues(
    gene_coords = NULL,
    cpg_results = NULL,
    summary_results = NULL,
    genes_from_file = NULL,
    analysis_done = FALSE
  )

  output$progress_text <- renderText({
    "No results yet.\n\nPlease enter gene names (or upload a file) and click 'Find CpG Sites'."
  })

  observeEvent(input$array_version, {

    if (input$array_version == "v2_hg38") {
      updateSelectInput(session, "genome",
                        selected = "hg38")

    } else if (input$array_version == "v1_hg19") {
      updateSelectInput(session, "genome",
                        selected = "hg19")
    }
  })

  observeEvent(input$array_version, {

    if (input$array_version == "v2_hg38") {
      updateSelectInput(session, "genome",
                        choices = c("hg38" = "hg38"),
                        selected = "hg38")

    } else if (input$array_version == "v1_hg19") {
      updateSelectInput(session, "genome",
                        choices = c("hg19" = "hg19"),
                        selected = "hg19")
    }
  })

  # Navigate to search tab
  observeEvent(input$goto_search, {
    updateTabItems(session, "sidebar", "search")
  })

  # Handle file upload
  output$file_uploaded <- reactive({
    return(!is.null(input$gene_file))
  })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)

  observeEvent(input$gene_file, {
    req(input$gene_file)

    file_path <- input$gene_file$datapath
    file_ext <- tools::file_ext(input$gene_file$name)

    tryCatch({
      if (file_ext %in% c("xlsx", "xls")) {
        df <- read_excel(file_path)
      } else if (file_ext == "csv") {
        df <- read.csv(file_path, stringsAsFactors = FALSE)
      } else if (file_ext == "txt") {
        df <- read.table(file_path, stringsAsFactors = FALSE, header = FALSE)
      } else {
        showNotification("Unsupported file format", type = "error")
        return(NULL)
      }

      # Get first column
      genes <- df[[1]]
      genes <- unique(trimws(as.character(genes)))
      genes <- genes[nzchar(genes)]

      rv$genes_from_file <- genes

      showNotification(paste("Successfully loaded", length(genes), "genes"), type = "message")

    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })

  # Preview uploaded genes
  output$uploaded_genes_preview <- renderDT({
    req(rv$genes_from_file)

    df <- data.frame(
      Index = 1:length(rv$genes_from_file),
      Gene = rv$genes_from_file
    )

    datatable(df,
              options = list(pageLength = 10, scrollX = TRUE),
              caption = paste("Preview of", length(rv$genes_from_file), "genes loaded from file"))
  })

  observeEvent(input$show_format_guide, {

    showModal(
      modalDialog(
        title = "Input File Format Guide",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),

        h4("✔ Correct Format"),
        p("Your file should contain gene symbols in the first column only."),

        tags$table(
          class = "table table-bordered",
          tags$thead(
            tags$tr(
              tags$th("Gene")
            )
          ),
          tags$tbody(
            tags$tr(tags$td("BRCA1")),
            tags$tr(tags$td("TP53")),
            tags$tr(tags$td("EGFR")),
            tags$tr(tags$td("KRAS")),
            tags$tr(tags$td("MYC"))
          )
        ),

        hr(),

        h4("⚠ Common Mistakes"),
        tags$ul(
          tags$li("Multiple columns included in the file."),
          tags$li("Using Ensembl IDs instead of gene symbols."),
          tags$li("Empty rows or extra formatting."),
          tags$li("Merged cells or special formatting in Excel.")
        ),

        hr(),

        h4("Supported File Types"),
        tags$ul(
          tags$li("CSV (.csv)"),
          tags$li("Excel (.xlsx)")
        ),

        p("Only the first column will be used. Additional columns will be ignored.")
      )
    )
  })

  # Main analysis
  observeEvent(input$run_analysis, {
    rv$analysis_done <- FALSE
    # Get genes based on input method
    if (input$input_method == "text") {
      gene_text <- input$gene_input
      genes <- unlist(strsplit(gene_text, "[,\n]"))
      genes <- unique(trimws(genes))
      genes <- genes[nzchar(genes)]
    } else {
      genes <- rv$genes_from_file
    }

    if (is.null(genes) || length(genes) == 0) {
      showNotification("Please provide gene names", type = "error")
      return(NULL)
    }

    # Select annotation data
    if (input$array_version == "v2_hg38") {

      load(
        system.file(
          "app/www/annEPICv2_hg38.rda",
          package = "CpGFinder"
        )
      )
      ann_data <- annEPICv2_hg38

    } else if (input$array_version == "v1_hg19") {

      load(
        system.file(
          "app/www/annEPICv1_hg19.rda",
          package = "CpGFinder"
        )
      )
      ann_data <- annEPICv1_hg19
    }

    # Progress
    output$progress_text <- renderText({
      paste("Processing", length(genes), "genes...\nThis may take a few minutes.")
    })


    # Run analysis
    withProgress(message = 'Analyzing genes...', value = 0, {

      # Step 1: Get gene coordinates
      incProgress(0.2, detail = "Fetching gene coordinates from UCSC...")

      gene_coords <- ucsc_gene_coords_batch(genes,
                                            genome = input$genome,
                                            sleep_sec = input$sleep_time)

      rv$gene_coords <- gene_coords

      # Step 2: Find CpGs
      incProgress(0.4, detail = "Searching for CpG sites...")

      cpg_results <- find_cpgs_for_genes(gene_coords, ann_data)

      rv$cpg_results <- cpg_results

      # Step 3: Create summary
      incProgress(0.3, detail = "Creating summary...")

      if (!is.null(cpg_results) && nrow(cpg_results) > 0) {
        summary_results <- gene_coords %>%
          distinct(gene, chrom, start, end) %>%
          left_join(
            cpg_results %>%
              count(gene, name = "cpg_count"),
            by = "gene"
          ) %>%
          mutate(cpg_count = ifelse(is.na(cpg_count), 0, cpg_count)) %>%
          arrange(desc(cpg_count))
      } else {
        summary_results <- gene_coords %>%
          distinct(gene, chrom, start, end) %>%
          mutate(cpg_count = 0)
      }

      rv$summary_results <- summary_results

      incProgress(0.1, detail = "Done!")
    })

    # Update progress
    output$progress_text <- renderText({
      n_genes <- length(genes)
      n_found <- sum(!is.na(rv$gene_coords$start))
      n_cpgs <- if (!is.null(rv$cpg_results)) nrow(rv$cpg_results) else 0

      paste0(
        "Analysis Complete!\n",
        "Genes queried: ", n_genes, "\n",
        "Genes found: ", n_found, "\n",
        "Total CpG sites found: ", n_cpgs, "\n",
        "Genome: ", input$genome, "\n",
        "Array: ", input$array_version
      )
    })

    showNotification("Analysis completed successfully!", type = "message", duration = 5)
    rv$analysis_done <- TRUE

  })

  output$next_step_button <- renderUI({
    req(rv$analysis_done)

    div(
      style = "text-align:center; margin-top:20px;",
      actionButton(
        "go_results",
        "View Results",
        icon = icon("arrow-right"),
        class = "btn-getstarted"
      )
    )
  })


  observeEvent(input$go_results, {
    updateTabItems(session, "sidebar", "results")
  })


  # Summary table
  output$summary_table <- renderDT({

    if (is.null(rv$summary_results) || nrow(rv$summary_results) == 0) {
      return(
        datatable(
          data.frame(Message = "No results yet."),
          options = list(dom = 't'),
          rownames = FALSE
        )
      )
    }

    datatable(rv$summary_results,
              options = list(
                pageLength = 10,
                scrollX = TRUE
              ),
              caption = "Summary of CpG sites per gene",
              rownames = FALSE) %>%
      formatStyle('cpg_count',
                  backgroundColor = styleInterval(c(0, 10, 50, 100),
                                                  c('#ffcccc', '#ffffcc', '#ccffcc', '#ccffff', '#ccccff')))
  })

  # CpG table
  output$cpg_table <- renderDT({

    if (is.null(rv$cpg_results) || nrow(rv$cpg_results) == 0) {
      return(
        datatable(
          data.frame(Message = "No results yet."),
          options = list(dom = 't'),
          rownames = FALSE
        )
      )
    }

    datatable(rv$cpg_results,
              options = list(
                pageLength = 25,
                scrollX = TRUE,
                scrollY = "500px"
              ),
              filter = 'top',
              caption = "Detailed CpG site information",
              rownames = FALSE)
  })


  # Download handlers
  output$download_summary_excel <- downloadHandler(
    filename = function() {
      paste0("CpGFinder_summary_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(rv$summary_results, file)
    }
  )

  output$download_summary_csv <- downloadHandler(
    filename = function() {
      paste0("CpGFinder_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$summary_results, file, row.names = FALSE)
    }
  )


  output$download_cpg_excel <- downloadHandler(
    filename = function() {
      paste0("CpGFinder_detailed_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(rv$cpg_results, file)
    }
  )

  output$download_cpg_csv <- downloadHandler(
    filename = function() {
      paste0("CpGFinder_detailed_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$cpg_results, file, row.names = FALSE)
    }
  )
}

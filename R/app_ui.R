#' CpGFinder App UI
#'
#' @return A Shiny dashboard UI object
#' @export
#'
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody
#' @importFrom shinydashboard sidebarMenu menuItem tabItems tabItem box
#' @importFrom shiny icon tags fluidRow column h2 h3 h4 hr p wellPanel actionButton
#' @importFrom shiny radioButtons tagList div HTML textAreaInput fileInput
#' @importFrom shiny conditionalPanel numericInput selectInput verbatimTextOutput uiOutput
#' @importFrom shiny downloadButton helpText br strong
#' @importFrom DT DTOutput
#'
#' @examples
#' NULL

app_ui <- function() {

  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "CpGFinder"),

    dashboardSidebar(
      sidebarMenu(
        id = "sidebar",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("CpG Search", tabName = "search", icon = icon("search")),
        menuItem("Result Export", tabName = "results", icon = icon("table")),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      )
    ),

    dashboardBody(
      tags$head(
        tags$style(HTML("
        .box-header { background-color: #3c8dbc; color: white; }
        .content-wrapper { background-color: #f4f4f4; }
        .main-header .logo { font-weight: bold; }


        /* Get Started Button */
        .btn-getstarted {
          background-color: #1e88e5;
          color: white;
          border: none;
          border-radius: 30px;
          padding: 12px 28px;
          font-size: 18px;
          transition: all 0.3s ease;
        }

        .btn-getstarted:hover {
          background-color: #1565c0;
          transform: translateY(-2px);
          box-shadow: 0 6px 12px rgba(0,0,0,0.2);
        }

        /* Download Buttons */
        .btn-download {
          background-color: #455a64;
          color: white;
          border: none;
          border-radius: 20px;
          padding: 8px 18px;
          font-size: 14px;
          transition: all 0.3s ease;
        }

        .btn-download:hover {
          background-color: #263238;
          transform: translateY(-1px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }
      "))
      ),

      tabItems(
        # Home tab
        # Home tab
        tabItem(
          tabName = "home",

          fluidRow(
            box(
              width = 12,
              status = "primary",
              solidHeader = TRUE,

              h2("Welcome to CpGFinder!"),
              h4("Gene-level identification of CpG probes from Illumina MethylationEPIC arrays"),

              hr(),

              h4("Overview"),
              p("CpGFinder is a research-oriented web application designed to identify CpG probes located within genes of interest. The tool integrates genomic coordinate retrieval from the UCSC Genome Browser with Illumina EPIC array annotations to generate reproducible gene-level CpG summaries."),
              hr(),

              fluidRow(

                column(
                  4,
                  wellPanel(
                    h4("Input"),
                    tags$ul(
                      tags$li("Gene symbols (manual entry or file upload)"),
                      tags$li("Genome build selection (hg19 or hg38)"),
                      tags$li("EPIC array version (v1 or v2)")
                    )
                  )
                ),

                column(
                  4,
                  wellPanel(
                    h4("Processing"),
                    tags$ul(
                      tags$li("Genomic coordinate retrieval via UCSC API"),
                      tags$li("Coordinate overlap with EPIC probe annotations"),
                      tags$li("Automated gene-level CpG counting")
                    )
                  )
                ),

                column(
                  4,
                  wellPanel(
                    h4("Output"),
                    tags$ul(
                      tags$li("CpG probe counts per gene"),
                      tags$li("Detailed probe annotation tables"),
                      tags$li("Exportable CSV and Excel files")
                    )
                  )
                )
              ),

              hr(),

              h4("Supported Data"),
              tags$ul(
                tags$li("Illumina Infinium MethylationEPIC Array v1 (hg19)"),
                tags$li("Illumina Infinium MethylationEPIC Array v2 (hg38)"),
                tags$li("Official gene symbols only")
              ),

              hr(),

              h4("Author"),

              fluidRow(
                column(
                  4,
                  tags$p(strong("Name:"), " Wang Yuting"),
                  tags$p(strong("Email:"), " ytwangzero@outlook.com")
                ),
                column(
                  4,
                  tags$p(
                    strong("Profile: "),
                    tags$a("https://ytwangzero.github.io",
                           href = "https://ytwangzero.github.io/",
                           target = "_blank")
                  ),
                  tags$p(
                    strong("GitHub: "),
                    tags$a("github.com/ytwangZero",
                           href = "https://github.com/ytwangZero/",
                           target = "_blank")
                  )
                )
              ),

              hr(),

              div(
                style = "text-align:center; margin-top:25px;",
                actionButton(
                  "goto_search",
                  tagList(icon("rocket"), " Start Analysis"),
                  class = "btn-getstarted"
                )
              )
            )
          )
        ),

        # Search tab
        tabItem(
          tabName = "search",
          fluidRow(
            box(
              title = "Input Configuration",
              width = 12,
              status = "primary",
              solidHeader = TRUE,

              fluidRow(
                column(6,
                       radioButtons("input_method", "Input Method:",
                                    choices = c("Enter gene names" = "text",
                                                "Upload file (Excel/CSV)" = "file"),
                                    selected = "text")
                ),
                column(6,
                       selectInput("genome", "Genome Build:",
                                   choices = c("hg38" = "hg38", "hg19" = "hg19"),
                                   selected = "hg38"),
                       selectInput("array_version", "EPIC Array Version:",
                                   choices = c("EPICv2 (hg38)" = "v2_hg38",
                                               "EPICv1 (hg19)" = "v1_hg19"),
                                   selected = "v2_hg38")
                )
              ),

              hr(),

              conditionalPanel(
                condition = "input.input_method == 'text'",
                textAreaInput("gene_input",
                              "Enter Gene Names (one per line or comma-separated):",
                              value = "BRCA1, TP53, EGFR",
                              rows = 6,
                              width = "100%"),
                helpText("Examples: BRCA1, TP53, EGFR, KRAS, MYC")
              ),

              conditionalPanel(
                condition = "input.input_method == 'file'",
                fileInput("gene_file",
                          "Upload Gene List File:",
                          accept = c(".xlsx", ".xls", ".csv"),
                          buttonLabel = "Browse...",
                          placeholder = "No file selected"),
                helpText("Accepted formats: Excel (.xlsx, .xls) or CSV (.csv). The file should contain gene names in the first column."),
                br(),
                actionButton("show_format_guide",
                             tagList(icon("info-circle"), " View Format Guide"),
                             class = "btn-download"),

                conditionalPanel(
                  condition = "output.file_uploaded",
                  DTOutput("uploaded_genes_preview")
                )
              ),

              hr(),

              fluidRow(
                column(6,
                       numericInput("sleep_time", "API Rate Limit (seconds between requests):",
                                    value = 1, min = 0, max = 5, step = 0.5)
                )
              ),

              hr(),

              fluidRow(
                column(12, align = "center",
                       actionButton("run_analysis", "Find CpG Sites",
                                    icon = icon("play"),
                                    class = "btn-getstarted",
                                    width = "300px")
                )
              )
            )
          ),

          fluidRow(
            box(
              title = "Analysis Status",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              verbatimTextOutput("progress_text"),
              uiOutput("next_step_button")
            )
          )
        ),

        # Results tab
        tabItem(
          tabName = "results",
          fluidRow(
            box(
              title = "Summary Statistics",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              DTOutput("summary_table"),
              hr(),
              div(
                style = "text-align:right; margin-top:10px;",
                downloadButton("download_summary_excel",
                               "Download Summary (Excel)",
                               class = "btn-download"),
                tags$span(style = "margin-left:10px;"),
                downloadButton("download_summary_csv",
                               "Download Summary (CSV)",
                               class = "btn-download")
              )
            )
          ),

          fluidRow(
            box(
              title = "Detailed CpG Sites",
              width = 12,
              status = "warning",
              solidHeader = TRUE,
              DTOutput("cpg_table"),
              hr(),
              div(
                style = "text-align:right; margin-top:10px;",
                downloadButton("download_cpg_excel",
                               "Download CpG Table (Excel)",
                               class = "btn-download"),
                tags$span(style = "margin-left:10px;"),
                downloadButton("download_cpg_csv",
                               "Download CpG Table (CSV)",
                               class = "btn-download")
              )

            )
          )
        ),

        # About tab
        # About tab
        # About tab
        tabItem(
          tabName = "about",

          fluidRow(
            box(
              width = 12,
              status = "primary",
              solidHeader = TRUE,

              h2("About CpGFinder"),

              p("CpGFinder is a web-based application designed to identify CpG probes located within user-defined genes using Illumina Infinium MethylationEPIC array annotations. The tool integrates genomic coordinate retrieval from UCSC Genome Browser with EPIC array probe annotations to generate gene-level CpG summaries."),

              hr(),

              h3("Overview"),
              tags$ul(
                tags$li("Gene-level CpG probe identification"),
                tags$li("Support for hg19 (EPIC v1) and hg38 (EPIC v2)"),
                tags$li("Automated coordinate retrieval via UCSC API"),
                tags$li("Interactive result tables with export functionality")
              ),

              hr(),

              h3("Workflow"),

              tags$ol(
                tags$li("User provides gene symbols manually or uploads a gene list file."),
                tags$li("The application retrieves genomic coordinates from UCSC Genome Browser."),
                tags$li("EPIC array annotations are queried to identify overlapping CpG probes."),
                tags$li("Gene-level CpG counts and detailed probe tables are generated for export.")
              ),

              hr(),

              h3("Data Sources"),

              tags$ul(
                tags$li("UCSC Genome Browser API"),
                tags$li("Illumina Infinium MethylationEPIC Array v1 (hg19)"),
                tags$li("Illumina Infinium MethylationEPIC Array v2 (hg38)")
              ),

              p("Only official gene symbols are supported. Transcript-level inputs and Ensembl identifiers are not currently supported."),

              hr(),

              h3("Technical Details"),

              tags$ul(
                tags$li("Developed using R Shiny and shinydashboard"),
                tags$li("Annotation mapping based on genomic coordinate overlap"),
                tags$li("Interactive tables rendered using DT"),
                tags$li("Export functionality provided in CSV and Excel formats")
              ),

              hr(),

              h3("Citation"),

              p("If you use CpGFinder in your research, please cite the relevant Illumina EPIC annotation resources and UCSC Genome Browser."),

              hr(),

              h3("Disclaimer"),

              p("CpGFinder is provided for research purposes only. Results depend on external annotation databases and UCSC API availability. Users are responsible for validating results prior to publication or downstream analysis."),

              hr(),

              div(
                style = "text-align:center; color:#777;",
                tags$small("Version 1.0.0 | Last updated: 2026.02")
              )

            )
          )
        )

      )
    )
  )
}


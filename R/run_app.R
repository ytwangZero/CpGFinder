#' Run CpGFinder App
#' @import shinyWidgets
#' @importFrom shinydashboard dashboardPage dashboardHeader box
#' @importFrom shiny fluidRow renderText observeEvent
#' @importFrom DT datatable renderDT
#' @import httr2
#' @import dplyr
#' @import readxl
#' @import writexl

#' @export
run_app <- function() {

  shiny::shinyApp(
    ui = app_ui(),
    server = app_server
  )

}

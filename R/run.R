library(shiny)

source("R/ui.R")
source("R/server.R")

#' Run the Shiny application
#'
#' This function launches the Shiny application.
#'
#' @export
run <- function() {
    runApp(shinyApp(
        ui = ui,
        server = server
    ))
}

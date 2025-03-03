library(shiny)

source("R/input.R")
source("R/affectations.R")
source("R/tables.R")

#' Backend server logic
#'
#' @param input Input data from the frontend
#' @param output Output data the frontend will receive
server <- function(input, output) {
    df <- reactiveVal(NULL)

    observe({
        if (!is.null(input$file)) {
            data <- parse_file(input$file$datapath[1])
            as.integer(data$Classement)
            df(data)
            showNotification("Fichier charg\u00E9 avec succ\u00E8s.", type = "message")
        }
    })

    handle_affectations(input, output, df)

    output$download_button <- renderUI({
        downloadButton("download", paste("T\u00E9l\u00E9charger ", input$download_name, ".xlsx", sep = ""))
    })

    output$download <- downloadHandler(
        filename = function() {
            paste(input$download_name, ".xlsx", sep = "")
        },
        content = function(file) {
            write("Not implemented yet.", file)
        }
    )

    display_tables(input, output, df)
}

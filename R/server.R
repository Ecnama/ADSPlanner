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

    output$recuperation_button <- renderUI({
        downloadButton("recuperation_capacities", paste("Valider", sep=""))
    })

    output$recuperation_capacities <- renderTable({
        capacities <- c(
            "EII" = input$capacity_EII,
            "E&T" = input$capacity_EetT,
            "INFO" = input$capacity_INFO,
            "MA" = input$capacity_MA,
            "GCU" = input$capacity_GCU,
            "GMA" = input$capacity_GMA,
            "GPM" = input$capacity_GPM
        )
    })

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

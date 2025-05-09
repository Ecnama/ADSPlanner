library(shiny)

source("R/input.R")
source("R/output.R")
source("R/affectations.R")
source("R/tables.R")

#' Backend server logic
#'
#' @param input Input data from the frontend
#' @param output Output data the frontend will receive
#' @param session The Shiny session object
server <- function(input, output, session) {
    df <- reactiveVal(NULL)

    capacities <- reactiveVal(NULL)

    remaining_capacities <- reactiveVal(NULL)

    observe({
        if (!is.null(input$file)) {
            data <- parse_file(input$file$datapath[1])
            df(data$df)
            for (col in names(data$capacities)) {
                if (!is.null(data$capacities[[col]])) {
                    updateNumericInput(session, paste0("capacity_", col), value = data$capacities[[col]][1])
                }
            }
            showNotification("Fichier charg\u00E9 avec succ\u00E8s.", type = "message")
        }
    })

    handle_affectations(input, output, df, capacities, remaining_capacities)

    handle_capacities(input, output, df, capacities, remaining_capacities)

    output$download_button <- renderUI({
        if (is.null(df())) {
            tags$div(
                style = "color: gray; text-align: center;",
                "Aucun fichier charg\u00E9"
            )
        } else {
            downloadButton("download", paste("T\u00E9l\u00E9charger ", input$download_name, ".xlsx", sep = ""))
        }
    })

    output$download <- downloadHandler(
        filename = function() {
            paste(input$download_name, ".xlsx", sep = "")
        },
        content = function(file) {
            write_output(df(), capacities(), file)
        }
    )

    display_tables(input, output, df)
}

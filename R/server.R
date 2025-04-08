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

    capacities <- reactiveVal(NULL)

    observe({
        capacities <- c(
            "EII" = input$capacity_EII * 3,
            "E&T" = input$capacity_EetT * 3,
            "INFO" = input$capacity_INFO * 3,
            "MA" = input$capacity_MA * 3,
            "GCU" = input$capacity_GCU * 3,
            "GMA" = input$capacity_GMA * 3,
            "GPM" = input$capacity_GPM * 3
        )
    })

    remaining_capacities <- reactiveVal(capacities())

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

    capacity_full_shown <- reactiveVal(FALSE)

    output$capacity_full <- renderText({
        if (any(unlist(remaining_capacities) < 0)) {
            return("La capacité d'un département est pleine : changez de méthode d'affectation.")
        }
    })

    output$capacities_counters <- renderTable({
        remaining_capacities()
    })
}

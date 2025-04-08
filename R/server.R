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
        downloadButton("recuperation_capacities", paste("Valider", sep = ""))
    })

    output$recuperation_capacities <- renderUI({
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
        if (capacity_full_shown()) {
            return("La capacité d'un département est pleine : changez de méthode d'affectation.")
        }
    })

    capacities_counters_table <- reactive({
        capacities_counters_table <- capacities

        for (i in 1:3) {
            observeEvent(input$assign_depart_hard_i, {
                for (dep in names(capacities)) {
                    capacities_counters_table[dep] <- sum(assign_depart_hard(df(), i)[Aff_depart_i = dep])
                    if (capacities_counters_table[dep] < 0) {
                        capacity_full_shown(TRUE)
                    }
                }
            })
        }

        observeEvent(input$assign_depart_erase, {
            capacities_counters_table <- capacities
            capacity_full_shown(FALSE)
        })
    })

    output$capacities_counters <- renderTable({
        capacities_counters_table()
    })
}

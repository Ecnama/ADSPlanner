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

    capacities_counters_table <- reactive({
        capacities_counters_table <- capacities

        observeEvent(input$assign_depart_hard_1, {
            # todo : deduire de chaque capacité les affectations faites
        })

        observeEvent(input$assign_depart_hard_2, {
            # same
        })

        observeEvent(input$assign_depart_hard_3, {
            # same
            # if une capacité <0 mettre la cellule en rouge
            # et print un message pour indiquer qu'il n'y a plus de places dans le depart en question
        })

        observeEvent(input$assign_depart_erase, {
            capacities_counters_table <- capacities
        })
    })

    output$capacities_counters <- renderTable({
        capacities_counters_table()
    })
}

#' Function used by server to handle affectations
#'
#' @param input Input data from the frontend
#' @param output Output data the frontend will receive
#' @param df The reactive data frame of students's wishes and affectations
handle_affectations <- function(input, output, df) {
    try_affectation <- function(number) {
        if (is.null(df())) {
            showNotification("Aucun fichier charg\u00E9.", type = "warning")
            return()
        }

        df(assign_depart_hard(df(), number))

        showNotification(paste("D\u00E9partements des voeux ", number, " affect\u00E9s."), type = "message")
    }

    observeEvent(input$assign_depart_hard_1, try_affectation(1))

    observeEvent(input$assign_depart_hard_2, try_affectation(2))

    observeEvent(input$assign_depart_hard_3, try_affectation(3))

    observeEvent(input$assign_depart_real, {
        if (is.null(df())) {
            showNotification("Aucun fichier charg\u00E9.", type = "warning")
            return()
        }

        showNotification("Not implemented yet.", type = "warning")
    })

    observeEvent(input$assign_depart_erase, {
        if (is.null(df())) {
            showNotification("Aucun fichier charg\u00E9.", type = "warning")
            return()
        }

        df(assign_depart_erase(df()))

        showNotification("Affectations de d\u00E9partements effac\u00E9es.", type = "message")
    })
}

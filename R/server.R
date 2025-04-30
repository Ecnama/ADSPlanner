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

    remaining_capacities <- reactiveVal(NULL)

    handle_capacities(df, capacities(), remaining_capacities)

    observe({
        if (!is.null(input$file)) {
            capacities(c(
                "EII" = input$capacity_EII * 3,
                "E&T" = input$capacity_EetT * 3,
                "INFO" = input$capacity_INFO * 3,
                "MA" = input$capacity_MA * 3,
                "GCU" = input$capacity_GCU * 3,
                "GMA" = input$capacity_GMA * 3,
                "GPM" = input$capacity_GPM * 3
            ))
            remaining_capacities(capacities())
        }
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

    output$capacity_full <- renderText({
        if (is.null(remaining_capacities()) || length(remaining_capacities()) == 0) {
            return("")
        }
        if (any(as.numeric(remaining_capacities()) < 0)) {
            return("La capacité d'un département est dépassée : changez de méthode d'affectation.")
        } else {
            return("")
        }
    })

    output$capacities_counters <- renderUI({
        # Récupérer les capacités restantes
        remaining <- remaining_capacities()
        # Vérifier si les capacités sont valides
        if (is.null(remaining) || length(remaining) == 0) {
            return(NULL)
        }
        # Créer un data frame pour les départements et leurs capacités
        capacities_df <- data.frame(
            Department = names(remaining),
            Capacity = as.numeric(remaining)
        )
        # Générer un tableau HTML transposé
        html <- tags$table(
            style = "width: 40%; border-collapse: collapse; float: right; table-layout: fixed;",
            tags$thead(
                tags$tr(
                    lapply(capacities_df$Department, function(department) {
                        tags$th(department, style = "border: 1px solid black; padding: 5px; text-align: center;")
                    })
                )
            ),
            tags$tbody(
                tags$tr(
                    lapply(capacities_df$Capacity, function(capacity) {
                        tags$td(
                            capacity,
                            style = paste0(
                                "border: 1px solid black; padding: 5px; text-align: center;",
                                if (capacity < 0) "color: red; font-weight: bold;" else ""
                            )
                        )
                    })
                )
            )
        )
        # Retourner le tableau HTML
        html
    })
}

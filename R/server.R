library(shiny)

source("R/input.R")
source("R/affectations.R")

#' Backend server logic
#'
#' @param input Input data from the frontend
#' @param output Output data the frontend will receive
server <- function(input, output) {
    df <- reactiveVal(NULL)

    observe({
        if (!is.null(input$file)) {
            df(parse_file(input$file$datapath[1]))
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

    output$filter_filiere <- renderUI(
        selectInput(
            "select_filiere",
            "Fili\u00E8res :",
            df()$Filiere,
            multiple = TRUE,
        )
    )

    filtered_df <- reactive({
        filtered_df <- df()
        if (length(input$select_filiere) > 0) {
            filtered_df <- filtered_df[filtered_df$Filiere %in% input$select_filiere, ]
        }
        filtered_df <- filtered_df[
            grepl(tolower(input$filter_search), tolower(filtered_df[["Nom"]]), fixed = TRUE)
            | grepl(tolower(input$filter_search), tolower(filtered_df[["Prenom"]]), fixed = TRUE)
            | grepl(tolower(input$filter_search), tolower(paste(filtered_df[["Prenom"]], filtered_df[["Nom"]])), fixed = TRUE)
            | grepl(tolower(input$filter_search), tolower(paste(filtered_df[["Nom"]], filtered_df[["Prenom"]])), fixed = TRUE),
        ]
        filtered_df
    })

    output$vis_table <- renderTable(
        {
            filtered_df()[, !grepl("^Aff", names(filtered_df()))]
        },
        striped = TRUE
    )

    output$vis <- renderUI({
        if (is.null(input$file)) {
            HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100vh; font-weight: bold;">Veuillez charger un fichier pour commencer.</div>')
        } else {
            tableOutput("vis_table")
        }
    })

    output$aff_depart_table <- renderTable(
        {
            filtered_df_depart <- filtered_df()
            filtered_df_depart[["D\u00E9partements affect\u00E9s"]] <- apply(filtered_df_depart[, grepl("^Aff_depart_", names(filtered_df_depart))], 1, function(x) {
                x <- x[!is.na(x)]
                if (length(x) == 0) {
                    return(NA)
                }
                paste(x, collapse = ", ")
            })
            filtered_df_depart[, !grepl("^Aff", names(filtered_df_depart))]
        },
        striped = TRUE
    )

    output$aff_depart <- renderUI({
        if (is.null(input$file)) {
            HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100vh; font-weight: bold;">Veuillez charger un fichier pour commencer.</div>')
        } else {
            tableOutput("aff_depart_table")
        }
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

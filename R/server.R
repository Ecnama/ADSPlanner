library(shiny)
library(DT)

source("R/input.R")
source("R/affectations.R")
source("R/DTutilities.R")

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

    # output$filter_filiere <- renderUI(
    #     selectInput(
    #         "select_filiere",
    #         "Fili\u00E8res :",
    #         df()$Filiere,
    #         multiple = TRUE,
    #     )
    # )

    filtered_df <- reactive({
        filtered_df <- df()
        if (length(input$select_filiere) > 0) {
            filtered_df <- filtered_df[filtered_df$Filiere %in% input$select_filiere, ]
        }
        filtered_df
    })

    output$vis_table <- DT::renderDataTable(
        {
            filtered_df()[, !grepl("^Aff", names(filtered_df()))]
        },
        extensions = c("Scroller"),
        filter = "top",
        selection = "none",
        server = FALSE
    )

    output$vis <- renderUI({
        if (is.null(input$file)) {
            HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100vh; font-weight: bold;">Veuillez charger un fichier pour commencer.</div>')
        } else {
            DT::dataTableOutput("vis_table")
        }
    })

    output$aff_depart_table <- DT::renderDataTable(
        {
            filtered_df_depart <- df()
            filtered_df_depart[["D\u00E9partements affect\u00E9s"]] <- apply(filtered_df_depart[, grepl("^Aff_depart_", names(filtered_df_depart))], 1, function(x) {
                x <- x[!is.na(x)]
                if (length(x) == 0) {
                    return(NA)
                }
                paste(x, collapse = ", ")
            })
            filtered_df_depart[, !grepl("^Aff", names(filtered_df_depart))]
        },
        filter = "top",
        extensions = c("Select", "Buttons", "Scroller"),
        options = list(
            select = list(style = "multi+shift", items = "row"),
            dom = '<"top"lfB>rt<"bottom"ip><"clear">',
            buttons = dt_select_deselect_buttons,
            deferRender = TRUE,
            scrollY = 320,
            scroller = TRUE
        ),
        selection = "none",
        server = FALSE
    )

    output$aff_depart <- renderUI({
        if (is.null(input$file)) {
            HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100vh; font-weight: bold;">Veuillez charger un fichier pour commencer.</div>')
        } else {
            c(
                DT::dataTableOutput("aff_depart_table"),
                "Cliquez sur les lignes pour les s\u00E9lectionner. Les modifications ne s'appliqueront qu'aux lignes s\u00E9lectionn\u00E9es."
            )
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

        df(assign_depart_hard(df(), input$aff_depart_table_rows_selected, number))

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

        df(assign_depart_erase(df(), input$aff_depart_table_rows_selected))

        showNotification("Affectations de d\u00E9partements effac\u00E9es.", type = "message")
    })
}

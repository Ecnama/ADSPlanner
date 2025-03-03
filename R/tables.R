library(DT)

source("R/DTutilities.R")

#' Function used by server to display tables
#'
#' @param input Input data from the frontend
#' @param output Output data the frontend will receive
#' @param df The reactive data frame of students's wishes and affectations
display_tables <- function(input, output, df) {
    # UI definitions

    output$vis <- renderUI({
        if (is.null(input$file)) {
            HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100vh; font-weight: bold;">Veuillez charger un fichier pour commencer.</div>')
        } else {
            DTOutput("vis_table")
        }
    })

    output$aff_depart <- renderUI({
        if (is.null(input$file)) {
            HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100vh; font-weight: bold;">Veuillez charger un fichier pour commencer.</div>')
        } else {
            c(
                DTOutput("aff_depart_table"),
                "Cliquez sur les lignes pour les s\u00E9lectionner. Les modifications ne s'appliqueront qu'aux lignes s\u00E9lectionn\u00E9es."
            )
        }
    })

    # Table definitions

    output$vis_table <- renderDT(
        {
            df()[, !grepl("^Aff", names(df()))]
        },
        extensions = c("Scroller"),
        filter = "top",
        selection = "none",
        server = FALSE
    )

    output$aff_depart_table <- renderDT(
        {
            df_depart <- df()
            df_depart[["D\u00E9partements affect\u00E9s"]] <- apply(df_depart[, grepl("^Aff_depart_", names(df_depart))], 1, function(x) {
                x <- x[!is.na(x)]
                if (length(x) == 0) {
                    return(NA)
                }
                paste(x, collapse = ", ")
            })
            df_depart[, !grepl("^Aff", names(df_depart))]
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
}

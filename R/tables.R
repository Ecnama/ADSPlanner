library(DT)

source("R/DTutilities.R")
source("R/config.R")

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

    output$aff_session <- renderUI({
        if (is.null(input$file)) {
            HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100vh; font-weight: bold;">Veuillez charger un fichier pour commencer.</div>')
        } else {
            c(
                DTOutput("aff_session_table")
                #"Cliquez sur les lignes pour les s\u00E9lectionner. Les modifications ne s'appliqueront qu'aux lignes s\u00E9lectionn\u00E9es."
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
        fillContainer = TRUE,
        selection = "none",
        server = FALSE
    )

    output$aff_depart_table <- renderDT(
        {
            df_depart <- df()
            if (input$filter_full) {
                df_depart <- df_depart[sapply(seq_len(nrow(df_depart)), function(i) {
                    sum(!is.na(df_depart[i, grepl("^Aff_depart_", names(df_depart))])) < NB_SESSIONS[df_depart$Filiere[i]]
                }), ]
            }
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
        fillContainer = TRUE,
        selection = "none",
        server = FALSE
    )

    output$aff_session_table <- renderDT(
        {
            df_session <- df()
            df_session[["D\u00E9partements affect\u00E9s"]] <- apply(df_session[, grepl("^Aff_depart_", names(df_session))], 1, function(x) {
                x <- x[!is.na(x)]
                if (length(x) == 0) {
                    return(NA)
                }
                paste(x, collapse = ", ")
            })
            # Reorder the columns
            names(df_session) <- sub("^Aff_session_1$", "Session 1", names(df_session))
            names(df_session) <- sub("^Aff_session_2$", "Session 2", names(df_session))
            names(df_session) <- sub("^Aff_session_3$", "Session 3", names(df_session))
            df_session <- df_session[, !(grepl("^Aff", names(df_session)) | grepl("^V", names(df_session)))]
            df_session <- df_session[, c("Nom", "Prenom", "Classement", "Filiere", "D\u00E9partements affect\u00E9s", "Session 1", "Session 2", "Session 3")]
            df_session
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
        fillContainer = TRUE,
        selection = "none",
        server = FALSE
    )
}

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
            filter_for_table(df(), "vis", input)
        },
        extensions = c("Scroller"),
        filter = "top",
        fillContainer = TRUE,
        selection = "none",
        server = FALSE
    )

    output$aff_depart_table <- renderDT(
        {
            filter_for_table(df(), "aff_depart", input)
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
            filter_for_table(df(), "aff_session", input)
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

filter_for_table <- function(df, table, input) {
    switch(table,
        "vis" = {
            df[, !grepl("^Aff", names(df))]
        },
        "aff_depart" = {
            if (input$filter_full) {
                df <- df[sapply(seq_len(nrow(df)), function(i) {
                    sum(!is.na(df[i, grepl("^Aff_depart_", names(df))])) < NB_SESSIONS[df$Filiere[i]]
                }), ]
            }
            df[["D\u00E9partements affect\u00E9s"]] <- apply(df[, grepl("^Aff_depart_", names(df))], 1, function(x) {
                x <- x[!is.na(x)]
                if (length(x) == 0) {
                    return(NA)
                }
                paste(x, collapse = ", ")
            })
            df[, !grepl("^Aff", names(df))]
        },
        "aff_session" = {
            df[["D\u00E9partements affect\u00E9s"]] <- apply(df[, grepl("^Aff_depart_", names(df))], 1, function(x) {
                x <- x[!is.na(x)]
                if (length(x) == 0) {
                    return(NA)
                }
                paste(x, collapse = ", ")
            })
            # Reorder the columns
            names(df) <- sub("^Aff_session_1$", "Session 1", names(df))
            names(df) <- sub("^Aff_session_2$", "Session 2", names(df))
            names(df) <- sub("^Aff_session_3$", "Session 3", names(df))
            df <- df[, !(grepl("^Aff", names(df)) | grepl("^V", names(df)))]
            df <- df[, c("Nom", "Prenom", "Classement", "Filiere", "D\u00E9partements affect\u00E9s", "Session 1", "Session 2", "Session 3")]
            df
        },
        stop("Unknown table for filtering")
    )
}

get_selection <- function(df, table, input) {
    df <- filter_for_table(df, table, input)
    realsel <- c()
    for (i in input[[paste0(table, "_table_rows_selected")]]) {
        cpt <- 1
        for (j in rownames(df)) {
            if (cpt == i) {
                realsel <- c(realsel, as.numeric(j))
                break
            }
            cpt <- cpt + 1
        }
    }
    print(input[[paste0(table, "_table_rows_selected")]])
    print(realsel)
    cat("\n")
    realsel
}

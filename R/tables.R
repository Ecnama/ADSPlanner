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
        selection = "none",
        server = FALSE
    )

    output$aff_depart_table <- renderDT({
        filtered <- filter_for_table(df(), "aff_depart", input)
        dt <- datatable(
            filtered,
            filter = "top",
            extensions = c("Select", "Buttons", "Scroller"),
            options = list(
                select = list(style = "multi+shift", items = "row"),
                dom = '<"top"lfB>rt<"bottom"ip><"clear">',
                buttons = dt_select_deselect_buttons,
                deferRender = TRUE,
                scrollY = 350,
                scroller = TRUE,
                rowCallback = JS(color_departments_column(df(), filtered))
            ),
            selection = "none"
        )
        dt
    }, server = FALSE)

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
            scrollY = 350,
            scroller = TRUE
        ),
        selection = "none",
        server = FALSE
    )
}

#' Filter the data frame for the specified table
#'
#' @param df The data frame to filter
#' @param table The name of the table to filter for
#' @param input The input data from the frontend
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

#' Get the selection from the specified table
#'
#' @param df The main data frame
#' @param table The name of the table to select from
#' @param input The input data from the frontend
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

    realsel
}

#' Create a JavaScript function to color the departments column according to the worst wish of a student
#'
#' @param df The full main data frame
#' @param filtered The filtered data frame that will be displayed
#' @return A JavaScript function as a vector of lines
color_departments_column <- function(df, filtered) {
    green_rows <- c()
    yellow_rows <- c()
    orange_rows <- c()
    red_rows <- c()

    aff_cols <- grep("Aff_depart_", names(df))
    aff_session1_idx <- which(names(df) == "Aff_depart_1")
    v1_idx <- which(names(filtered) == "V1")

    for (i in seq_len(nrow(filtered))) {
        wishes <- c()
        j <- v1_idx
        while (j <= length(names(filtered)) && grepl("V", names(filtered)[j])) {
            if (!is.na(filtered[i, j])) {
                wishes <- c(wishes, filtered[i, j])
            }
            j <- j + 1
        }

        indices <- c()
        j <- aff_session1_idx - 1
        for (col in aff_cols) {
            j <- j + 1
            if (is.na(df[rownames(filtered)[i], col])) {
                next
            }

            indices <- c(indices, match(df[rownames(filtered)[i], col], wishes)[1])
        }

        if (length(indices) == 0) {
            next
        }

        maxi <- max(indices, na.rm = TRUE)
        if (maxi <= 3) {
            green_rows <- c(green_rows, i)
        } else if (maxi <= 4) {
            yellow_rows <- c(yellow_rows, i)
        } else if (maxi <= 5) {
            orange_rows <- c(orange_rows, i)
        } else if (maxi >= 6) {
            red_rows <- c(red_rows, i)
        }
    }

    column <- which(names(filtered) == "D\u00E9partements affect\u00E9s")

    c(
        "function(row, data, num, index){",
        paste0("  const green_rows = [", paste(green_rows - 1, collapse = ","), "];"),
        paste0("  const yellow_rows = [", paste(yellow_rows - 1, collapse = ","), "];"),
        paste0("  const orange_rows = [", paste(orange_rows - 1, collapse = ","), "];"),
        paste0("  const red_rows = [", paste(red_rows - 1, collapse = ","), "];"),
        "  const firstNumber = parseInt(data[0], 10) - 1;",
        "  if (green_rows.includes(firstNumber)) {",
        sprintf("    $('td:eq(' + %d + ')', row)", column),
        "    .css({'background-color': '#b2ffb2'});",
        "  } else if (yellow_rows.includes(firstNumber)) {",
        sprintf("    $('td:eq(' + %d + ')', row)", column),
        "    .css({'background-color': '#ffffb7'});",
        "  } else if (orange_rows.includes(firstNumber)) {",
        sprintf("    $('td:eq(' + %d + ')', row)", column),
        "    .css({'background-color': '#ffdba5'});",
        "  } else if (red_rows.includes(firstNumber)) {",
        sprintf("    $('td:eq(' + %d + ')', row)", column),
        "    .css({'background-color': '#ffa5b7'});",
        "  }",
        "}"
    )
}

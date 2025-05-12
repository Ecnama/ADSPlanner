NB_SESSIONS <- c(
    "FC_FIRE" = 3,
    "EMIR" = 2,
    "MICA" = 2
)

#' Function used by server to handle affectations
#'
#' @param input Input data from the frontend
#' @param output Output data the frontend will receive
#' @param df The reactive data frame of students's wishes and affectations
handle_affectations <- function(input, output, df) {
    common_checks <- function() {
        if (is.null(df())) {
            showNotification("Aucun fichier charg\u00E9.", type = "warning")
            return(FALSE)
        }

        if (is.null(input$aff_depart_table_rows_selected)) {
            showNotification("Aucune ligne s\u00E9lectionn\u00E9e.", type = "warning")
            return(FALSE)
        }
        TRUE
    }

    wish_input <- reactiveVal(1)

    observeEvent(input$assign_depart_hard, {
        if (!common_checks()) {
            return()
        }

        showModal(modalDialog(
            title = "Affectation dure",
            numericInput("wish_selection", "Num\u00E9ro de voeu", value = wish_input(), min = 1, max = 7),
            footer = tagList(
                modalButton("Annuler"),
                actionButton("confirm_assign_depart_hard", "Confirmer")
            )
        ))
    })

    old_department_input <- reactiveVal(NA)
    new_department_input <- reactiveVal(NA) 

    observeEvent(input$assign_depart_targeted, {
        if (!common_checks()) {
            return()
        }

        showModal(modalDialog(
        title = "Affectation ciblée",
        tagList(
            selectInput("old_department", "Département actuel :",
                        choices = c("EII", "E&T", "MA", "INFO", "GCU", "GPM", "GMA"), selected = old_department_input(), selectize = FALSE),
            selectInput("new_department", "Nouveau département :",
                        choices = c("EII", "E&T", "MA", "INFO", "GCU", "GPM", "GMA"), selected = new_department_input(), selectize = FALSE)
        ),
        footer = tagList(
            modalButton("Annuler"),
            actionButton("confirm_assign_targeted", "Confirmer"))
        ))

        observeEvent(input$confirm_assign_targeted, ignoreInit = TRUE, {
            
            old_department_input(input$old_department)
            new_department_input(input$new_department)

            removeModal()
            handle_operation(function(df, selection) targeted_affectation(df, selection, input$old_department, input$new_department))
        }, once = TRUE)
    })

    handle_operation <- function(operation) {
        if (!common_checks()) {
            return()
        }

        r <- operation(df(), input$aff_depart_table_rows_selected)
        df(r$df)

        if (length(r$fails) == length(input$aff_depart_table_rows_selected)) {
            showNotification("Op\u00E9ration impossible pour tous les \u00E9l\u00E9ments s\u00E9lectionn\u00E9s.", type = "warning")
        } else if (length(r$fails) > 5) {
            showNotification(paste("Op\u00E9ration impossible pour", length(r$fails), "\u00E9l\u00E9ments "), type = "warning")
        } else if (length(r$fails) > 0) {
            showNotification(
                paste("Op\u00E9ration impossible pour les \u00E9l\u00E9ments ",
                    paste(
                          paste(
                                df()[r$fails, ]$Nom, df()[r$fails, ]$Prenom, sep = " "),
                          collapse = ", "), ".",
                    sep = ""
                ),
                type = "warning"
            )
        } else {
            showNotification("Op\u00E9ration r\u00E9alis\u00E9e.", type = "message")
        }
    }

    observeEvent(input$confirm_assign_depart_hard, {
        wish_input(input$wish_selection)
        removeModal()

        handle_operation(function(df, selection) assign_depart_hard(df, selection, wish_input()))
    })

    observeEvent(input$assign_depart_real, {
        showNotification("Not implemented yet.", type = "warning")
    })

    observeEvent(input$assign_depart_erase, {
        handle_operation(assign_depart_erase)
    })
}

#' Erase all affected departments
#'
#' @param df The data frame with the students and their wishes
#' @param selection The indices of students to erase
#' @return A list with (list: The input data frame with erased departments, fails: An empty vector because this can never fail)
assign_depart_erase <- function(df, selection) {
    df[selection, ]$Aff_depart_1 <- NA_character_
    df[selection, ]$Aff_depart_2 <- NA_character_
    df[selection, ]$Aff_depart_3 <- NA_character_

    list(df = df, fails = c())
}

#' Assign departments to students according to their wishes, regardless of the number of places
#'
#' @param df The data frame with the students and their wishes
#' @param selection The indices of students to assign
#' @param wish_number The number of the wish to assign
#' @return A list with (list: The input data frame with affected departments, fails: The indices of students that could not be assigned)
assign_depart_hard <- function(df, selection, wish_number) {
    fails <- c()

    for (i in selection) {
        if (is.na(df[[paste("V", wish_number, sep = "")]][i])) {
            fails <- c(fails, i)
            next()
        }
        j <- 1
        while (j <= NB_SESSIONS[df$Filiere[i]]) {
            if (is.na(df[[paste("Aff_depart_", j, sep = "")]][i])) {
                df[[paste("Aff_depart_", j, sep = "")]][i] <- df[[paste("V", wish_number, sep = "")]][i]
                break()
            } else if (df[[paste("Aff_depart_", j, sep = "")]][i] == df[[paste("V", wish_number, sep = "")]][i]) { # Don't assign the same department twice
                break()
            } else {
                j <- j + 1
            }
        }
        if (j > NB_SESSIONS[df$Filiere[i]]) {
            fails <- c(fails, i)
        }
    }

    list(df = df, fails = fails)
}

#' assign certain students to a certain departement, deleting it from a certain departement
#'
#' @param df The data frame with the students and their wishes
#' @param selection The indices of students to assign
#' @param old_depart The departement the students were assigned
#' @param new_depart The departement to which the students will be assigned
#' @return The input data frame with affected departments
targeted_affectation <- function(df, selection, old_depart, new_depart) {
    fails <- c()
    for (i in selection) {
        j <- 1
        success <- FALSE
        while (j <= NB_SESSIONS[df$Filiere[i]]) {
            col_name <- paste("Aff_depart_", j, sep = "")
            current_val <- df[[col_name]][i]
            if (!is.na(current_val) && current_val == old_depart) {
                if (old_depart != new_depart &&
                    any(df[i, paste0("Aff_depart_", 1:NB_SESSIONS[df$Filiere[i]])] == new_depart, na.rm = TRUE)) {
                    fails <- c(fails, i)
                    break
                }
                df[[col_name]][i] <- new_depart
                success <- TRUE
                break
            }
            j <- j + 1
        }
        if (!success) {
            fails <- c(fails, i)
        }
    }
    list(df = df, fails = fails)
}

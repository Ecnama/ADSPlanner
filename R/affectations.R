library(combinat)

NB_SESSIONS <- c(
    "FC_FIRE" = 3,
    "EMIR" = 2,
    "MICA" = 2
)

# TODO: Vérifier automatiquement que ça colle avec le nombre de sessions
SESSION_DEBUT <- c( # Numéro de la première session pour chaque filière (utile pour celle qui ont moins du maximum de sessions)
    "FC_FIRE" = 1,
    "EMIR" = 2,
    "MICA" = 2
)

#' Function used by server to handle affectations
#'
#' @param input Input data from the frontend
#' @param output Output data the frontend will receive
#' @param df The reactive data frame of students's wishes and affectations
#' @param capacities The total capacities of the departments over 3 sessions
#' @param remaining_capacities The remaining capacities of the departments
handle_affectations <- function(input, output, df, capacities, remaining_capacities) {
    common_checks <- function(selected) {
        if (is.null(df())) {
            showNotification("Aucun fichier charg\u00E9.", type = "warning")
            return(FALSE)
        }

        if (is.null(selected)) {
            showNotification("Aucune ligne s\u00E9lectionn\u00E9e.", type = "warning")
            return(FALSE)
        }
        TRUE
    }

    wish_input <- reactiveVal(1)

    observeEvent(input$assign_depart_hard, {
        if (!common_checks(input$aff_depart_table_rows_selected)) {
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

    handle_operation <- function(operation, sessions = FALSE) {
        selected <- if (sessions) {
            selected <- input$aff_session_table_rows_selected
        } else {
            selected <- input$aff_depart_table_rows_selected
        }

        if (!common_checks(selected)) {
            return()
        }

        r <- operation(df(), selected)
        df(r$df)

        if (length(r$fails) == length(selected)) {
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
        handle_operation(function(df, selection) assign_depart_soft(df, selection, remaining_capacities()))
    })

    observeEvent(input$assign_depart_erase, {
        handle_operation(assign_depart_erase)
    })

    observeEvent(input$assign_session_auto, {
        handle_operation(function(df, selection) assign_session_auto(df, selection, capacities()), sessions = TRUE)
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
        if (is.na(df[[paste0("V", wish_number)]][i])) {
            fails <- c(fails, i)
            next()
        }
        j <- 1
        while (j <= NB_SESSIONS[df$Filiere[i]]) {
            if (is.na(df[[paste0("Aff_depart_", j)]][i])) {
                df[[paste0("Aff_depart_", j)]][i] <- df[[paste0("V", wish_number)]][i]
                break()
            } else if (df[[paste0("Aff_depart_", j)]][i] == df[[paste0("V", wish_number)]][i]) { # Don't assign the same department twice
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

#' Assign departments to students according to their wishes, looking for the first wish that is not full
#'
#' @param df The data frame with the students and their wishes
#' @param selection The indices of students to assign
#' @param capacities The remaining capacities of the departments
#' @return A list with (list: The input data frame with affected departments, fails: The indices of students that could not be assigned)
assign_depart_soft <- function(df, selection, capacities) {
    fails <- c()

    for (session in seq_len(max(NB_SESSIONS))) {
        for (student in selection) {
            if (student %in% fails) {
                next()
            }
            if (session > NB_SESSIONS[df$Filiere[student]]) {
                next()
            }
            if (!is.na(df[[paste("Aff_depart_", session, sep = "")]][student])) {
                next()
            }
            for (wish_number in 1:7) {
                wish <- df[[paste("V", wish_number, sep = "")]][student]
                if (is.na(wish)) {
                    break()
                }
                assigned <- unlist(df[student, paste0("Aff_depart_", 1:NB_SESSIONS[df$Filiere[student]])])
                if (!is.na(wish) && capacities[wish] > 0 && !(wish %in% assigned)) {
                    df[[paste0("Aff_depart_", session)]][student] <- wish
                    capacities[wish] <- capacities[wish] - 1
                    break()
                }
            }
            if (is.na(df[[paste("Aff_depart_", session, sep = "")]][student])) {
                fails <- c(fails, student)
            }
        }
    }

    list(df = df, fails = fails)
}

#' Automatically assign sessions to students based on their department affectations
#'
#' @param df The data frame with the students and affected departements
#' @param selection The indices of students to assign
#' @param capacities The total capacities of the departments over 3 sessions
#' @return A list with (list: The input data frame with affected sessions, fails: The indices of students that could not be assigned)
assign_session_auto <- function(df, selection, capacities) {
    nb_in_session <- vector("list", max(NB_SESSIONS))

    fails <- c()

    for (i in selection) { # TODO: Randomize order (but make the seed, like, the name of the first student so it's still deterministic)
        n_sessions <- NB_SESSIONS[df$Filiere[i]]
        sessions_offset <- SESSION_DEBUT[df$Filiere[i]] - 1

        # Get all the departments the student was assigned to
        depart_vec <- sapply(seq_len(n_sessions), function(j) df[[paste0("Aff_depart_", j)]][i])

        # Calculate all the permutations of those departments
        perms <- combinat::permn(depart_vec)

        works <- TRUE
        for (perm in perms) {
            works <- TRUE
            for (j in 1:n_sessions) {
                if (perm[j] %in% seq_along(nb_in_session[[j + sessions_offset]])) {
                    if (nb_in_session[[j + sessions_offset]][perm[j]] >= capacities[[perm[j]]]) {
                        works <- FALSE
                        break
                    }
                }
            }
            if (works) {
                for (j in 1:n_sessions) {
                    df[[paste0("Aff_session_", j + sessions_offset)]][i] <- perm[j]

                    # Update the department counter properly
                    if (perm[j] %in% seq_along(nb_in_session[[j + sessions_offset]])) {
                        nb_in_session[[j + sessions_offset]][perm[j]] <- nb_in_session[[j + sessions_offset]][perm[j]] + 1
                    } else {
                        nb_in_session[[j + sessions_offset]][perm[j]] <- 1
                    }
                }
                break
            }
        }
        # If the last permutation was a failure, none of them must have worked
        if (!works) { # TODO: improve this by backtracking or something, this should ideally never happen
            fails <- c(fails, i)
        }
    }

    list(df = df, fails = fails)
}

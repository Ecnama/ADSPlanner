library(combinat)

NB_SESSIONS <- c(
    "FC_FIRE" = 3,
    "EMIR" = 2,
    "MICA" = 2
)

SESSION_DEBUT <- c( # Numero de la première session pour chaque filière (utile pour celle qui ont moins du maximum de sessions)
    "FC_FIRE" = 1,
    "EMIR" = 2,
    "MICA" = 2
)


local({ # Check that the two vectors are consistent
    if (length(NB_SESSIONS) != length(SESSION_DEBUT)) {
        stop("Erreur: Le nombre de sessions n'est pas le meme entre NB_SESSIONS et SESSION_DEBUT.")
    }
    max_sessions <- max(NB_SESSIONS)
    for (i in names(NB_SESSIONS)) {
        if (SESSION_DEBUT[i] - 1 + NB_SESSIONS[i] > max_sessions) {
            stop(paste("Erreur: trop de sessions pour", i))
        }
    }
})

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
    showNotification("Calcul des affectations en cours...", type = "message")

    capacities <- capacities / 3

    nb_in_session <- vector("list", max(NB_SESSIONS))

    fails <- selection

    # Set a deterministic seed
    set.seed(sum(capacities))

    recursive_assign <- function(sel) {
        if (length(sel) <= 0) {
            return(TRUE)
        }

        i <- sel[1]

        # cat(paste(replicate((length(selection) - length(sel)), " "), df$Nom[i], " ", df$Prenom[i], "\n", sep = ""))

        n_sessions <- NB_SESSIONS[df$Filiere[i]]
        sessions_offset <- SESSION_DEBUT[df$Filiere[i]] - 1

        # Get all the departments the student was assigned to
        depart_vec <- sapply(seq_len(n_sessions), function(j) df[[paste0("Aff_depart_", j)]][i])

        if (any(is.na(depart_vec))) {
            # Give up if there's a missing department
            return(recursive_assign(sel[sel != i]))
        }

        # Calculate all the permutations of those departments
        perms <- combinat::permn(depart_vec)

        # Vector of working permutations
        working_perms <- vector("list", 0)

        # Heuristic for each working permutation
        heuristics <- c()

        for (perm in perms) {
            works <- TRUE
            for (j in 1:n_sessions) {
                if (perm[j] %in% names(nb_in_session[[j + sessions_offset]])) {
                    if (nb_in_session[[j + sessions_offset]][perm[j]] >= capacities[[perm[j]]]) {
                        works <- FALSE
                        break
                    }
                }
            }

            if (works) {
                heur <- 0
                for (d in names(capacities)) { # The point of this heuristic is to minimize the variation of the assigned sessions
                    numbers <- c()

                    for (n in seq_along(nb_in_session)) {
                        if (d %in% names(nb_in_session[[n]])) {
                            numbers <- c(numbers, nb_in_session[[n]][d])
                        } else {
                            numbers <- c(numbers, 0)
                        }
                        if (n > sessions_offset && n <= sessions_offset + n_sessions) {
                            if (perm[n - sessions_offset] == d) {
                                numbers[length(numbers)] <- numbers[length(numbers)] + 1
                            }
                        }
                    }

                    heur <- heur + stats::sd(numbers)
                }

                heuristics <- c(heuristics, heur)
                working_perms <- append(working_perms, list(perm))
            }
        }

        if (length(working_perms) == 0) {
            FALSE # No working permutation, backtracking
        } else {
            sorted_indices <- order(heuristics)

            for (id in sorted_indices) { # Try again and again in heuristic order until we get to the end of the tree
                for (j in 1:n_sessions) {
                    df[[paste0("Aff_session_", j + sessions_offset)]][i] <<- working_perms[[id]][j]
                    #print(paste("Affectation de", df$Nom[i], df$Prenom[i], "au departement", working_perms[[id]][j], "en session", j + sessions_offset))

                    # Update the department counter properly
                    if (working_perms[[id]][j] %in% names(nb_in_session[[j + sessions_offset]])) {
                        nb_in_session[[j + sessions_offset]][working_perms[[id]][j]] <<- nb_in_session[[j + sessions_offset]][working_perms[[id]][j]] + 1
                    } else {
                        nb_in_session[[j + sessions_offset]][working_perms[[id]][j]] <<- 1
                    }
                }

                # If we assigned, then it's not a fail
                fails <<- fails[fails != i]

                # print("worked")

                # Recursive call
                if (recursive_assign(sel[sel != i])) {
                    return(TRUE) # If we get to the end of the tree, then we can return TRUE
                }

                # print("nevermind")
            }

            FALSE # Nothing worked, backtracking
        }
    }

    recursive_assign(sample(selection, length(selection), replace = FALSE))

    list(df = df, fails = fails)
}

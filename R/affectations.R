library(combinat)

source("R/config.R")
source("R/tables.R")
source("R/capacities.R")

AFF_SESSIONS_MAX_TIME <- 20 # Maximum time to assign sessions before stopping the process (in seconds)

local({ # Check that the two vectors are consistent
    if (length(NB_SESSIONS) != length(SESSION_DEBUT)) {
        stop("Erreur: Le nombre de sessions n'est pas le m\U00EAme entre NB_SESSIONS et SESSION_DEBUT.")
    }
    max_sessions <- max(NB_SESSIONS)
    if (any(SESSION_DEBUT - 1 + NB_SESSIONS > max_sessions)) {
        stop(paste("Erreur: trop de sessions pour", names(which(SESSION_DEBUT - 1 + NB_SESSIONS > max_sessions))))
    }
})

#' Function used by server to handle affectations
#'
#' @param input Input data from the frontend
#' @param output Output data the frontend will receive
#' @param df The reactive data frame of students's wishes and affectations
#' @param capacities The total capacities of the departments over 3 sessions
#' @param remaining_depart_capacities The remaining capacities of the departments
#' @param remaining_session_capacities The remaining capacities of the sessions
handle_affectations <- function(input, output, df, capacities, remaining_depart_capacities, remaining_session_capacities) {
    common_checks <- function(table) {
        if (is.null(df())) {
            showNotification("Aucun fichier charg\u00E9.", type = "warning")
            return(FALSE)
        }

        if (is.null(get_selection(df(), table, input))) {
            showNotification("Aucune ligne s\u00E9lectionn\u00E9e.", type = "warning")
            return(FALSE)
        }
        TRUE
    }

    wish_input <- reactiveVal(1)

    observeEvent(input$assign_depart_hard, {
        if (!common_checks("aff_depart")) {
            return()
        }

        showModal(modalDialog(
            title = "Affectation dure",
            numericInput("wish_selection", "Num\u00E9ro de v\u0153u", value = wish_input(), min = 1, max = 7),
            footer = tagList(
                modalButton("Annuler"),
                actionButton("confirm_assign_depart_hard", "Confirmer")
            )
        ))
    })

    old_department_input <- reactiveVal(NA)
    new_department_input <- reactiveVal(NA)

    observeEvent(input$assign_depart_targeted, {
        if (!common_checks("aff_depart")) {
            return()
        }

        showModal(modalDialog(
            title = "Affectation cibl\u00E9e",
            tagList(
                selectInput("old_department", "D\u00E9partement actuel :",
                    choices = names(capacities()), selected = old_department_input(), selectize = FALSE
                ),
                selectInput("new_department", "Nouveau d\u00E9partement :",
                    choices = names(capacities()), selected = new_department_input(), selectize = FALSE
                )
            ),
            footer = tagList(
                modalButton("Annuler"),
                actionButton("confirm_assign_targeted", "Confirmer")
            )
        ))
    })

    manual_department_input <- reactiveVal(NA)
    manual_session_input <- reactiveVal(1)

    observeEvent(input$assign_session_manual, {
        if (!common_checks("aff_session")) {
            return()
        }

        showModal(modalDialog(
            title = "Affectation manuelle",
            tagList(
                selectInput("manual_department", "D\u00E9partement :",
                    choices = c(names(capacities()), "Aucun"), selected = manual_department_input(), selectize = FALSE
                ),
                numericInput("manual_session", "Num\u00E9ro de session", value = manual_session_input(), min = 1, max = max(NB_SESSIONS))
            ),
            footer = tagList(
                modalButton("Annuler"),
                actionButton("confirm_assign_session_manual", "Confirmer")
            )
        ))
    })

    handle_operation <- function(operation, sessions = FALSE) {
        table <- if (sessions) {
            "aff_session"
        } else {
            "aff_depart"
        }

        if (!common_checks(table)) {
            return()
        }

        selected <- get_selection(df(), table, input)

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
                            df()[r$fails, ]$Nom, df()[r$fails, ]$Prenom,
                            sep = " "
                        ),
                        collapse = ", "
                    ), ".",
                    sep = ""
                ),
                type = "warning"
            )
        } else {
            showNotification("Op\u00E9ration r\u00E9alis\u00E9e.", type = "message")
        }
    }

    observeEvent(input$confirm_assign_depart_hard, {
        if (is.na(input$wish_selection)) {
            showNotification("Aucun v\u0153u s\u00E9lectionn\u00E9.", type = "warning")
            return()
        }

        wish_input(input$wish_selection)
        removeModal()

        handle_operation(function(df, selection) assign_depart_hard(df, selection, wish_input()))
    })

    observeEvent(input$confirm_assign_targeted, {
        old_department_input(input$old_department)
        new_department_input(input$new_department)
        removeModal()

        handle_operation(function(df, selection) targeted_affectation(df, selection, old_department_input(), new_department_input()))
    })

    observeEvent(input$assign_depart_real, {
        handle_operation(function(df, selection) assign_depart_soft(df, selection, remaining_depart_capacities()))
    })

    observeEvent(input$assign_depart_erase, {
        handle_operation(assign_erase)
    })

    observeEvent(input$assign_session_erase, {
        handle_operation(function(df, selection) assign_erase(df, selection, sessions = TRUE), sessions = TRUE)
    })

    observeEvent(input$assign_session_auto, {
        negatives <- remaining_depart_capacities()
        negatives <- negatives[negatives < 0]
        if (length(negatives) > 0) {
            showNotification(
                paste("Impossible d'affecter les sessions, les d\u00E9partements suivants sont surbook\u00E9s : ", paste(names(negatives), collapse = ", ")),
                type = "warning"
            )
            return()
        }

        handle_operation(function(df, selection) assign_session_auto(df, selection, capacities()), sessions = TRUE)
    })

    observeEvent(input$confirm_assign_session_manual, {
        if (is.na(input$manual_session)) {
            showNotification("Aucune session s\u00E9lectionn\u00E9e.", type = "warning")
            return()
        }

        manual_department_input(input$manual_department)
        manual_session_input(input$manual_session)

        removeModal()
        handle_operation(function(df, selection) assign_session_manual(df, selection, manual_department_input(), manual_session_input()), sessions = TRUE)
    })
}

#' Erase all affected departments
#'
#' @param df The data frame with the students and their wishes
#' @param selection The indices of students to erase
#' @param sessions Whether to erase affected sessions or departments
#' @return A list with (list: The input data frame with erased departments, fails: An empty vector because this can never fail)
assign_erase <- function(df, selection, sessions = FALSE) {
    table <- if (sessions) {
        "session"
    } else {
        "depart"
    }

    for (i in seq_len(max(NB_SESSIONS))) {
        df[[paste0("Aff_", table, "_", i)]][selection] <- NA_character_
    }

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

#' Assign certain students to a certain departement, deleting it from a certain departement
#' If the student is not assigned to the old department, it will still be assigned to the new department
#'
#' @param df The data frame with the students and their wishes
#' @param selection The indices of students to assign
#' @param old_depart The departement the students were assigned
#' @param new_depart The departement to which the students will be assigned
#' @return The input data frame with affected departments
targeted_affectation <- function(df, selection, old_depart, new_depart) {
    fails <- c()
    for (i in selection) {
        if (any(df[i, paste("Aff_depart_", 1:NB_SESSIONS[df$Filiere[i]], sep = "")] == new_depart)) {
            next()
        }
        success <- FALSE
        for (j in 1:NB_SESSIONS[df$Filiere[i]]) {
            col_name <- paste("Aff_depart_", j, sep = "")
            current_val <- df[[col_name]][i]
            if (is.na(current_val) || current_val == old_depart) {
                df[[col_name]][i] <- new_depart
                success <- TRUE
                break()
            }
        }
        if (!success) {
            fails <- c(fails, i)
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

    # Erase selected affectations before assigning new ones
    erased <- assign_erase(df, selection, sessions = TRUE)
    df <- erased$df
    cap <- calculate_session_capacities(df, capacities)

    # Everything failed by default, when we assign students we'll remove them
    fails <- selection

    # Set a deterministic seed
    set.seed(sum(cap))

    calculate_heuristic <- function(perm, n_sessions, sessions_offset) {
        heur <- 0
        for (d in names(cap)) { # The point of this heuristic is to minimize the variation of the assigned sessions
            numbers <- c()

            for (n in seq_len(nrow(cap))) {
                numbers <- c(numbers, cap[n, d])

                # Count numbers after the perm is applied
                if (n > sessions_offset && n <= sessions_offset + n_sessions) {
                    if (perm[n - sessions_offset] == d) {
                        numbers[length(numbers)] <- numbers[length(numbers)] - 1
                    }
                }
            }

            heur <- heur + stats::sd(numbers)
        }
        heur
    }

    start_time <- Sys.time()
    outatime <- FALSE

    recursive_assign <- function(sel) {
        if (length(sel) <= 0) {
            return(TRUE)
        }

        # If the function takes too long, stop it
        if (Sys.time() - start_time > AFF_SESSIONS_MAX_TIME) {
            # Reset all affectations that may have happened recursively but were in other branches of the tree
            erased <- assign_erase(df, sel, sessions = TRUE)
            df <<- erased$df
            outatime <<- TRUE
            return(FALSE)
        }

        i <- sel[1]

        # cat(paste0(strrep(" ", length(selection) - length(sel)), df$Nom[i], " ", df$Prenom[i], "\n"))

        n_sessions <- NB_SESSIONS[df$Filiere[i]]
        sessions_offset <- SESSION_DEBUT[df$Filiere[i]] - 1

        # Get all the departments the student was assigned to
        depart_vec <- sapply(seq_len(n_sessions), function(j) df[[paste0("Aff_depart_", j)]][i])

        # Give up on this student if they have a missing department
        if (any(is.na(depart_vec))) {
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

            # Check if the permutation respects the capacities
            for (j in seq_len(n_sessions)) {
                if (cap[j + sessions_offset, perm[j]] <= 0) {
                    works <- FALSE
                    break
                }
            }

            if (works) {
                heuristics <- c(heuristics, calculate_heuristic(perm, n_sessions, sessions_offset))
                working_perms <- append(working_perms, list(perm))
            }
        }

        if (length(working_perms) == 0) {
            FALSE # No working permutation, backtracking
        } else {
            # print(cap)

            sorted_indices <- order(heuristics)

            for (id in sorted_indices) { # Try again and again in heuristic order until we get to the end of the tree
                cap_backup <- cap
                for (j in seq_len(n_sessions)) {
                    df[[paste0("Aff_session_", j + sessions_offset)]][i] <<- working_perms[[id]][j]
                    # print(paste("Affectation de", df$Nom[i], df$Prenom[i], "au departement", working_perms[[id]][j], "en session", j + sessions_offset))

                    # Update the department counter properly
                    cap[j + sessions_offset, working_perms[[id]][j]] <<- cap[j + sessions_offset, working_perms[[id]][j]] - 1
                }

                # If we assigned, then it's not a fail
                fails <<- fails[fails != i]

                # Recursive call
                if (recursive_assign(sel[sel != i])) {
                    return(TRUE) # If we get to the end of the tree, then we can return TRUE
                }

                # Didn't work, rollback the changes
                cap <<- cap_backup

                if (outatime) {
                    return(FALSE)
                }
            }

            FALSE # Nothing worked, backtracking
        }
    }

    if (!recursive_assign(sample(selection, length(selection), replace = FALSE))) {
        error_text <- "Les capacit\u00E9s pourraient ne pas \U00EAtre suffisantes pour ces contraintes."
        if (outatime) {
            error_text <- paste("Calcul des affections stopp\u00E9, il prenait trop de temps.", error_text)
        }
        showNotification(error_text, type = "warning")
    }

    list(df = df, fails = fails)
}

#' Manually assign a department to students' sessions
#'
#' @param df The data frame with the students and affected departements
#' @param selection The indices of students to assign
#' @param depart The department to assign
#' @param session The session to assign it to
#' @return A list with (list: The input data frame with affected sessions, fails: The indices of students that could not be assigned)
assign_session_manual <- function(df, selection, depart, session) {
    fails <- c()

    for (i in selection) {
        filiere <- df$Filiere[i]

        if (depart == "Aucun") {
            df[[paste0("Aff_session_", session)]][i] <- NA_character_
            next()
        }

        # Don't assign a department to a session if the student is not in that session
        if (session < SESSION_DEBUT[filiere] || session > NB_SESSIONS[filiere] + SESSION_DEBUT[filiere] - 1) {
            fails <- c(fails, i)
            next()
        }

        # Check if the department is already assigned to another session, if so, move it
        previous_session <- match(depart, df[i, paste0("Aff_session_", SESSION_DEBUT[filiere]:(NB_SESSIONS[filiere] + SESSION_DEBUT[filiere] - 1))])
        if (!is.na(previous_session)) {
            df[[paste0("Aff_session_", previous_session)]][i] <- NA_character_
        }

        df[[paste0("Aff_session_", session)]][i] <- depart
    }

    list(df = df, fails = fails)
}

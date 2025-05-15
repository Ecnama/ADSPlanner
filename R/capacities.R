source("R/config.R")

#' Handle capacities on server
#'
#' @param input Input data from the frontend
#' @param output Output data the frontend will receive
#' @param df The reactive data frame of students's wishes and affectations
#' @param capacities The reactive table of each departments capacities
#' @param remaining_depart_capacities The reactive table of the remaining capacities in each department
#' @param remaining_session_capacities The reactive table of the remaining capacities in each session
handle_capacities <- function(input, output, df, capacities, remaining_depart_capacities, remaining_session_capacities) {
    observe({
        if (!is.null(df())) {
            remaining_depart_capacities(calculate_depart_capacities(df(), capacities()))
            remaining_session_capacities(calculate_session_capacities(df(), capacities()))
        }
    })

    observe({
        if (!is.null(input$file)) {
            nb_sessions <- max(NB_SESSIONS)
            capacities(c(
                "EII" = input$capacity_EII * nb_sessions,
                "E&T" = input$capacity_ET * nb_sessions,
                "INFO" = input$capacity_INFO * nb_sessions,
                "MA" = input$capacity_MA * nb_sessions,
                "GCU" = input$capacity_GCU * nb_sessions,
                "GMA" = input$capacity_GMA * nb_sessions,
                "GPM" = input$capacity_GPM * nb_sessions
            ))
            remaining_depart_capacities(calculate_depart_capacities(df(), capacities()))
            remaining_session_capacities(calculate_session_capacities(df(), capacities()))
        }
    })

    output$depart_capacities_counter <- renderUI({
        # Récupérer les capacités restantes
        remaining <- remaining_depart_capacities()

        # Vérifier qu'un fichier est ouvert
        if (is.null(remaining)) {
            return(NULL)
        }

        # Générer un tableau HTML transposé
        html <- tags$table(
            style = "width: 100%; border-collapse: collapse; float: right; table-layout: fixed;",
            tags$thead(
                tags$tr(
                    lapply(names(remaining), function(department) {
                        tags$th(department, style = "padding: 3px; text-align: center;")
                    })
                )
            ),
            tags$tbody(
                tags$tr(
                    lapply(as.numeric(remaining), function(capacity) {
                        tags$td(
                            capacity,
                            style = paste0(
                                "padding: 3px; text-align: center;",
                                if (capacity < 0) "color: red; font-weight: bold;" else ""
                            )
                        )
                    })
                )
            )
        )

        # Retourner le tableau HTML
        tagList(
            br(),
            "Capacit\u00E9s restantes :",
            html,
            if (any(is.na(remaining))) {
                "Les capacit\u00E9s contiennent des valeurs manquantes."
            } else if (any(as.numeric(as.matrix(remaining)) < 0)) {
                "La capacit\u00E9 d'un d\u00E9partement est d\u00E9pass\u00E9e : changez de m\u00E9thode d'affectation."
            } else {
                ""
            }
        )
    })

    output$session_capacities_counter <- renderUI({
        # Récupérer les capacités restantes, attendues sous forme de data frame avec 3 lignes pour 3 sessions
        remaining <- remaining_session_capacities()

        # Vérifier qu'un fichier est ouvert
        if (is.null(remaining)) {
            return(NULL)
        }

        # Générer un tableau HTML transposé avec une colonne pour la session
        html <- tags$table(
            style = "width: 100%; border-collapse: collapse; float: right; table-layout: fixed;",
            tags$thead(
                tags$tr(
                    tags$th(),
                    lapply(colnames(remaining), function(department) {
                        tags$th(department, style = "padding: 3px; text-align: center;")
                    })
                )
            ),
            tags$tbody(
                lapply(seq_len(nrow(remaining)), function(i) {
                    tags$tr(
                        tagList(
                            tags$td(paste0("Session ", i), style = "padding: 3px; text-align: center; font-weight: bold;"),
                            lapply(remaining[i, ], function(capacity) {
                                tags$td(
                                    capacity,
                                    style = paste0(
                                        "padding: 3px; text-align: center;",
                                        if (capacity < 0) "color: red; font-weight: bold;" else ""
                                    )
                                )
                            })
                        )
                    )
                })
            )
        )

        # Retourner le tableau HTML
        tagList(
            br(),
            "Capacit\u00E9s restantes :",
            html,
            if (any(is.na(remaining))) {
                "Les capacit\u00E9s contiennent des valeurs manquantes."
            } else if (any(as.numeric(as.matrix(remaining)) < 0)) {
                "La capacit\u00E9 d'un d\u00E9partement est d\u00E9pass\u00E9e : changez de m\u00E9thode d'affectation."
            } else {
                ""
            }
        )
    })
}

#' Calculate the new department capacities
#'
#' @param df The reactive data frame of students's wishes and affectations
#' @param capacities The reactive table of each departments capacities
calculate_depart_capacities <- function(df, capacities) {
    result <- capacities

    for (dep in names(capacities)) {
        nb_students_affected <- sum(df$Aff_depart_1 == dep, na.rm = TRUE) +
            sum(df$Aff_depart_2 == dep, na.rm = TRUE) +
            sum(df$Aff_depart_3 == dep, na.rm = TRUE)
        result[dep] <- capacities[dep] - nb_students_affected
    }

    result
}

#' Calculate the new department capacities for each session
#'
#' @param df The reactive data frame of students's wishes and affectations
#' @param capacities The reactive table of each departments capacities
calculate_session_capacities <- function(df, capacities) {
    capacities <- capacities / max(NB_SESSIONS)
    result <- data.frame(matrix(ncol = 0, nrow = max(NB_SESSIONS)))

    for (dep in names(capacities)) {
        cap <- c()
        for (session in 1:max(NB_SESSIONS)) {
            cap <- c(cap, capacities[dep] - sum(df[paste0("Aff_session_", session)] == dep, na.rm = TRUE))
        }
        result[[dep]] <- cap
    }

    result
}

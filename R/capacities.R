#' Handle capacities on server
#'
#' @param input Input data from the frontend
#' @param output Output data the frontend will receive
#' @param df The reactive data frame of students's wishes and affectations
#' @param capacities The reactive table of each departments capacities
#' @param remaining_capacities The reactive table of the remaining capacities in each department
handle_capacities <- function(input, output, df, capacities, remaining_capacities) {
    observe({
        if (!is.null(df())) {
            remaining_capacities(calculate_new_capacities(df(), capacities()))
        }
    })

    observe({
        if (!is.null(input$file)) {
            capacities(c(
                "EII" = input$capacity_EII * 3,
                "E&T" = input$capacity_ET * 3,
                "INFO" = input$capacity_INFO * 3,
                "MA" = input$capacity_MA * 3,
                "GCU" = input$capacity_GCU * 3,
                "GMA" = input$capacity_GMA * 3,
                "GPM" = input$capacity_GPM * 3
            ))
            remaining_capacities(capacities())
            remaining_capacities(calculate_new_capacities(df(), capacities()))
        }
    })

    output$capacity_full <- renderText({
        if (is.null(remaining_capacities()) || length(remaining_capacities()) == 0) {
            return("")
        }
        if (any(is.na(remaining_capacities()))) {
            print(remaining_capacities())
            return("Les capacités contiennent des valeurs manquantes.")
        }
        if (any(as.numeric(remaining_capacities()) < 0)) {
            return("La capacit\u00E9 d'un d\u00E9partement est d\u00E9pass\u00E9e : changez de m\u00E9thode d'affectation.")
        } else {
            return("")
        }
    })

    output$capacities_counters <- renderUI({
        # Récupérer les capacités restantes
        remaining <- remaining_capacities()
        # Vérifier si les capacités sont valides
        if (is.null(remaining) || length(remaining) == 0 || any(is.na(remaining))) {
            print(remaining)
            return(NULL)
        }
        # Créer un data frame pour les départements et leurs capacités
        capacities_df <- data.frame(
            Department = names(remaining),
            Capacity = as.numeric(remaining)
        )
        # Générer un tableau HTML transposé
        html <- tags$table(
            style = "width: 40%; border-collapse: collapse; float: right; table-layout: fixed;",
            tags$thead(
                tags$tr(
                    lapply(capacities_df$Department, function(department) {
                        tags$th(department, style = "border: 1px solid black; padding: 5px; text-align: center;")
                    })
                )
            ),
            tags$tbody(
                tags$tr(
                    lapply(capacities_df$Capacity, function(capacity) {
                        tags$td(
                            capacity,
                            style = paste0(
                                "border: 1px solid black; padding: 5px; text-align: center;",
                                if (capacity < 0) "color: red; font-weight: bold;" else ""
                            )
                        )
                    })
                )
            )
        )
        # Retourner le tableau HTML
        html
    })
}


#' Calculate the new capacities avec each changes of the df
#'
#' @param df The reactive data frame of students's wishes and affectations
#' @param capacities The reactive table of each departments capacities
calculate_new_capacities <- function(df, capacities) {
    result <- capacities
    for (dep in names(capacities)) {

        nb_students_affected <- sum(df$Aff_depart_1 == dep, na.rm = TRUE) +
            sum(df$Aff_depart_2 == dep, na.rm = TRUE) +
            sum(df$Aff_depart_3 == dep, na.rm = TRUE)
        result[dep] <- capacities[dep] - nb_students_affected
    }
    result
}

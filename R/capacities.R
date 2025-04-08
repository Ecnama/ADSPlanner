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
            updated_df <- df()
            remaining_capacities(calculate_new_capacities(updated_df, capacities))
        }
    })
    # gerer l'affichage renderTable
}



#' Calculate the new capacities avec each changes of the df
#'
#' @param df The reactive data frame of students's wishes and affectations
#' @param capacities The reactive table of each departments capacities
calculate_new_capacities <- function(df, capacities) {
    result <- capacities
    for (dep in names(capacities)) {
        nb_students_affected <- sum(df[Aff_depart_1 == dep], na.rm = TRUE)
        + sum(df[Aff_depart_2 == dep], na.rm = TRUE)
        + sum(df[Aff_depart_3 == dep], na.rm = TRUE)
        result[dep] <- capacities[dep] - nb_students_affected
    }
    result
}

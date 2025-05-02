library(openxlsx)

#' Output data to an Excel file
#'
#' @param df The data frame to write
#' @param capacities The capacities of each department
#' @param file The path to the file to write
write_output <- function(df, capacities, file) {
    wb <- openxlsx::createWorkbook()

    openxlsx::addWorksheet(wb, "ADSPlanner_affectations")
    openxlsx::addStyle(wb, "ADSPlanner_affectations", cols = 15:17, rows = 1:(nrow(df)), style = openxlsx::createStyle(textDecoration = "bold"), gridExpand = TRUE) # nolint: seq_linter.
    openxlsx::writeData(wb, "ADSPlanner_affectations", df)

    cdf <- data.frame(
        Departement = names(capacities),
        Capacite = capacities / 3
    )

    openxlsx::addWorksheet(wb, "ADSPlanner_capacites")
    openxlsx::writeData(wb, "ADSPlanner_capacites", cdf)

    openxlsx::saveWorkbook(wb, file)
}

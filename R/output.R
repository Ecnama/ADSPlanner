library(openxlsx)

#' Output data to an Excel file
#'
#' @param df The data frame to write
#' @param capacities The capacities of each department
#' @param file The path to the file to write
write_output <- function(df, capacities, file) {
    showNotification("G\u00E9n\u00E9ration du fichier Excel...", type = "message")

    wb <- openxlsx::createWorkbook()

    openxlsx::addWorksheet(wb, "ADSPlanner_affectations")
    openxlsx::writeData(wb, "ADSPlanner_affectations", df)
    color_session_cells(wb, df)

    cdf <- data.frame(
        Departement = names(capacities),
        Capacite = capacities / 3
    )

    openxlsx::addWorksheet(wb, "ADSPlanner_capacites")
    openxlsx::writeData(wb, "ADSPlanner_capacites", cdf)

    openxlsx::saveWorkbook(wb, file)
}

#' Color the cells of the session columns based on the wishes
#'
#' @param wb The workbook that will be exported
#' @param df The main data frame
color_session_cells <- function(wb, df) {
    green_style <- openxlsx::createStyle(fgFill = "#b2ffb2", textDecoration = "bold")
    yellow_style <- openxlsx::createStyle(fgFill = "#ffffb7", textDecoration = "bold")
    orange_style <- openxlsx::createStyle(fgFill = "#ffdba5", textDecoration = "bold")
    red_style <- openxlsx::createStyle(fgFill = "#ffa5b7", textDecoration = "bold")

    session_cols <- grep("Aff_session_", names(df))
    aff_session1_idx <- which(names(df) == "Aff_session_1")
    v1_idx <- which(names(df) == "V1")

    for (i in seq_len(nrow(df))) {
        wishes <- c()
        j <- v1_idx
        while (j <= length(names(df)) && grepl("V", names(df)[j])) {
            if (!is.na(df[i, j])) {
                wishes <- c(wishes, df[i, j])
            }
            j <- j + 1
        }

        j <- aff_session1_idx - 1
        for (col in session_cols) {
            j <- j + 1
            if (is.na(df[i, col])) {
                next
            }

            index <- match(df[i, col], wishes)[1]
            cell_style <- openxlsx::createStyle(textDecoration = "bold")

            if (is.na(index)) {
                cell_style <- red_style
            } else {
                if (index <= 3) {
                    cell_style <- green_style
                } else if (index <= 4) {
                    cell_style <- yellow_style
                } else if (index <= 5) {
                    cell_style <- orange_style
                } else if (index <= 6) {
                    cell_style <- red_style
                }
            }
            openxlsx::addStyle(wb, "ADSPlanner_affectations", cols = j, rows = i + 1, style = cell_style)
        }
    }
}

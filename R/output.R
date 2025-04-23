library(openxlsx)

#' Output data to an Excel file
#'
#' @param df The data frame to write
#' @param file The path to the file to write
write_output <- function(df, file) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "ADSPlanner")
    openxlsx::addStyle(wb, "ADSPlanner", cols = 15:17, rows = 1:(nrow(df)), style = openxlsx::createStyle(textDecoration = "bold"), gridExpand = TRUE) # nolint: seq_linter.
    openxlsx::writeData(wb, "ADSPlanner", df)
    openxlsx::saveWorkbook(wb, file)
}

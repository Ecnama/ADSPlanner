library(openxlsx)

write_output <- function(df, file) {
    df <- df[, !grepl("^Aff_depart_", names(df))]
    openxlsx::write.xlsx(df, file)
}

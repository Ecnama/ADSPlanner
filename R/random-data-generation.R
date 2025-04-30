library(openxlsx)

# Lists of first names/last names/sector to generate fake students
sample_first_name <- c("Hector", "Mari", "Amance", "Peter", "Nathalie", "Agathe", "Romain", "Eve", "Marc", "Enzo")
sample_last_name <- c("Dupont", "Nguyen", "Martin", "Garcia", "Fouquier", "Dubois", "Jack", "Boisu", "Zaky", "Mathy")
sector <- c(rep("1 : FC_FIRE (Filiere classique ou filiere internationale)", 140), rep("2 : EMIR", 30), rep("3 : MICA", 30))

# Lists of available answers for each sector
classic_answer <- c("EII", "MA", "INFO", "E&T", "GPM", "GMA", "GCU")
emir_answer <- c("EII", "INFO", "GPM", "E&T")
mica_answer <- c("GCU", "MA", "GMA", "INFO")

column_choice <- c(
    paste0("Q02_Voeux->", classic_answer),
    paste0("Q03_VoeuxEMIR->", emir_answer),
    paste0("Q04_voeuxMICA->", mica_answer)
)

# Quantity of students to generate
students_quantity <- 200

#' Generates student's wishs in fonction of their sector (MICA/EMIR/CLASSIQUE)
#'
#' @param sector "CLASSIQUE", "EMIR" ou "MICA".
#' @return a vector with attributed wishes
wishes_generation <- function(sector) {
    result <- rep(NA, length(column_choice))
    names(result) <- column_choice
    if (sector == "1 : FC_FIRE (Filiere classique ou filiere internationale)") {
        ranking <- sample(classic_answer)
        result[paste0("Q02_Voeux->", ranking)] <- 1:7
    } else if (sector == "2 : EMIR") {
        ranking <- sample(emir_answer)
        result[paste0("Q03_VoeuxEMIR->", ranking)] <- 1:4
    } else if (sector == "3 : MICA") {
        ranking <- sample(mica_answer)
        result[paste0("Q04_voeuxMICA->", ranking)] <- 1:4
    }
    result
}

#' Applies wishes_generation to each line of the dataframe of the students, and adds the generated wishes in the dataframe
#' fusion the dataframes to create the final one
#'
#' @return a dataframe with the wishes
random_data <- function() {
    df <- data.frame(
        "Nom complet" = paste(sample(sample_first_name, students_quantity, replace = TRUE), sample(sample_last_name, students_quantity, replace = TRUE)),
        "Classement" = sample(1:students_quantity),
        "Q01_Filiere" = sample(sector, students_quantity, replace = TRUE),
        check.names = FALSE
    )
    df_wishes <- t(apply(df, 1, function(row) {
        wishes_generation(row[["Q01_Filiere"]])
    }))
    df_wishes <- as.data.frame(df_wishes)
    df_final <- cbind(df, df_wishes)

    write.xlsx(df_final, "tests/data/random_data.xlsx")
}

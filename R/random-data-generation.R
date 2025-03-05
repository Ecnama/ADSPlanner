library(openxlsx)

library(roxygen2)

#' Lists of first names/last names/sector to generate fake students
#'
#' @format a vector with first names / last names / sectors
sample_first_name <- c("Hector","Mari","Amance","Peter","Nathalie","Agathe","Romain","Eve","Marc","Enzo")
sample_last_name <- c("Dupont","Nguyen","Martin","Garcia","Fouquier","Dubois","Jack","Boisu","Zaky","Mathy")
sector <- c(rep("EMIR",30),rep("MICA",30),rep("CLASSIQUE",140))

#' lists of available answers for each sector
#'
#' @format a vector with the answers
classic_answer <- c("EII","MA","INFO","ET","GPM","GMA","GCU")
emir_answer <- c("EII","INFO","GPM","ET")
mica_answer <- c("GCU","MA","GMA","INFO")

column_choice <- c(
  paste0("Q02_Voeux->", classic_answer),
  paste0("Q03_VoeuxEMIR->", emir_answer),
  paste0("Q04_voeuxMICA->", mica_answer)
)

#' quantity of students to generate
#' @format integer
students_quantity <- 200

#' generates a dataframe of the students with random informations (names/lastnames/sector)
#'
#' @return dataframe with student's infos
df <- data.frame(
  first_name = sample(sample_first_name, students_quantity, replace = TRUE),
  last_name = sample(sample_last_name, students_quantity, replace = TRUE),
  sector = sample(sector, students_quantity, replace = TRUE)
)

#' Generates student's wishs in fonction of their sector (MICA/EMIR/CLASSIQUE)
#' 
#'
#' @param sector "CLASSIQUE", "EMIR" ou "MICA".
#' @return a vector with attributed wishes
wishes_generation <- function(sector) {
  result <- rep(NA, length(column_choice))
  names(result) <- column_choice
  
  if (sector == "CLASSIQUE") {
      ranking <- sample(classic_answer)
      result[paste0("Q02_Voeux->", ranking)] <- 1:7
  } else if (sector == "EMIR") {
      ranking <- sample(emir_answer)
      result[paste0("Q03_VoeuxEMIR->", ranking)] <- 1:4
  } else if (sector == "MICA") {
      ranking <- sample(mica_answer)
      result[paste0("Q04_voeuxMICA->", ranking)] <- 1:4
  } 
  return(result)
}

#' adds the generated wishes to the dataframe
#'
#' applies wishes_generation to each line of the dataframe of the students, and adds the generated wishes in the dataframe
#'
#' @param df dataframe with the student's infos
#' @return a dataframe with the wishes
df_wishes <- t(apply(df, 1, function(row) {
    wishes_generation(row["sector"])
}))
df_wishes <- as.data.frame(df_wishes)

#' fusion the dataframes to create the final one
#'
#' @param df dataframe with the student's infos
#' @param df_wishes dataframe with the wishes added to the student's infos
df_final <- cbind(df, df_wishes)

#' saves the dataframe in an excel file
#'
write.xlsx(df_final,"resultatfinal.xlsx")
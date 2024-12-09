library(openxlsx)

prenoms <- c("Hector","Mari","Amance","Peter","Nathalie","Agathe","Romain","Eve","Marc","Enzo")
noms <- c("Dupont","Nguyen","Martin","Garcia","Fouquier","Dubois","Jack","Boisu","Zaky","Mathy")
filiere <- c(rep("EMIR",30),rep("MICA",30),rep("CLASSIQUE",140))

#réponses possibles
reponse_classique <- c("EII","MA","INFO","ET","GPM","GMA","GCU")
reponse_emir <- c("EII","INFO","GPM","ET")
reponse_mica <- c("GCU","MA","GMA","INFO")

choix_colonnes <- c(
  paste0("Q02_Voeux->", reponse_classique),
  paste0("Q03_VoeuxEMIR->", reponse_emir),
  paste0("Q04_voeuxMICA->", reponse_mica)
)

nb_etudiants <- 200

df <- data.frame(
  prenom = sample(prenoms, nb_etudiants, replace = TRUE),
  nom = sample(noms, nb_etudiants, replace = TRUE),
  filiere = sample(filiere, nb_etudiants, replace = TRUE)
)

generer_voeux <- function(filiere) {
  result <- rep(NA, length(choix_colonnes))
  names(result) <- choix_colonnes
  
  if (filiere == "CLASSIQUE") {
    classement <- sample(reponse_classique)
    result[paste0("Q02_Voeux->", classement)] <- 1:7
  } else if (filiere == "EMIR") {
    classement <- sample(reponse_emir)
    result[paste0("Q03_VoeuxEMIR->", classement)] <- 1:4
  } else if (filiere == "MICA") {
    classement <- sample(reponse_mica)
    result[paste0("Q04_voeuxMICA->", classement)] <- 1:4
  }
  return(result)
}

# ajout des voeux
df_voeux <- t(apply(df, 1, function(row) {
  generer_voeux(row["filiere"])
}))
df_voeux <- as.data.frame(df_voeux)


# fusion
df_final <- cbind(df, df_voeux)

# affichage
write.xlsx(df_final,"resultatfinal.xlsx")
library(openxlsx)
library(testthat)
library(dplyr)

# Source le script d'affectation
source("targeted-affectation.R")

source("random-data-generation.R")  # Ton script qui appelle final_dataframe(df)


# 1. Génération ou chargement des données
# (optionnel : tu peux aussi sourcer le générateur et appeler la fonction)
df <- read.xlsx("resultatfinal.xlsx")

# 2. Choisir un étudiant au hasard (ou spécifique)
first <- df$first_name[1]
last <- df$last_name[1]

# 3. Récupère les colonnes de vœux du bon secteur
student_sector <- df$sector[1]
wish_cols <- if (student_sector == "CLASSIQUE") {
    grep("^Q02_Voeux->", names(df), value = TRUE)
} else if (student_sector == "EMIR") {
    grep("^Q03_VoeuxEMIR->", names(df), value = TRUE)
} else {
    grep("^Q04_voeuxMICA->", names(df), value = TRUE)
}

# 4. Cherche un numéro de vœu valide
wish_numbers <- df[1, wish_cols]
valid_wish <- which(!is.na(wish_numbers))[1]

test_that("assign_targeted_chosen affecte correctement le voeu", {
    session <- 1
    df_new <- assign_targeted_chosen(df, first, last, valid_wish, session)
    
    # Vérifie que la colonne Session_1 a bien été créée
    expect_true(paste0("Session_", session) %in% names(df_new))
    
    # Vérifie que l'affectation a bien été faite
    expected_value <- sub("^Q[0-9]+_Voeux(EMIR|MICA)?->", "", wish_cols[valid_wish])
    expect_equal(df_new[df_new$first_name == first & df_new$last_name == last, paste0("Session_", session)], expected_value)
})

#targeted affectation

#' changes a student's affection depending on a wish number and on a session number
#'
#' @param df the dataframe
#' @param first_name the student's first name
#' @param last_name the student's last name
#' @param wish_number the wish number that will be affected
#' @param session the session on which the student will have the wish affected
#'
#' @return The file as a data frame
assign_targeted_chosen <- function(df, first_name, last_name, wish_number, session) {

    student_index <- which(df$first_name == first_name & df$last_name == last_name)
    if (length(student_index) == 0) {
        stop("Erreur : l'étudiant n'a pas été trouvé dans le dataframe.")
    }

    # verifies the student's departement 
    student_sector <- df$sector[student_index][1]

    if (student_sector == "CLASSIQUE") {
        wish_cols <- grep("^Q02_Voeux->", names(df), value = TRUE)
    } else if (student_sector == "EMIR") {
        wish_cols <- grep("^Q03_VoeuxEMIR->", names(df), value = TRUE)
    } else if (student_sector == "MICA") {
        wish_cols <- grep("^Q04_voeuxMICA->", names(df), value = TRUE)
    } else {
        stop("Erreur : département inconnu pour cet étudiant.")
    }

    # verifies the wish number exists
    if (wish_number > length(wish_cols)) {
        stop("Erreur : numéro de voeu invalide pour cet étudiant.")
    }

    # finds the departement that corresponds to the wish number
    chosen_department <- sub("^Q[0-9]+_Voeux(EMIR|MICA)?->", "", wish_cols[wish_number])

    # creates the session column if it doesn't exists yet
    session_col <- paste0("Session_", session)
    if (!session_col %in% names(df)) {
        df[[session_col]] <- NA
    }

    # Vchecks if the student was already affected to something on that session
    previous_department <- df[student_index, session_col][1]
    if (!is.na(previous_department)) {
        message("Attention : ", first_name, " ", last_name, " était déjà affecté à ", previous_department, " en session ", session, ". Changement en cours...")
    } else {
        message(first_name, " ", last_name, " n'était pas encore affecté en session ", session, ". Affectation en cours...")
    }

    # affects the asked wish to the session
    df[student_index[1], session_col] <- chosen_department

    return(df)
}

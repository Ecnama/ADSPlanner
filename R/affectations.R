NB_SESSIONS <- c(
    "FC_FIRE" = 3,
    "EMIR" = 3,
    "MICA" = 3,
    "FISP" = 2
)

#' Erase all affected departments
#'
#' @param df The data frame with the students and their wishes
assign_depart_erase <- function(df) {
    df$Aff_depart_1 <- NA_character_
    df$Aff_depart_2 <- NA_character_
    df$Aff_depart_3 <- NA_character_
    df
}

#' Assign departments to students according to their wishes, regardless of the number of places
#'
#' @param df The data frame with the students and their wishes
#' @param wish_number The number of the wish to assign
#' @return The input data frame with affected departments
assign_depart_hard <- function(df, wish_number) {
    for (i in seq_len(nrow(df))) {
        j <- 1
        while (j <= NB_SESSIONS[df$Filiere[i]]) {
            if (is.na(df[[paste("Aff_depart_", j, sep = "")]][i])) {
                df[[paste("Aff_depart_", j, sep = "")]][i] <- df[[paste("V", wish_number, sep = "")]][i]
                #print(paste("Assigned", df[[paste("V", wish_number, sep = "")]][i], "to", df$Nom[i], df$Prenom[i]))
                break()
            } else if (df[[paste("Aff_depart_", j, sep = "")]][i] == df[[paste("V", wish_number, sep = "")]][i]) { # Don't assign the same department twice
                break()
            } else {
                j <- j + 1
            }
        }
    }

    df
}

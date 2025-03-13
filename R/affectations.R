NB_SESSIONS <- c(
    "FC_FIRE" = 3,
    "EMIR" = 2,
    "MICA" = 2
)

#' Function used by server to handle affectations
#'
#' @param input Input data from the frontend
#' @param output Output data the frontend will receive
#' @param df The reactive data frame of students's wishes and affectations
handle_affectations <- function(input, output, df) {
    common_checks <- function() {
        if (is.null(df())) {
            showNotification("Aucun fichier charg\u00E9.", type = "warning")
            return(FALSE)
        }

        if (is.null(input$aff_depart_table_rows_selected)) {
            showNotification("Aucune ligne s\u00E9lectionn\u00E9e.", type = "warning")
            return(FALSE)
        }
        TRUE
    }

    try_affectation <- function(number) {
        if (!common_checks()) {
            return()
        }

        df(assign_depart_hard(df(), input$aff_depart_table_rows_selected, number))

        showNotification(paste("D\u00E9partements des voeux ", number, " affect\u00E9s."), type = "message")
    }

    observeEvent(input$assign_depart_hard_1, try_affectation(1))

    observeEvent(input$assign_depart_hard_2, try_affectation(2))

    observeEvent(input$assign_depart_hard_3, try_affectation(3))

    observeEvent(input$assign_depart_real, {
        if (!common_checks()) {
            return()
        }

        showNotification("Not implemented yet.", type = "warning")
    })

    observeEvent(input$assign_depart_erase, {
        if (!common_checks()) {
            return()
        }

        df(assign_depart_erase(df(), input$aff_depart_table_rows_selected))

        showNotification("Affectations de d\u00E9partements effac\u00E9es.", type = "message")
    })
}

#' Erase all affected departments
#'
#' @param df The data frame with the students and their wishes
#' @param selection The indices of students to erase
assign_depart_erase <- function(df, selection) {
    df[selection, ]$Aff_depart_1 <- NA_character_
    df[selection, ]$Aff_depart_2 <- NA_character_
    df[selection, ]$Aff_depart_3 <- NA_character_
    df
}

#' Assign departments to students according to their wishes, regardless of the number of places
#'
#' @param df The data frame with the students and their wishes
#' @param selection The indices of students to assign
#' @param wish_number The number of the wish to assign
#' @return The input data frame with affected departments
assign_depart_hard <- function(df, selection, wish_number) {
    for (i in selection) {
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

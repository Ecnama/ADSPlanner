library(openxlsx)
library(readODS)

# Prefixes of the head of the wishes columns
Q1_PREFIX <- "Q01_Filiere"
Q2_PREFIX <- "Q02_Voeux->"
Q3_PREFIX <- "Q03_VoeuxEMIR->"
Q4_PREFIX <- "Q04_voeuxMICA->"

DF_STRUCTURE <- data.frame(
    Nom = character(0),
    Prenom = character(0),
    Classement = integer(0),
    Filiere = character(0),
    V1 = character(0),
    V2 = character(0),
    V3 = character(0),
    V4 = character(0),
    V5 = character(0),
    V6 = character(0),
    V7 = character(0),
    Aff_depart_1 = character(0), # Assigned departments (order doesn't matter)
    Aff_depart_2 = character(0),
    Aff_depart_3 = character(0),
    Aff_session_1 = character(0), # Three columns for final affectations of all the sessions
    Aff_session_2 = character(0),
    Aff_session_3 = character(0)
)

#' Synthesizes a student's wishes from multiple columns per filiere to 7 columns for all filieres
#'
#' @param wishes The wishes columns
#' @return A vector with the filiere and the 7 wishes (NA if no wish)
synthesize_wishes <- function(wishes) {
    # Get the head of the columns
    colnames <- colnames(wishes)
    # Extract the filiere info
    if (length(grep("FC_FIRE", wishes[1])) > 0) {
        # Dataframe from rank number and speciality name
        df <- data.frame(rank = unlist(wishes[2:8]), spe = sub(Q2_PREFIX, "", colnames[2:8]))
        # Sort the dataframe by rank
        df <- df[order(df$rank), ]
        # Set result to the filiere and the specialities ordered by the student's preferences
        result <- c("FC_FIRE", df$spe)
    } else if (length(grep("EMIR", wishes[1])) > 0) {
        df <- data.frame(rank = unlist(wishes[9:12]), spe = sub(Q3_PREFIX, "", colnames[9:12]))
        df <- df[order(df$rank), ]
        result <- c("EMIR", df$spe)
    } else if (length(grep("MICA", wishes[1])) > 0) {
        df <- data.frame(rank = unlist(wishes[13:16]), spe = sub(Q4_PREFIX, "", colnames[13:16]))
        df <- df[order(df$rank), ]
        result <- c("MICA", df$spe)
    } else {
        stop("Unknown filiere")
    }

    result
}

#' Extracts raw data from the provided file path (.xlsx or .ods)
#'
#' @param file_path The path to the file to extract data from
#' @param sheet The sheet number to read from
#' @return A list containing the file data and the sheet names
read_file <- function(file_path, sheet) {
    if (!file.exists(file_path)) {
        stop("File does not exist")
    }

    file_data <- NULL
    sheet_names <- NULL

    if (grepl(".xlsx", file_path)) {
        file_data <- openxlsx::read.xlsx(file_path, sheet = sheet, skipEmptyRows = FALSE, skipEmptyCols = FALSE, colNames = TRUE, sep.names = " ")
        sheet_names <- openxlsx::getSheetNames(file_path)
    } else if (grepl(".ods", file_path)) {
        file_data <- readODS::read_ods(file_path, sheet = sheet, col_names = TRUE, as_tibble = FALSE, na = "NULL")
        sheet_names <- readODS::list_ods_sheets(file_path)
    } else {
        stop("File type not supported")
    }

    list("file_data" = file_data, "sheet_names" = sheet_names)
}

#' Processes the file data to match our data structure
#'
#' @param file_data Raw data from the file
#' @param data_source The string of the data source (Moodle or ADSPlanner)
#' @return The final data frame
process_data <- function(file_data, data_source) {
    data <- DF_STRUCTURE

    for (i in seq_len(nrow(file_data))) {
        switch(data_source,
            "Moodle" = {
                # Name
                name_split <- strsplit(file_data[i, 1], split = " ")[[1]] # We assume that the first and last name are separated by a space
                data[i, 1] <- name_split[2]
                data[i, 2] <- name_split[1]
                # Rank
                data[i, 3] <- as.integer(file_data[i, 2])
                # Wishes
                tryCatch(
                    {
                        synthesized_wishes <- synthesize_wishes(file_data[i, 3:18])
                    },
                    error = function(e) {
                        stop("Error while synthesizing the wishes (l.", i, "): ", conditionMessage(e))
                    }
                )
                data[i, 4:(length(synthesized_wishes) + 3)] <- synthesized_wishes
            },
            "ADSPlanner" = {
                data <- file_data
            },
            stop("Unknown data source")
        )
    }

    data <- data[order(data$Classement), ]
    rownames(data) <- data$Classement
    data
}

#' Parses the file to build a clean data frame (main function to call)
#'
#' @param file_path The path to the file to parse
#' @return A list containing the data frame and the capacities if they exist in the input file
parse_file <- function(file_path) {
    read_return <- read_file(file_path, 1)
    file_data <- read_return$file_data
    sheet_names <- read_return$sheet_names

    data_source <- NULL
    capacities <- NULL

    if (grepl("ADSPlanner", sheet_names[1])) {
        read <- read_file(file_path, 2)$file_data
        capacities <- data.frame(t(read$Capacite))
        colnames(capacities) <- gsub("&", "", read$Departement)

        diff <- setdiff(names(DF_STRUCTURE), names(file_data))
        if (length(diff) > 0) {
            stop("File parsing error, missing columns: ", paste(diff, collapse = ", "))
        }

        data_source <- "ADSPlanner"
        file_data <- file_data[, names(DF_STRUCTURE)]
    } else {
        needed_columns <- grep(sprintf("%s|%s|%s|%s", Q1_PREFIX, Q2_PREFIX, Q3_PREFIX, Q4_PREFIX), colnames(file_data), value = TRUE)

        diff <- setdiff(needed_columns, names(file_data))
        if (length(diff) > 0) {
            stop("File parsing error, missing columns: ", paste(diff, collapse = ", "), "\nAre you sure this is a Moodle file?")
        }

        data_source <- "Moodle"
        file_data <- file_data[, c("Nom complet", "Classement", needed_columns)]
    }

    list("df" = process_data(file_data, data_source), "capacities" = capacities)
}

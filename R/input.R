library(openxlsx)
library(readODS)

# Prefixes of the head of the wishes columns
Q2_PREFIX <- "Q02_Voeux->"
Q3_PREFIX <- "Q03_VoeuxEMIR->"
Q4_PREFIX <- "Q04_voeuxMICA->"

#' Reads the file and selects the needed columns
#'
#' @param file_path The path to the file to parse
#' @return The file as a data frame
read_file <- function(file_path) {
    # Check if file extension is supported (xlsx or ods)
    if (grepl(".xlsx", file_path)) {
        file_data <- openxlsx::read.xlsx(file_path, sheet = 1, skipEmptyRows = FALSE, skipEmptyCols = FALSE, colNames = TRUE, cols = (8:25), sep.names = " ")
        file_data <- file_data[, c(8:25)]
    } else if (grepl(".ods", file_path)) {
        file_data <- readODS::read_ods(file_path, sheet = 1, col_names = TRUE, as_tibble = FALSE, na = "NULL")
        file_data <- file_data[, c(8:25)]
    } else {
        stop("File type not supported")
    }
    return(file_data)
}

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
    return(result)
}

#' Parses the file to build a clean data frame
#'
#' @param file_path The path to the file to parse
#' @return A data frame with the cleaned data
parse_file <- function(file_path) {
    # Get file data as a data frame
    tryCatch({
        file_data <- read_file(file_path)
    }, error = function(e) {
        stop("Error while reading the file : ", conditionMessage(e))
    })
    # Build the result data frame
    data <- data.frame(
        Nom = character(0),
        Prenom = character(0),
        Filiere = character(0),
        V1 = character(0),
        V2 = character(0),
        V3 = character(0),
        V4 = character(0),
        V5 = character(0),
        V6 = character(0),
        V7 = character(0),
        Aff_depart_1 = character(0), # Affected departments (order doesn't matter)
        Aff_depart_2 = character(0),
        Aff_depart_3 = character(0),
        Aff_session_1 = character(0), # Three columns for final affectations of all the sessions
        Aff_session_2 = character(0),
        Aff_session_3 = character(0)
    )
    # Process each row of the file's data
    for (i in seq_len(nrow(file_data))) {
        # Name
        name_split <- strsplit(file_data[i, 1], split = " ")[[1]] # We assume that the first and last name are separated by a space
        data[i, 1] <- name_split[2]
        data[i, 2] <- name_split[1]
        # Wishes
        tryCatch({
            synthesized_wishes <- synthesize_wishes(file_data[i, 3:18])
        }, error = function(e) {
            stop("Error while synthesizing the wishes (l.", i, "): ", conditionMessage(e))
        })
        data[i, 3:(length(synthesized_wishes) + 2)] <- synthesized_wishes
    }
    return(data)
}

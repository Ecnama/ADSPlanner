synth_wishes_input_fc_fire <- data.frame(
    "Q01_Filiere" = "1 : FC_FIRE (Filière classique ou filière internationale)",
    "Q02_Voeux->EII" = 1,
    "Q02_Voeux->E&T" = 2,
    "Q02_Voeux->INFO" = 5,
    "Q02_Voeux->MA" = 4,
    "Q02_Voeux->GCU" = 3,
    "Q02_Voeux->GMA" = 6,
    "Q02_Voeux->GPM" = 7,
    "Q03_VoeuxEMIR->EII" = NA,
    "Q03_VoeuxEMIR->INFO" = NA,
    "Q03_VoeuxEMIR->GPM" = NA,
    "Q03_VoeuxEMIR->E&T" = NA,
    "Q04_voeuxMICA->GCU" = NA,
    "Q04_voeuxMICA->MA" = NA,
    "Q04_voeuxMICA->GMA" = NA,
    "Q04_voeuxMICA->INFO" = NA,
    check.names = FALSE
)

synth_wishes_output_fc_fire <- c(
    "FC_FIRE",
    "EII",
    "E&T",
    "GCU",
    "MA",
    "INFO",
    "GMA",
    "GPM"
)

synth_wishes_input_emir <- data.frame(
    "Q01_Filiere" = "2 : EMIR",
    "Q02_Voeux->EII" = NA,
    "Q02_Voeux->E&T" = NA,
    "Q02_Voeux->INFO" = NA,
    "Q02_Voeux->MA" = NA,
    "Q02_Voeux->GCU" = NA,
    "Q02_Voeux->GMA" = NA,
    "Q02_Voeux->GPM" = NA,
    "Q03_VoeuxEMIR->EII" = 4,
    "Q03_VoeuxEMIR->INFO" = 3,
    "Q03_VoeuxEMIR->GPM" = 2,
    "Q03_VoeuxEMIR->E&T" = 1,
    "Q04_voeuxMICA->GCU" = NA,
    "Q04_voeuxMICA->MA" = NA,
    "Q04_voeuxMICA->GMA" = NA,
    "Q04_voeuxMICA->INFO" = NA,
    check.names = FALSE
)

synth_wishes_output_emir <- c(
    "EMIR",
    "E&T",
    "GPM",
    "INFO",
    "EII"
)

synth_wishes_input_mica <- data.frame(
    "Q01_Filiere" = "3 : MICA",
    "Q02_Voeux->EII" = NA,
    "Q02_Voeux->E&T" = NA,
    "Q02_Voeux->INFO" = NA,
    "Q02_Voeux->MA" = NA,
    "Q02_Voeux->GCU" = NA,
    "Q02_Voeux->GMA" = NA,
    "Q02_Voeux->GPM" = NA,
    "Q03_VoeuxEMIR->EII" = NA,
    "Q03_VoeuxEMIR->INFO" = NA,
    "Q03_VoeuxEMIR->GPM" = NA,
    "Q03_VoeuxEMIR->E&T" = NA,
    "Q04_voeuxMICA->GCU" = 4,
    "Q04_voeuxMICA->MA" = 1,
    "Q04_voeuxMICA->GMA" = 3,
    "Q04_voeuxMICA->INFO" = 2,
    check.names = FALSE
)

synth_wishes_output_mica <- c(
    "MICA",
    "MA",
    "INFO",
    "GMA",
    "GCU"
)

synth_wishes_input_invalid <- data.frame(
    "Q01_Filiere" = "4 : Filière that doesn't exist",
    "Q02_Voeux->EII" = NA,
    "Q02_Voeux->E&T" = NA,
    "Q02_Voeux->INFO" = NA,
    "Q02_Voeux->MA" = NA,
    "Q02_Voeux->GCU" = NA,
    "Q02_Voeux->GMA" = NA,
    "Q02_Voeux->GPM" = NA,
    "Q03_VoeuxEMIR->EII" = NA,
    "Q03_VoeuxEMIR->INFO" = NA,
    "Q03_VoeuxEMIR->GPM" = NA,
    "Q03_VoeuxEMIR->E&T" = NA,
    "Q04_voeuxMICA->GCU" = 4,
    "Q04_voeuxMICA->MA" = 1,
    "Q04_voeuxMICA->GMA" = 3,
    "Q04_voeuxMICA->INFO" = 2,
    check.names = FALSE
)

parse_file_output <- data.frame(
    Nom = c("Marchand", "ROZE", "Roze"),
    Prenom = c("Laurence", "Laurence", "Laurence"),
    Classement = c(1, 2, 3),
    Filiere = c("FC_FIRE", "EMIR", "MICA"),
    V1 = c("EII", "E&T", "MA"),
    V2 = c("E&T", "GPM", "INFO"),
    V3 = c("GCU", "INFO", "GMA"),
    V4 = c("MA", "EII", "GCU"),
    V5 = c("INFO", NA_character_, NA_character_),
    V6 = c("GMA", NA_character_, NA_character_),
    V7 = c("GPM", NA_character_, NA_character_),
    Aff_depart_1 = c(NA_character_, NA_character_, NA_character_),
    Aff_depart_2 = c(NA_character_, NA_character_, NA_character_),
    Aff_depart_3 = c(NA_character_, NA_character_, NA_character_),
    Aff_session_1 = c(NA_character_, NA_character_, NA_character_),
    Aff_session_2 = c(NA_character_, NA_character_, NA_character_),
    Aff_session_3 = c(NA_character_, NA_character_, NA_character_)
)

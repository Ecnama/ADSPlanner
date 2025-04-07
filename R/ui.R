library(bslib)

#' Frontend UI layout
ui <- page_sidebar(
    title = "ADS-Planner",
    sidebar = sidebar(
        width = 350,
        card(
            "Nombre de places par session",
            numericInput("capacity_EII", "EII", value = 0),
            numericInput("capacity_EetT", "EetT", value = 0),
            numericInput("capacity_INFO", "INFO", value = 0),
            numericInput("capacity_MA", "MA", value = 0),
            numericInput("capacity_GCU", "GCU", value = 0),
            numericInput("capacity_GMA", "GMA", value = 0),
            numericInput("capacity_GPM", "GPM", value = 0),
            uiOutput("recuperation_button")
        ),
        card(
            fileInput("file", "R\u00E9sultats du sondage", accept = c(".xlsx", ".ods"), buttonLabel = "Parcourir...", placeholder = "Aucun fichier", multiple = FALSE),
        ),
        card(
            textInput("download_name", "Nom du fichier d'affectations", value = "affectations"),
            uiOutput("download_button"),
        ),
    ),
    navset_tab(
        nav_panel(
            "Visualisation v\u0153ux",
            uiOutput("vis"),
        ),
        nav_panel(
            "Affectations d\u00E9partements",
            div(
                actionButton("assign_depart_erase", "Effacer toutes affectations", width = 180),
                actionButton("assign_depart_hard_1", "Affectation \"dure\" voeu 1", width = 180),
                actionButton("assign_depart_hard_2", "Affectation \"dure\" voeu 2", width = 180),
                actionButton("assign_depart_hard_3", "Affectation \"dure\" voeu 3", width = 180),
                actionButton("assign_depart_real", "Affectation r\u00E9elle voeux restants", width = 200),
            ),
            uiOutput("aff_depart"),
            card(
                tableOutput("capacities_counters"),
                textOutput("capacity_full")
            )
        ),
        nav_panel(
            "Affectations sessions",
            "Not implemented yet.",
        )
    ),
)

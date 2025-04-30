library(bslib)

#' Frontend UI layout
ui <- page_sidebar(
    title = "ADS-Planner",
    sidebar = sidebar(
        width = 350,
        card(
            "Nombre de places par session",
            numericInput("capacity_EII","EII"),
            numericInput("capacity_EetT","EetT"),
            numericInput("capacity_INFO","INFO"),
            numericInput("capacity_MA","MA"),
            numericInput("capacity_GCU","GCU"),
            numericInput("capacity_GMA","GMA"),
            numericInput("capacity_GPM","GPM"),
            uiOutput("recuperation_button")
        ),
        card(
            fileInput("file", "R\u00E9sultats du sondage", accept = c(".xlsx", ".ods"), buttonLabel = "Parcourir...", placeholder = "Aucun fichier", multiple = FALSE),
        ),
        card(
            textInput("download_name", "Nom du fichier d'affectations", value = "affectations"),
            uiOutput("download_button"),
        ),
        card(
            "Nombre de places par session",
            numericInput("capacity_EII", "EII", value = 3, min = 0),
            numericInput("capacity_EetT", "E&T", value = 3, min = 0),
            numericInput("capacity_INFO", "INFO", value = 3, min = 0),
            numericInput("capacity_MA", "MA", value = 3, min = 0),
            numericInput("capacity_GCU", "GCU", value = 3, min = 0),
            numericInput("capacity_GMA", "GMA", value = 3, min = 0),
            numericInput("capacity_GPM", "GPM", value = 3, min = 0)
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
                actionButton("assign_depart_hard", "Affectation \"dure\"", width = 180),
                actionButton("assign_depart_real", "Affectation r\u00E9elle voeux restants", width = 200),
            ),
            card(
                "Capacit\u00E9s restantes",
                uiOutput("capacities_counters"),
                textOutput("capacity_full")
            ),
            uiOutput("aff_depart")
        ),
        nav_panel(
            "Affectations sessions",
            "Not implemented yet.",
        )
    ),
)

library(bslib)

#' Frontend UI layout
ui <- page_sidebar(
    title = "ADS-Planner",
    sidebar = sidebar(
        width = 350,
        card(
            fileInput("file", "R\u00E9sultats du sondage", accept = c(".xlsx", ".ods"), buttonLabel = "Parcourir...", placeholder = "Aucun fichier", multiple = FALSE),
        ),
        card(
            textInput("download_name", "Nom du fichier d'affectations", value = "affectations"),
            uiOutput("download_button"),
        ),
        card(
            "Nombre de places par session",
            numericInput("capacity_EII", "EII", value = 30, min = 0),
            numericInput("capacity_ET", "E&T", value = 30, min = 0),
            numericInput("capacity_INFO", "INFO", value = 30, min = 0),
            numericInput("capacity_MA", "MA", value = 30, min = 0),
            numericInput("capacity_GCU", "GCU", value = 30, min = 0),
            numericInput("capacity_GMA", "GMA", value = 30, min = 0),
            numericInput("capacity_GPM", "GPM", value = 30, min = 0)
        ),
    ),
    navset_tab(
        nav_panel(
            "Visualisation v\u0153ux",
            br(),
            uiOutput("vis"),
        ),
        nav_panel(
            "Affectations d\u00E9partements",
            layout_column_wrap(uiOutput("capacities_counters")),
            layout_columns(
                style = "height: 70px; display: flex; align-items: center;",
                actionButton("assign_depart_erase", "Effacer toutes affectations", style = "height:70px;"),
                actionButton("assign_depart_hard", "Affectation \"dure\"", style = "height:70px;"),
                actionButton("assign_depart_real", "Affectation r\u00E9elle voeux restants", style = "height:70px;"),
                actionButton("assign_depart_targeted", "Affectation cibl\u00E9e", style = "height:70px;"),
                checkboxInput("filter_full", "Cacher les \u00E9tudiants compl\u00E8tement affect\u00E9s", value = FALSE),
            ),
            uiOutput("aff_depart")
        ),
        nav_panel(
            "Affectations sessions",
            br(),
            layout_columns(
                actionButton("assign_session_auto", "Affectation automatique des sessions"),
            ),
            uiOutput("aff_session"),
        )
    ),
)

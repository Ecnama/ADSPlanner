library(bslib)

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
    ),
    navset_tab(
        nav_panel(
            "Visualisation des choix",
            tableOutput("vis_table"),
        ),
        nav_panel(
            "Sans r\u00E9ponse",
            "Not implemented yet.",
        ),
        nav_panel(
            "Affectations",
            "Not implemented yet.",
            actionButton("assign_first", "Auto-assigner premier voeu"),
            actionButton("assign_second", "Auto-assigner deuxi\u00E8me voeu"),
        ),
    ),
)

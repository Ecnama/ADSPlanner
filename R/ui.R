library(bslib)

ui <- page_sidebar(
    title = "ADS-Planner",
    sidebar = sidebar(
        width = 350,
        card(
            fileInput("file", "R\u00E9sultats du sondage", accept = c(".xlsx", ".ods"), buttonLabel = "Parcourir...", placeholder = "Aucun fichier", multiple = FALSE),
        ),
        downloadButton("download", "T\u00E9l\u00E9charger affectations"),
    ),
    navset_tab(
        nav_panel(
            "Visualisation des choix",
            "Not implemented yet.",
        ),
        nav_panel(
            "Sans r\u00E9ponse",
            "Not implemented yet.",
        ),
        nav_panel(
            "Affectations",
            "Not implemented yet.",
        ),
    ),
)

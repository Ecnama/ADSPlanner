library(bslib)

#' Custom tooltip with preset options for the operation buttons
#'
#' @param target The target element for the tooltip
#' @param text The text of the tooltip
#' @param placement The placement of the tooltip
#' @param options Additional options for the tooltip
#' @param ... Additional arguments for the tooltip
#' @return A bslib tooltip object
operation_tooltip <- function(target, text, placement = "bottom", options = list(delay = list(show = 250, hide = 0)), ...) {
    tooltip(
        target,
        text,
        placement = placement,
        options = options,
        ...
    )
}

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
            "Affectation d\u00E9partements",
            layout_column_wrap(uiOutput("depart_capacities_counter")),
            layout_columns(
                style = "height: 70px; display: flex; align-items: center;",
                operation_tooltip(
                    actionButton("assign_depart_erase", "Effacer toutes affectations", style = "height:70px;"),
                    "Efface toutes les affectations de d\u00E9partements des \u00E9tudiants s\u00E9lectionn\u00E9s"
                ),
                operation_tooltip(
                    actionButton("assign_depart_hard", "Affectation dure", style = "height:70px;"),
                    "Affectation du d\u00E9partement au num\u00E9ro de v\u0153u sp\u00E9cifi\u00E9"
                ),
                operation_tooltip(
                    actionButton("assign_depart_targeted", "Affectation cibl\u00E9e", style = "height:70px;"),
                    "Affectation d'un d\u00E9partement \u00E0 la place d'un autre, ou si l'\u00E9tudiant n'a pas toutes ses affectations"
                ),
                operation_tooltip(
                    actionButton("assign_depart_real", "Affectation r\u00E9elle voeux restants", style = "height:70px;"),
                    "Affectation intelligente des d\u00E9partements prenant en compte les places restantes et le classement des \u00E9tudiants"
                ),
                operation_tooltip(
                    checkboxInput("filter_full_depart", "Cacher \u00E9tudiants compl\u00E8tement affect\u00E9s", value = FALSE),
                    "Cache les \u00E9tudiants \u00E9tant affect\u00E9s \u00E0 assez de d\u00E9partements pour toutes leurs sessions"
                ),
            ),
            uiOutput("aff_depart")
        ),
        nav_panel(
            "Affectation sessions",
            layout_column_wrap(uiOutput("session_capacities_counter")),
            layout_columns(
                operation_tooltip(
                    actionButton("assign_session_erase", "Effacer toutes affectations", style = "height:70px;"),
                    "Efface toutes les affectations de sessions des \u00E9tudiants s\u00E9lectionn\u00E9s"
                ),
                operation_tooltip(
                    actionButton("assign_session_auto", "Affectation automatique", style = "height:70px;"),
                    "Affectation automatique des sessions en fonction des d\u00E9partements affect\u00E9s"
                ),
                operation_tooltip(
                    actionButton("assign_session_manual", "Affectation manuelle", style = "height:70px;"),
                    "Affectation manuelle d'un d\u00E9partements \u00E0 une session, en le d\u00E9pla\u00E7ant si il est d\u00E9j\u00E0 affect\u00E9 \u00E0 une autre session"
                ),
                operation_tooltip(
                    checkboxInput("filter_full_session", "Cacher \u00E9tudiants compl\u00E8tement affect\u00E9s", value = FALSE),
                    "Cache les \u00E9tudiants \u00E9tant affect\u00E9s \u00E0 toutes leurs sessions"
                ),
            ),
            uiOutput("aff_session"),
        )
    ),
)

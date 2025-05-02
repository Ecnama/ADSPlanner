library(shiny)

source("R/input.R")
source("R/affectations.R")
source("R/tables.R")

#' Backend server logic
#'
#' @param input Input data from the frontend
#' @param output Output data the frontend will receive
server <- function(input, output) {

    depart_choices <- c("EII", "ET", "MA", "INFO", "GPM", "GMA", "GCU")

    df <- reactiveVal(NULL)

    observe({
        if (!is.null(input$file)) {
            data <- parse_file(input$file$datapath[1])
            as.integer(data$Classement)
            df(data)
            showNotification("Fichier charg\u00E9 avec succ\u00E8s.", type = "message")
        }
    })

    handle_affectations(input, output, df)

    output$download_button <- renderUI({
        downloadButton("download", paste("T\u00E9l\u00E9charger ", input$download_name, ".xlsx", sep = ""))
    })

    output$download <- downloadHandler(
        filename = function() {
            paste(input$download_name, ".xlsx", sep = "")
        },
        content = function(file) {
            write("Not implemented yet.", file)
        }
    )

    display_tables(input, output, df) 
    
    # Bouton "affectation ciblée" déclenche les menus
observeEvent(input$show_targeted_ui, {
    output$aff_depart_targeted <- renderUI({
        tagList(
            selectInput("old_department", "Département actuel :", 
                        choices = c("EII", "ET", "MA", "INFO", "GCU", "GPM", "GMA")),
            selectInput("new_department", "Nouveau département :", 
                        choices = c("EII", "ET", "MA", "INFO", "GCU", "GPM", "GMA")),
            actionButton("validate_targeted", "Valider l'affectation", width = 200)
        )
    })
})

# Lorsqu'on valide l'affectation
observeEvent(input$validate_targeted, {
    req(df())
    
    # Sélectionner les étudiants
    selection <- input$students_table_rows_selected
    
    # Vérifier si aucune ligne n'est sélectionnée
    if (length(selection) == 0) {
        showNotification("Veuillez sélectionner au moins un étudiant.", type = "error")
        return()  # Retourne immédiatement sans effectuer l'affectation
    }
    
    # Appliquer l'affectation ciblée
    updated_df <- targeted_affectation(
        df(),
        selection,
        input$old_department,
        input$new_department
    )
    df(updated_df)
    showNotification("Affectation ciblée effectuée avec succès.", type = "message")
})

}

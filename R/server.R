library(shiny)

source("R/input.R")

server <- function(input, output) {
    data <- reactive({
        if (is.null(input$file)) {
            return(NULL)
        }
        parse_file(input$file$datapath[1])
    })

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

    output$vis_table <- {
        renderTable(data(), striped = TRUE)
    }
}

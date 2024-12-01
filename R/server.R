library(shiny)

server <- function(input, output) {
    output$download <- downloadHandler(
        filename = "affectations.xlsx",
        content = function(file) {
            write("Not implemented yet.", file)
        }
    )
}

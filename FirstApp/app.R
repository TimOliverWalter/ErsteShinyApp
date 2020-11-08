library(shiny)
library(shinythemes)
library(plotly)
library(datasets)

ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage("Meine erste App",
                           tabPanel("Iris Datensatz",
                                    sidebarPanel(selectInput("var_x","X-Variable:",
                                                             c("Sepal Length"="Sepal.Length",
                                                               "Petal Length"="Petal.Length")
                                    )),
                                    mainPanel(plotlyOutput("iris_data")
                                              )
                                    ),
                           tabPanel("Mtcars Datensatz",
                                    mainPanel(plotlyOutput("mtcars_data")
                                              )
                                    )
                           )
                )
     
# Define server function  
server <- function(input, output) {
    
    output$iris_data <- renderPlotly({
        plot <- plot_ly(x=iris[,input$var_x],
                        y=iris[,"Petal.Length"],
                        type="scatter",
                        mode="markers")
    })
    
    output$mtcars_data <- renderPlotly({
        plot <- plot_ly(x=mtcars[,"cyl"],
                        y=mtcars[,"mpg"],
                        type = "scatter",
                        mode="markers")
    })
}
# Run the application 
shinyApp(ui = ui, server = server)

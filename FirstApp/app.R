library(shiny)
library(shinythemes)
library(plotly)
library(datasets)

ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                    "Meine erste App",
                    tabPanel(
                        "Iris Datensatz",
                        sidebarPanel(
                            selectInput(
                                "var_x",
                                "X-Variable:",
                                c(
                                    "Sepal Length" = "Sepal.Length",
                                    "Sepal Width" = "Sepal.Width",
                                    "Petal Length" = "Petal.Length",
                                    "Petal Width" = "Petal.Width"
                                )
                            ),
                            selectInput(
                                "var_y",
                                "X-Variable:",
                                c(
                                    "Petal Length" = "Petal.Length",
                                    "Petal Width" = "Petal.Width",
                                    "Sepal Length" = "Sepal.Length",
                                    "Sepal Width" = "Sepal.Width"
                                )
                            )
                        ),
                        mainPanel(plotlyOutput("iris_data"))
                    ),
                    tabPanel(
                        "Mtcars Datensatz",
                        mainPanel(plotlyOutput("mtcars_data"))
                    )
                ))

server <- function(input, output) {
    output$iris_data <- renderPlotly({
        plot <- plot_ly(
            x = iris[, input$var_x],
            y = iris[, input$var_y],
            type = "scatter",
            mode = "markers",
            color = iris$Species
        ) %>%
            layout(
                xaxis = list(title = input$var_x),
                yaxis = list(title = input$var_y)
            )
    })
    
    output$mtcars_data <- renderPlotly({
        plot <- plot_ly(
            x = mtcars[, "wt"],
            y = mtcars[, "hp"],
            z = mtcars[, "qsec"],
            marker = list(
                color = mtcars$mpg,
                colorscale = c('#FFE1A1', '#683531'),
                showscale = TRUE
            )
        ) %>%
            add_markers() %>%
            layout(scene = list(
                xaxis = list(title = 'Weight'),
                yaxis = list(title = 'Horsepower'),
                zaxis = list(title = 'Mile Time')
            ))
        
    })
}

shinyApp(ui = ui, server = server)

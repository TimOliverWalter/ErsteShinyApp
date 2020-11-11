library(shiny)
library(shinythemes)
library(plotly)
library(datasets)
library(htmlwidgets)
library(dplyr)
library(RColorBrewer)

fifa19_data <-
    read.csv("C:/Users/timwa/Downloads/ErsteShinyApp/ErsteShinyApp/data.csv")

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
                        sidebarPanel(downloadButton("download")),
                        mainPanel(plotlyOutput("mtcars_data"))
                    ),
                    tabPanel(
                        "Spieler Fifa19 Datensatz",
                        sidebarPanel(
                            sliderInput(
                                "age",
                                "Spieleralter:",
                                min = 16,
                                max = 39,
                                value = c(30, 36)
                            ),
                            selectInput(
                                "foot",
                                "Bevorzugter Fuß:",
                                c("Rechts" = "Right",
                                  "Links" = "Left")
                            )
                        ),
                        mainPanel(
                            "Spieler mit deutscher Staatsangehörigkeit",
                            plotlyOutput("fifa_data")
                        )
                    )
                ))

server <- function(input, output) {
    iris_plot <- reactive({
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
    
    mtcars_plot <- reactive({
        plot <- mtcars %>%
            select(wt, hp, qsec, mpg) %>%
            plot_ly(
                x =  ~ wt,
                y =  ~ hp,
                z =  ~ qsec,
                marker = list(
                    color = ~ mpg,
                    colorscale = c("#FFE1A1", "#683531"),
                    showscale = TRUE
                )
            ) %>%
            add_markers() %>%
            layout(scene = list(
                camera = list(eye = list(
                    x = 1.5, y = 1.8, z = 1
                )),
                xaxis = list(title = "Weight"),
                yaxis = list(title = "Horsepower"),
                zaxis = list(title = "Mile Time")
            ))
    })
    
    fifa_plot <- reactive({
        plot <- fifa19_data %>%
            filter(
                Age %in% input$age[1]:input$age[2] &
                    Nationality == "Germany" &
                    Preferred.Foot == input$foot
            ) %>%
            plot_ly(
                type = 'scatter',
                mode = 'markers',
                x =  ~ Age,
                y =  ~ Name,
                color =  ~ Club,
                colors = colorRampPalette(brewer.pal(8, "Set2"))(18),
                hoverinfo='text',
                text=~Club,
                hovertemplate = paste("<b>Name: %{y}</b><br>",
                                      "Alter: %{x}<br>",
                                      "Verein: %{text}")
            ) %>%
            layout(
                xaxis = list(title = "", showticklabels = FALSE),
                yaxis = list(title = "", showticklabels = FALSE)
            )
    })
    
    output$iris_data <- renderPlotly({
        iris_plot()
    })
    
    output$mtcars_data <- renderPlotly({
        mtcars_plot()
    })
    
    output$download <- downloadHandler(
        filename = function() {
            "Mtcars_Plot.html"
        },
        content = function(file) {
            saveWidget(as_widget(mtcars_plot()), file)
        }
    )
    
    output$fifa_data <- renderPlotly({
        fifa_plot()
    })
    
}

shinyApp(ui = ui, server = server)
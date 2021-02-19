library(shiny)

shinyUI(fluidPage(

    titlePanel("Protected Lands and Waters Indicator"),


    fluidRow(
        mainPanel(
          girafeOutput("map_bc"),
          plotOutput("map_ecoregion")
        )
    )
))

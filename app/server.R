library(shiny)
library(readr)
library(bcmaps)
library(ggiraph)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  eco <- read_rds("../data/eco_simp.rds")

  pa_eco <- read_rds("../data/CPCAD_Dec2020_eco_simp.rds")

  region <- reactive({
    req(input$map_bc_selected)
    filter(pa_eco, ecoregion_code == input$map_bc_selected)
  })

  output$map_bc <- renderGirafe({
    g <- ggplot(data = eco) +
      theme_void() +
      geom_sf_interactive(aes(tooltip = ecoregion_name, data_id = ecoregion_code),
                          fill = "skyblue", colour = "grey20")

    girafe(ggobj = g, options = list(
      opts_hover(css = "fill:white;stroke-width:2px"),
      opts_selection(type = "single", css = "fill:white;")))
  })

  output$map_ecoregion <- renderPlot({
    req(region(), input$map_bc_selected)
    ggplot(data = region()) +
      theme_void() +
      geom_sf(data = filter(eco, ecoregion_code == input$map_bc_selected), fill = "grey80", colour = NA) +
      geom_sf(aes(fill = oecm), colour = NA) +
      scale_fill_manual(values = c("Yes" = "#004529", "No" = "#93c288"), guide = FALSE)
  })

})

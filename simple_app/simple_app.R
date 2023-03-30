library(shiny)
library(tidyverse)
library(sf)

ui <- fluidPage(
  plotOutput('bcplot', click = 'plot_click', dblclick = 'plot_reset')
)

server <- function(input, output, session) {

  eco = readRDS('out/eco_simp.rds')
  pa_eco = readRDS('out/pa_eco_sum.rds')
  total_prot_area = readRDS('out/total_prot_area.rds')

  #Define that none of our ecoregions have been selected.
  selected_region = reactiveVal(rep(FALSE, nrow(eco)))

  #When person clicks, add that to selection.
  observeEvent(input$plot_click, {
    clicked = shiny::nearPoints(eco, input$plot_click, allRows = T)$selected_
    selected_region(clicked || selected_region())
  })

  #When person double-clicks, reset selection to none.
  observeEvent(input$plot_reset, {
    selected_region(rep(FALSE, nrow(eco)))
  })

  output$bcplot = renderPlot({

    eco$selected = selected_region()

    ggplot() +
      geom_sf(
        aes(
          fill = total_ecoregion_by_type,
          col = selected
        ),
        size = 0.2,
        data = eco %>% filter(type == 'land')
      ) +
      geom_sf(
        aes(
          fill = total_ecoregion_by_type,
          col = selected
        ),
        size = 0.2,
        data = eco %>% filter(type == 'water')
      ) +
      coord_equal()

    })

  # eco_selected = reactive({
  #   eco %>% filter(ecoregion_code %in% input$bcplot_selected)
  # })
}

shinyApp(ui, server)

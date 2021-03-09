# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

shinyServer(function(input, output) {
  output$top_left <- renderGirafe({

    g <- ggplot(data = eco) +
      theme_void() +
      geom_sf_interactive(aes(tooltip = ecoregion_name,
                              data_id = ecoregion_code,
                              fill = type, colour = type), size = 0.25) +
      scale_fill_manual(values = scale_map_fill, guide = NULL) +
      scale_colour_manual(values = scale_map_colour, guide = NULL) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0))

    girafe(ggobj = g, width_svg = top_left_width/72, height_svg = top_height/72,
           options = list(opts_hover(css = glue("fill:{hover};")),
                          opts_selection(type = "single", css = glue("fill:{select};")),
                          opts_toolbar(saveaspng = FALSE)))
  })

  output$top_right <- renderGirafe({
    if(is.null(input$top_left_selected)) {
      g <- ggplot(data = eco_area_sum,
                  aes(x = total_area, y = eco_names, fill = type_combo)) +
        theme_classic(base_size = 14) +
        theme(axis.title.y = element_blank(), legend.position = c(0.5, 0.5)) +
        geom_bar_interactive(aes(tooltip = tooltip, data_id = ecoregion_code),
                             width = 0.75, stat = "identity") +
        labs(x = lab_total_area) +
        scale_fill_manual(name = lab_oecm, values = scale_combo) +
        scale_x_continuous(expand = c(0,0))
    } else {
      region <- filter(pa_eco, ecoregion_code == input$top_left_selected)
      r <- filter(eco, ecoregion_code == input$top_left_selected)
      if(r$type[1] == "land") s <- scale_land else s <- scale_water
      g <- ggplot(data = region) +
        theme_void() +
        geom_sf(data = r, fill = "grey80", colour = NA) +
        geom_sf(aes(fill = factor(park_type)), colour = NA) +
        scale_fill_manual(name = lab_oecm, values = s, guide = FALSE) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0))
    }

    girafe(ggobj = g, width_svg = top_right_width/72, height_svg = top_height/72,
           options = list(opts_hover(css = glue("fill:{hover};")),
                          opts_selection(type = "single", css = glue("fill:{select};")),
                          opts_toolbar(saveaspng = FALSE)))
  })

  output$bottom <- renderGirafe({
    req(input$top_left_selected)

    r <- filter(eco_area, ecoregion_code == input$top_left_selected)
    if(r$type[1] == "land") s <- scale_land else s <- scale_water
    d <- min(r$date, na.rm = TRUE):max(r$date, na.rm = TRUE)
    r <- complete(r, date = full_seq(na.omit(date), 1),
                  park_type = c("OECM", "PPA"),
                  fill = list(total_area = 0))

    g1 <- ggplot(data = r, aes(x = as.integer(date),
                               y = total_area, colour = park_type)) +
      theme_classic() +
      geom_line(size = 2, na.rm = TRUE) +
      geom_point_interactive(aes(tooltip = tooltip,
                                 data_id = glue("{date} {park_type}")),
                             size = 3, na.rm = TRUE) +
      labs(x = lab_year, y = lab_growth,
           subtitle = "Excludes areas missing date of protection") +
      scale_x_continuous(breaks = break_int) +
      scale_colour_manual(name = "Type", values = s, guide = FALSE)

    r <- filter(eco_area_sum, ecoregion_code == input$top_left_selected)

    g2 <- ggplot(data = r, aes(x = total_area, y = park_type, fill = park_type)) +
      theme_classic() +
      theme(axis.title.y = element_blank()) +
      geom_bar_interactive(aes(tooltip = tooltip, data_id = park_type),
                           stat = "identity") +
      labs(x = lab_total_area,
           subtitle = "Includes all areas") +
      scale_fill_manual(name = "Type", values = s)


    g <- g1 + g2 + plot_layout(widths = c(2, 1))

    girafe(ggobj = g,
           options = list(opts_hover(css = glue("fill:{hover};")),
                          opts_selection(css = "", type = "single"),
                          opts_toolbar(saveaspng = FALSE)),
           height_svg = bottom_height/72,
           width_svg = app_width/72)
  })

})

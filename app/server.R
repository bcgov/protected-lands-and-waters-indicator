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


shinyServer(function(input, output, session) {


  # Top Panel ---------------------------------------------------------------
  output$top <- renderGirafe({

    # Top Left - Provincial Map
    g1 <- ggplot(data = eco) +
      theme_void() +
      theme(plot.margin = unit(c(0,0,0,0), "pt")) +
      geom_sf_interactive(aes(tooltip = tooltip,
                              data_id = ecoregion_code,
                              fill = type, colour = type), size = 0.25) +
      scale_fill_manual(values = scale_map_fill, guide = NULL) +
      scale_colour_manual(values = scale_map_colour, guide = NULL) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0))

    if(is.null(input$top_selected)) {

      # Top Right #1 - Provincial Bar plot
      g2 <- ggplot(data = eco_area_sum,
                   aes(x = p_type, y = ecoregion_name, fill = type_combo)) +
        theme_minimal(base_size = 10) +
        theme(panel.grid.major.y = element_blank(),
              axis.title.y = element_blank(), legend.position = c(0.5, 0.5),
              plot.margin = unit(c(0,0,0,0), "pt")) +
        geom_bar_interactive(aes(tooltip = tooltip, data_id = ecoregion_code),
                             width = 0.75, stat = "identity") +
        labs(x = lab_total_area) +
        scale_fill_manual(name = lab_oecm, values = scale_combo) +
        scale_x_continuous(expand = c(0,0), position = "top") +
        coord_fixed(ratio = 5)

    } else {
      region <- filter(pa_eco, ecoregion_code == input$top_selected)
      r <- filter(eco, ecoregion_code == input$top_selected)
      if(r$type[1] == "land") s <- scale_land else s <- scale_water

      # Top Right #2 - Ecoregion map
      g2 <- ggplot(data = region) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, size = 15),
              plot.margin = unit(c(0,0,0,0), "pt")) +
        geom_sf(data = r, fill = "grey80", colour = NA) +
        geom_sf(aes(fill = factor(park_type)), colour = NA) +
        scale_fill_manual(name = lab_oecm, values = s, guide = FALSE) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        labs(title = region$ecoregion_name[1])
    }

    g <- plot_grid(g1, g2, nrow = 1)

    # If selection no longer visible
    if(is.null(input$top_selected) ||
       !input$top_selected %in% eco$ecoregion_code) {
      selected <- NULL
    } else selected <- input$top_selected

    girafe(ggobj = g, width_svg = app_width/72, height_svg = top_height/72,
           options = list(opts_hover(css = glue("fill:{hover};")),
                          opts_selection(selected = selected,
                                         type = "single",
                                         css = glue("fill:{select};")),
                          opts_tooltip(css = tooltip_css, opacity = 1),
                          opts_toolbar(saveaspng = FALSE)))

  }) %>%
    bindCache(input$top_selected)




  # Bottom panel ------------------------------------------------------------
  output$bottom <- renderGirafe({

    if(is.null(input$top_selected)) {
      # Bottom #1 - Provincial Area plot
      r <- mutate(eco_area_all, park_type = type_combo)
      g <- gg_area(r, type = "all")
    } else {
      # Bottom #2 - Ecoregion Area plot
      r <- filter(eco_area, ecoregion_code == input$top_selected)
      g <- gg_area(r, type = "region")
    }

    girafe(ggobj = g,
           options = list(opts_hover(css = glue("fill:{hover};")),
                          opts_selection(css = "", type = "single"),
                          opts_tooltip(css = tooltip_css, opacity = 1),
                          opts_toolbar(saveaspng = FALSE)),
           height_svg = bottom_height/72,
           width_svg = bottom_width/72)
  }) %>%
    bindCache(input$top_selected)
})




# shinyServer(function(input, output, session) {
#
#   output$top <- renderGirafe({
#
#     g1 <- ggplot(data = eco) +
#       theme_void() +
#       geom_sf_interactive(aes(tooltip = ecoregion_name,
#                               data_id = ecoregion_code,
#                               fill = type, colour = type), size = 0.25) +
#       scale_fill_manual(values = scale_map_fill, guide = NULL) +
#       scale_colour_manual(values = scale_map_colour, guide = NULL) +
#       scale_x_continuous(expand = c(0,0)) +
#       scale_y_continuous(expand = c(0,0))
#
#     g2 <- ggplot(data = eco_area_sum,
#                 aes(x = p_region, y = ecoregion_name, fill = type_combo)) +
#       theme_classic(base_size = 14) +
#       theme(axis.title.y = element_blank(), legend.position = c(0.5, 0.5)) +
#       geom_bar_interactive(aes(tooltip = tooltip, data_id = ecoregion_code),
#                            width = 0.75, stat = "identity") +
#       labs(x = lab_total_area) +
#       scale_fill_manual(name = lab_oecm, values = scale_combo) +
#       scale_x_continuous(expand = c(0,0))
#
#     g <- g1 + g2 + plot_layout(widths = c(3, 1))
#
#     girafe(ggobj = g, width_svg = app_width/72, height_svg = top_height/72,
#            options = list(opts_hover(css = glue("fill:{hover};")),
#                           opts_selection(type = "single", css = glue("fill:{select};")),
#                           opts_toolbar(saveaspng = FALSE)))
#
#   })
#
#   output$bottom <- renderGirafe({
#     req(input$top_selected)
#
#     region <- filter(pa_eco, ecoregion_code == input$top_selected)
#     r <- filter(eco, ecoregion_code == input$top_selected)
#     if(r$type[1] == "land") s <- scale_land else s <- scale_water
#
#     g1 <- ggplot(data = region) +
#       theme_void() +
#       geom_sf(data = r, fill = "grey80", colour = NA) +
#       geom_sf(aes(fill = factor(park_type)), colour = NA) +
#       scale_fill_manual(name = lab_oecm, values = s, guide = FALSE) +
#       scale_x_continuous(expand = c(0,0)) +
#       scale_y_continuous(expand = c(0,0))
#
#     r <- filter(eco_area, ecoregion_code == input$top_selected)
#     r_missing <- filter(r, is.na(date)) %>%
#       mutate(date = max(r$date, na.rm = TRUE) + 1L)
#     r <- filter(r, !is.na(date))
#
#     g2 <- ggplot(data = r, aes(x = as.integer(date),
#                                y = cum_region, colour = park_type)) +
#       theme_classic() +
#       geom_line(size = 1, na.rm = TRUE) +
#       geom_point_interactive(aes(tooltip = tooltip_line,
#                                  data_id = glue("{date} {park_type}")),
#                              size = 2) +
#       labs(x = lab_year, y = lab_growth) +
#       scale_x_continuous() +
#       scale_colour_manual(name = "Type", values = s, guide = FALSE)
#
#     if(nrow(r_missing) > 0) {
#       line_missing <- bind_rows(group_by(r, park_type) %>% slice(n()), r_missing)
#       g2 <- g2 +
#         theme(legend.title = element_blank(), legend.position = "top") +
#         geom_point_interactive(data = r_missing,
#                                aes(tooltip = tooltip_line,
#                                    data_id = glue("{date} {park_type}")),
#                                size = 2, na.rm = TRUE) +
#         geom_line(data = line_missing, size = 1,
#                   na.rm = TRUE, aes(linetype = "Inc. missing dates")) +
#         scale_linetype_manual(values = "dotted")
#     }
#
#     g2 <- g2 + coord_flip()
#
#
#     r <- filter(eco_area, ecoregion_code == input$top_selected) %>%
#       select(ecoregion_code, ecoregion_name, park_type, p_area, tooltip_bar) %>%
#       distinct()
#
#     g3 <- ggplot(data = r, aes(x = p_area, y = park_type, fill = park_type)) +
#       theme_classic() +
#       theme(axis.title.y = element_blank()) +
#       geom_bar_interactive(aes(tooltip = tooltip_bar, data_id = park_type),
#                            stat = "identity") +
#       labs(x = lab_total_area,
#            subtitle = "Includes all areas") +
#       scale_fill_manual(name = "Type", values = s)
#
#     g <- g1 + g2 + plot_layout(widths = c(4, 1))
#
#     girafe(ggobj = g,
#            options = list(opts_hover(css = glue("fill:{hover};")),
#                           opts_selection(css = "", type = "single"),
#                           opts_toolbar(saveaspng = FALSE)),
#            height_svg = bottom_height/72,
#            width_svg = app_width/72)
#   })
#
#
# })

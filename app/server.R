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

  # Create BC Map with custom legends on app load
  g <- ggplot() +
    theme_void() +
    theme(plot.margin = unit(c(0,0,0,0), "pt"), legend.title = element_blank()) +
    scale_fill_manual(values = scale_map, guide = NULL) +
    scale_alpha_continuous(range = c(0.25, 1), n.breaks = 5, limits = c(0, 100),
                           labels = function(x) glue("{x}%")) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))

  g_bc <- g +
    theme(legend.position = c(0.85, 0.7)) +
    geom_sf_interactive(data = eco,
                        aes(tooltip = tooltip, fill = type, alpha = p_region,
                            data_id = ecoregion_code), size = 0.1, colour = "black") +
    guides(alpha = guide_legend(override.aes = list(fill = scale_map["land"])))

  g_legend_water <- g +
    theme(legend.position = c(0.78, 0.7)) +
    theme(legend.text = element_blank()) +
    geom_sf_interactive(data = dplyr::filter(eco, type == "water"),
                        aes(tooltip = tooltip, fill = type, alpha = p_region,
                            data_id = ecoregion_code), size = 0.1, colour = "black") +
    guides(alpha = guide_legend(override.aes = list(fill = scale_map["water"])))

  g_legend_water <- get_legend(g_legend_water)
  g_legend_land <- get_legend(g_bc)

  g_bc <- ggdraw(g_bc + theme(legend.position = "none")) +
    draw_plot(g_legend_water, x = 0, y = 0, width = 1, height = 1) +
    draw_plot(g_legend_land, x = 0, y = 0, width = 1, height = 1) +
    draw_label("Percent\nConserved", x = 0.8, y = 0.82, size = 12, colour = "black")


  # Top Panel ---------------------------------------------------------------
  output$top <- renderGirafe({

    # Top Left - Provincial Map
    g1 <- g_bc

    if(is.null(input$top_selected) || input$top_selected == "reset") {

      land <- ggplot(data=dplyr::filter(eco_area_sum, type=="land"),
                     aes(x = p_type, y = fct_reorder(ecoregion_name, p_region, .desc=FALSE),
                         fill = type, alpha = park_type)) +
        theme_minimal(base_size = 10) +
        theme(panel.grid.major.y = element_blank(),
              legend.position = c(0.7, 0.3)) +
        geom_bar_interactive(aes(tooltip = tooltip, data_id = ecoregion_code),
                             width = 0.75, stat = "identity") +
        theme(axis.title.x=element_blank()) +
        theme(axis.title.y=element_blank())+
        scale_fill_manual(values = scale_map, guide = FALSE) +
        scale_alpha_manual(name = "Type", values = c("OECM" = 0.5, "PA" = 1)) +
        scale_x_continuous(expand = c(0,0), limits=c(0,110))  +
        theme(legend.position='none')
      land

      water <-ggplot(data=dplyr::filter(eco_area_sum, type=="water"),
                     aes(x = p_type, y = fct_reorder(ecoregion_name, p_region, .desc=FALSE),
                         fill = type, alpha = park_type)) +
        theme_minimal(base_size = 10) +
        theme(panel.grid.major.y = element_blank(),
              legend.position = c(0.7, 0.5)) +
        geom_bar_interactive(aes(tooltip = tooltip, data_id = ecoregion_code),
                             width = 0.75, stat = "identity")+
        labs(x = lab_total_area) +
        theme(axis.title.y=element_blank())+
        scale_fill_manual(values = scale_map, guide = FALSE) +
        scale_alpha_manual(name = "Type", values = c("OECM" = 0.5, "PA" = 1)) +
        scale_x_continuous(expand = c(0,0), limits=c(0,110)) +
        theme(legend.position='none')
      water

      g2 <- plot_grid(land, water, ncol=1, align="v", rel_heights=c(4,1))

    #} else if (if(dplyr::filter(pa_eco, ecoregion_code == input$top_selected, type == "water" & type == "land")

    } else {
      region <- dplyr::filter(pa_eco, ecoregion_code == input$top_selected) %>%
        select(park_type, type_combo, geometry, ecoregion_name, type)
      r <- dplyr::filter(eco, ecoregion_code == input$top_selected) %>%
        pull(geometry)

      n <- region$ecoregion_name[1]
      # Top Right #2 - Ecoregion map
      if(length(unique(region$type))==2){
        s <- scale_map
        g2 <- ggplot(data = region) +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, size = 15),
                plot.margin = unit(c(0,0,0,0), "pt")) +
          geom_sf(data = r, fill = "grey80", colour = NA) +
          geom_sf(aes(fill = factor(type_combo)), colour = NA) +
          # scale_alpha_manual(name = "Type", values = c("OECM" = 0.5, "PA" = 1)) +
          scale_fill_manual(values = scale, guide = FALSE) +
          scale_x_continuous(expand = c(0,0)) +
          scale_y_continuous(expand = c(0,0)) +
          labs(title = " ") +
          guides(alpha = guide_legend(override.aes = list(fill = "black")))
      } else if (region$type[1] == "land"){
        s <- scale_land
        g2 <- ggplot(data = region) +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, size = 15),
                plot.margin = unit(c(0,0,0,0), "pt")) +
          geom_sf(data = r, fill = "grey80", colour = NA) +
          geom_sf(aes(fill = factor(park_type)), colour = NA) +
          scale_fill_manual(name = lab_oecm, values = s, guide = FALSE) +
          scale_x_continuous(expand = c(0,0)) +
          scale_y_continuous(expand = c(0,0)) +
          labs(title = " ")+
          guides(alpha = guide_legend(override.aes = list(fill = "#056100")))
      } else {
        s <- scale_water
      g2 <- ggplot(data = region) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, size = 15),
              plot.margin = unit(c(0,0,0,0), "pt")) +
        geom_sf(data = r, fill = "grey80", colour = NA) +
        geom_sf(aes(fill = factor(park_type)), colour = NA) +
        scale_fill_manual(name = lab_oecm, values = s, guide = FALSE) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        labs(title = " ")+
        guides(alpha = guide_legend(override.aes = list(fill = "#0a7bd1")))
      }

      g2 <- ggdraw(g2) +
        draw_plot(bc_button, x = 0.9, y = 0.9, width = 0.1, height = 0.1,
                  hjust = 0, vjust = 0) +
        draw_label(n, x = 0.5, y = 0.98, size = 16, colour = "black")
    }

    g <- plot_grid(g1, g2, nrow = 1)

    # If selection no longer visible
    if(is.null(input$top_selected) ||
       !input$top_selected %in% eco$ecoregion_code) {
      selected <- NULL
    } else selected <- input$top_selected

    girafe(ggobj = g, width_svg = app_width/72, height_svg = top_height/72,
           fonts = list(sans = "Roboto"),
           options = list(opts_hover(css = glue("fill:{hover};fill-opacity:1;")),
                          opts_selection(selected = selected,
                                         type = "single",
                                         css = glue("fill:{select};")),
                          opts_tooltip(css = tooltip_css, opacity = 1),
                          opts_toolbar(saveaspng = FALSE)))

  }) %>%
    bindCache(input$top_selected)




  # Bottom panel ------------------------------------------------------------
  output$bottom <- renderGirafe({

    if(is.null(input$top_selected) || input$top_selected == "reset") {
      # Bottom #1 - Provincial Area plot
      r <- yearly_sums
      g <- gg_area(r, type = "all")
    } else {
      # Bottom #2 - Ecoregion Area plot
      r <- dplyr::filter(eco_area, ecoregion_code == input$top_selected)
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

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


# Packages ----------------------------------------------------------------
library(shiny)
library(ggiraph)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(glue)
library(sf)


# Constants ---------------------------------------------------------------

# Data
eco <- readRDS("../out/eco_simp.rds")
pa_eco <- readRDS("../out/CPCAD_Dec2020_eco_simp.rds") %>%
  mutate(park_type = if_else(oecm == "Yes", "OECM", "PPA"))


#pa_eco_sum <- readRDS("../out/total_prot_area.rds") %>%
yearly_sums <- readRDS("../out/total_prot_area.rds") %>%
  mutate(tooltip_date = as.character(date),
         type_combo = glue("{tools::toTitleCase(type)} - {park_type}"),
         type_combo = factor(type_combo,
                             levels = c("Land - OECM", "Land - PPA",
                                        "Water - OECM", "Water - PPA"))) %>%
  group_by(date, type) %>%
  mutate(tooltip = glue(
    "<strong>Year:</strong> {tooltip_date}<br>",
    "<strong>Parks and Protected Areas:</strong> ",
    "{format(round(cum_year_type[park_type == 'PPA'], 2), big.mark = ',')} %<br>",
    "<strong>OECM:</strong> ",
    "{format(round(cum_year_type[park_type == 'OECM'], 2), big.mark = ',')} %")) %>%
  ungroup()

#readRDS("../out/pa_eco_sum.rds")
eco_area <- pa_eco_sum %>%
  mutate(tooltip_date = if_else(missing,
                                "Inc. unknown year of protection",
                                as.character(date))) %>%
  group_by(ecoregion_code, type, date) %>%
  mutate(tooltip = glue(
    "<strong>Year:</strong> {tooltip_date}<br>",
    "<strong>Parks and Protected Areas:</strong> ",
    "{format(round(cum_p_type[park_type == 'PPA'], 2), big.mark = ',')} %<br>",
    "<strong>OECM:</strong> ",
    "{format(round(cum_p_type[park_type == 'OECM'], 2), big.mark = ',')} %")) %>%
  ungroup()

eco_area_sum <- eco_area %>%
  select(ecoregion_code, ecoregion_name, park_type, type, p_type, p_region) %>%
  distinct() %>%
  group_by(ecoregion_code, type) %>%
  mutate(type_combo = glue("{tools::toTitleCase(type)} - {park_type}"),
         type_combo = factor(type_combo,
                             levels = c("Land - OECM", "Land - PPA",
                                        "Water - OECM", "Water - PPA")),
         tooltip = glue("<strong>Region:</strong> {ecoregion_name}<br>",
                        "<strong>Parks and Protected Areas:</strong> ",
                        "{format(round(p_type[park_type == 'PPA'], 1), big.mark = ',')}%<br>",
                        "<strong>OECM:</strong> ",
                        "{format(round(p_type[park_type == 'OECM'], 1), big.mark = ',')}%"))

# Add tool tip to map so they match
eco <- select(eco_area_sum, ecoregion_code, p_region, type, tooltip) %>%
  distinct() %>%
  left_join(eco, ., by = "ecoregion_code")

bc_button <- readRDS("../out/bc_button.rds")

# Sizes
app_width <- 900
bottom_width <- 900

top_height <- 500
bottom_height <- 200

# Styling - Matches msw-disposal-indicator
tooltip_css <- "background: white; opacity: 1; color: black; border-radius: 5px;
                padding: 5px; box-shadow: 3px 3px 5px 0px #888888;
                font-size: 12px; border-width 2px; border-color: black;"

# Colours - Somewhat matches msw-disposal-indicator and original
scale_land <- c("OECM" = "#93c288", "PPA" = "#004529")
scale_water <- c("OECM" = "#8bc3d5", "PPA" = "#063c4e")
scale_map <- c("land" = "#056100", "water" = "#0a7bd1")
scale_combo <- setNames(c(scale_land, scale_water),
                        c("Land - OECM", "Land - PPA",
                          "Water - OECM", "Water - PPA"))

hover <- "#FFFF99"
select <- "#FFFFFF"

# Labels
lab_total_area <- "Percent Area Protected"
lab_year <- "Year"
lab_growth <- "Cumulative\n% Protected"
lab_oecm <- "Type"

# Points and lines
size_pt <- 2
size_line <- 1
size_line_missing <- 0.5

# Functions ---------------------------------------------------------------

# https://stackoverflow.com/a/39877048/3362144
breaks_int <- function(x) {
  unique(floor(pretty(seq(min(x), (max(x))))))
}

gg_area <- function(data, type = "region") {

  if(type == "all") {
    scale <- scale_combo
  } else if(data$type[1] == "land") {
    scale <- scale_land
  } else {
    scale <- scale_water
  }

  data <- data %>%
    group_by(type, date) %>%
    arrange(date, desc(park_type)) %>%
    mutate(point = cumsum(cum_p_type)) %>%
    ungroup()

  g <- ggplot(data = data, aes(x = date, y = cum_p_type)) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor.x = element_blank(),
          axis.line = element_line(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = if_else(type == "region", "bottom", "none"),
          legend.box.margin = margin(),
          legend.margin = margin(),
          plot.margin = unit(c(5,10,0,0), "pt"),
          strip.background = element_blank(),
          strip.text = element_blank()) +
    geom_area(aes(fill = park_type)) +
    geom_col_interactive(aes(y = +Inf, tooltip = tooltip), fill = "grey",
                         na.rm = TRUE, show.legend = FALSE, alpha = 0.01, width = 1) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.01)),
                       breaks = breaks_int) +
    scale_fill_manual(name = "Type", values = scale) +
    labs(x = lab_year, y = lab_growth)+#,
         #subtitle = if_else(any(data$missing),
                            #"Inc. areas with unknown date of protection (*)",
                            #"")) +
    coord_cartesian(xlim = c(min(data$date), max(data$date)))

  if(type == "all") {
    g <- g + facet_wrap(~ type, nrow = 1)
  } else lim <- NULL

  g <- g + scale_y_continuous(expand = expansion(mult = c(0, 0.05)))


  g
}

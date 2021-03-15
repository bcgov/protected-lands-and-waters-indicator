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

eco_area <- readRDS("../out/eco_area.rds") %>%
  mutate(tooltip = glue("<strong>Year:</strong> {tooltip_date}<br>",
                        "<strong>Cumulative Area Protected:</strong> ",
                        "{format(round(cum_p_type, 2), big.mark = ',')} %"))

eco_area_sum <- eco_area %>%
  select(ecoregion_code, ecoregion_name, park_type, type, p_type, p_region) %>%
  distinct() %>%
  group_by(ecoregion_code) %>%
  mutate(type_combo = glue("{tools::toTitleCase(type)} - {park_type}"),
         type_combo = factor(type_combo,
                             levels = c("Land - PPA", "Land - OECM",
                                        "Water - PPA", "Water - OECM")),
         tooltip = glue("<strong>Region:</strong> {ecoregion_name}<br>",
                        "<strong>Protected:</strong> ",
                        "{format(round(p_region, 1), big.mark = ',')}%"))

# Add tool tip to map so they match
eco <- select(eco_area_sum, ecoregion_code, tooltip) %>%
  left_join(eco, ., by = "ecoregion_code")


# Sizes
app_width <- 900
bottom_width <- 700

top_height <- 500
bottom_height <- 200

# Styling - Matches msw-disposal-indicator
tooltip_css <- "background: white; opacity: 1; color: black; border-radius: 5px;
                padding: 5px; box-shadow: 3px 3px 5px 0px #888888;
                font-size: 12px; border-width 2px; border-color: black;"

# Colours - Somewhat matches msw-disposal-indicator and original
scale_land <- c("OECM" = "#93c288", "PPA" = "#004529")
scale_water <- c("OECM" = "#8bc3d5", "PPA" = "#063c4e")
scale_map_fill <- c(scale_land[[2]], scale_water[[2]])
scale_map_colour <- c(scale_land[[1]], scale_water[[1]])
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
  unique(floor(pretty(seq(min(x), (max(x) + 1)))))
}

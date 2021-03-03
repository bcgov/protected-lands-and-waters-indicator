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
library(patchwork)
library(glue)


# Constants ---------------------------------------------------------------

options(scipen = 999) # No scientific notation in plots

# Sizes
app_width <- 900
top_left_width <- 500
top_right_width <- 400

top_height <- 650
bottom_height <- 200


# Data
eco <- readRDS("../data/eco_simp.rds")
pa_eco <- readRDS("../data/CPCAD_Dec2020_eco_simp.rds") %>%
  mutate(park_type = if_else(oecm == "Yes", "OECM", "PPA"))
eco_area <- readRDS("../data/area_eco.rds")
eco_area_sum <- readRDS("../data/area_eco_sum.rds") %>%
  mutate(eco_names = factor(ecoregion_name, levels = rev(sort(unique(ecoregion_name)))))

# Colours
scale_land <- c("OECM" = "#004529", "PPA" = "#93c288")
scale_water <- c("OECM" = "#063c4e", "PPA" = "#8bc3d5")
scale_map_fill <- c(scale_land[[2]], scale_water[[2]])
scale_map_colour <- c(scale_land[[1]], scale_water[[1]])
scale_combo <- setNames(c(scale_land, scale_water),
                        c("Land - OECM", "Land - PPA",
                          "Water - OECM", "Water - PPA"))

hover <- "#FFFF99"
select <- "#FFFFFF"

# Labels
lab_total_area <- "Total Area Protected (ha)"
lab_year <- "Year"
lab_growth <- "Annual Growth (ha)"
lab_oecm <- "Type"




# Functions ---------------------------------------------------------------



# https://stackoverflow.com/a/39877048/3362144
break_int <- function(x) unique(floor(pretty(seq(min(x), (max(x) + 1)))))

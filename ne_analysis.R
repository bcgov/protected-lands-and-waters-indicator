# Copyright 2016 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(rgeos)
library(raster)
library(bcmaps)
library(maptools)

# data manipulation packages
library(dplyr) # summarizing data frames
library(tidyr) # for 'complete' function
library(readr)

# plotting
library(ggplot2)
library(ggthemes)

## Load some miscellaneous functions we need
source("fun.R")

m_to_ha <- function(x) x * 1e-4

## Need to run 02_clean.R first
## Load the cleanup up data from 02_clean.R
load("tmp/prot_areas_clean.rda")

ne_rd <- regional_districts_analysis[regional_districts_analysis$region_name %in%
                                       c("Northern Rockies", "Peace River"), ]
ne_rd$area <- rgeos::gArea(ne_rd, byid = TRUE)

prot_areas_ne <- raster::intersect(ne_rd, prot_areas_agg)
prot_areas_ne <- rgeos::createSPComment(prot_areas_ne) # Ensure polygon holes are properly identified

## Calculate size of protected areas in each ecoregion
prot_areas_ne$prot_area <- rgeos::gArea(prot_areas_ne, byid = TRUE)

## Summarize amount of area protected in each region each year
prot_areas_ne_summary_by_year <- prot_areas_ne@data %>%
  filter(prot_date > 0) %>%
  complete(nesting(region_name, area), prot_date, fill = list(prot_area = 0)) %>%
  group_by(region = region_name, year = prot_date) %>%
  summarise(region_area = min(area),
            tot_protected = sum(prot_area)) %>%
  ungroup() %>%
  mutate(percent_protected = tot_protected / region_area * 100)

## Calculate cumulative amount and percent protected over time
cum_summary_ne <- prot_areas_ne_summary_by_year %>%
  group_by(region) %>%
  arrange(year) %>%
  mutate(cum_area_protected = cumsum(tot_protected),
         cum_percent_protected = cumsum(percent_protected)) %>%
  filter(!is.na(cum_area_protected)) %>%
  ungroup()

## Make decades
prot_areas_ne$decade <- floor(prot_areas_ne$prot_date / 10) * 10
prot_areas_ne_decade <- raster::aggregate(prot_areas_ne, by = "decade")

## Visualize
## Make a cumulative dataframe of protected areas by decade
gg_cum_ne_prot_decade <- lapply(prot_areas_ne_decade$decade, function(x) {
  dec <- prot_areas_ne_decade[prot_areas_ne_decade$decade <= x, ]
  dec$cum_dec <- paste0(x, "'s")
  dec <- raster::aggregate(dec, "cum_dec")
  gg_fortify(dec)
}) %>%
  bind_rows()

## Make a gg data frame of NE regional districts, one for each level of decade
gg_ne_rd <- gg_fortify(ne_rd)
gg_ne_rd <- lapply(unique(gg_cum_ne_prot_decade$cum_dec), function(x) {
  newgg_ne_rd <- gg_ne_rd
  newgg_ne_rd$cum_dec <- x
  newgg_ne_rd
}) %>%
  bind_rows()

(map_plot <- ggplot(gg_cum_ne_prot_decade, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "forestgreen") +
  facet_wrap(~cum_dec, nrow = 2) +
  geom_polygon(data = gg_ne_rd, fill = NA, colour = "black") +
  coord_equal() +
  theme_map())


(line_plot <- ggplot(cum_summary_ne, aes(x = year, y = cum_percent_protected, colour = region)) +
    geom_line(size = 1) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = scales::pretty_breaks(8)) +
    scale_y_continuous(breaks = scales::pretty_breaks(6))+
    labs(x = "Year", y = "Cumulative percent protected", colour = "Region") +
    theme_bw())

## Current summary by designation
## Aggregate layers by designation (bc_admin_lands_agg already done)
bc_carts_des <- raster::aggregate(bc_carts, by = "TYPE_E")
fee_simple_des <- raster::aggregate(fee_simple_ngo_lands, by = "SecType1")

## Intersect each with BEC and calculate area
fee_simple_ne <- raster::intersect(ne_rd, fee_simple_des)
fee_simple_ne$prot_area <- rgeos::gArea(fee_simple_ne, byid = TRUE)

bc_admin_lands_ne <- raster::intersect(ne_rd, bc_admin_lands_agg)
bc_admin_lands_ne$prot_area <- rgeos::gArea(bc_admin_lands_ne, byid = TRUE)

bc_carts_ne <- raster::intersect(ne_rd, bc_carts_des)
bc_carts_ne$prot_area <- rgeos::gArea(bc_carts_ne, byid = TRUE)

# Summarize fee simple
fee_simple_ne_summary <- fee_simple_ne@data %>%
  group_by(region_name) %>%
  summarize(prot_area_ha = round(sum(prot_area) * 1e-4, 3)) %>%
  mutate(category = "Private Conservation Lands",
         designation = "Fee Simple")

# Summarize admin areas
admin_lands_ne_summary <- bc_admin_lands_ne@data %>%
  group_by(region_name, designation = TENURE_TYPE) %>%
  summarize(prot_area_ha = round(sum(prot_area) * 1e-4, 3)) %>%
  mutate(category = "BC Administered Lands")

# Summarize carts data
bc_carts_designations_categories <- bc_carts@data %>%
  mutate(category = ifelse(OWNER_E == "Government of British Columbia",
                           "Provincial Protected Lands & Waters",
                           "Federal Protected Lands & Waters")) %>%
  group_by(category, designation = TYPE_E) %>%
  summarise(n = n())

bc_carts_ne_summary <- bc_carts_ne@data %>%
  left_join(bc_carts_designations_categories[, -3], by = c("TYPE_E" = "designation")) %>%
  group_by(category, designation = TYPE_E, region_name) %>%
  summarize(prot_area_ha = round(sum(prot_area) * 1e-4, 3))


## Combine them all
designations_ne <- bind_rows(fee_simple_ne_summary, admin_lands_ne_summary,
                             bc_carts_ne_summary) %>%
  left_join(ne_rd@data, by = "region_name") %>%
  mutate(region_area_ha = area * 1e-4,
         percent_protected = round(prot_area_ha / region_area_ha * 100, 3)) %>%
  select(Region = region_name, "Region total area (ha)" = region_area_ha,
         Category = category,
         Designation = designation,
         "Protected area (ha)" = prot_area_ha,
         "Percent protected" = percent_protected) %>%
  arrange(Region, Category, Designation)

dir.create("out/ne", showWarnings = FALSE, recursive = TRUE)
ggsave("out/ne/map_ne_PAs_by_decade.png", map_plot, width = 8, height = 6, units = "in", dpi = 300)
ggsave("out/ne/line_plot_ne_prot_area.png", line_plot, width = 8, height = 3, units = "in", dpi = 300)

cum_summary_ne %>%
  arrange(region, year) %>%
  rename(Region = region,
         Year = year,
         "Region total area" = region_area,
         "Area protected" = tot_protected,
         "Percent protected" = percent_protected,
         "Cumulative area protected" = cum_area_protected,
         "Cumulative percent protected" = cum_percent_protected) %>%
  write_csv("out/ne/cumulative_percent_protected_ne.csv")

write_csv(designations_ne, "out/ne/protected_designations_summary_ne.csv")

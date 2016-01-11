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

# Spatial packages
library(rgeos)
library(raster)
library(bcmaps)

library(readr) # writing csv files
library(dplyr) # summarizing data frames
library(tidyr) # for 'complete' function
library(ggplot2) # plotting

source("fun.R")

## Load the cleanup up data from 02_clean.R
load("tmp/input_layers.rda")

## Get the earliest year of protection for polygon segments that overlap
bc_carts_t_unioned$prot_date <- get_unioned_attribute(bc_carts_t_unioned, bc_carts_t, "PROTDATE", min, "numeric", na.rm = TRUE)

## Get areas of unioned polygons
bc_carts_t_unioned$prot_area <- gArea(bc_carts_t_unioned, byid = TRUE)

## Aggregate the protected areas by their protected date
bc_carts_agg <- raster::aggregate(bc_carts_t_unioned[, "prot_date"], by = c("prot_date"))

###

## Calculate the area of the ecoregions
ecoregions$area <- rgeos::gArea(ecoregions, byid = TRUE)

## Intersect ecoregions with protected areas
carts_eco <- raster::intersect(ecoregions, bc_carts_agg)
carts_eco <- rgeos::createSPComment(carts_eco) # Ensure polygon holes are properly identified

## Calculate size of protected areas in each ecoregion
carts_eco$prot_area <- rgeos::gArea(carts_eco, byid = TRUE)


bc_area_sq_m <- 9.44735e11

## Summarize
carts_eco_summary_by_year <- carts_eco@data %>%
  filter(prot_date > 0) %>%
  complete(c(CRGNNM, area), prot_date, fill = list(prot_area = 0)) %>%
  group_by(ecoregion = CRGNNM, ecoregion_code = CRGNCD, prot_date) %>%
  summarise(ecoregion_area = min(area),
            tot_protected = sum(prot_area),
            percent_protected = tot_protected / ecoregion_area * 100)

## Provincial summary
carts_bc_summary_by_year <- bc_carts_t_unioned@data %>%
  filter(prot_date > 0) %>%
  group_by(prot_date) %>%
  summarise(ecoregion = "British Columbia",
            ecoregion_code = "BC",
            ecoregion_area = bc_area_sq_m,
            tot_protected = sum(prot_area),
            percent_protected = tot_protected / ecoregion_area * 100)

cum_summary <- bind_rows(carts_eco_summary_by_year, carts_bc_summary_by_year) %>%
  group_by(ecoregion, ecoregion_code) %>%
  arrange(prot_date) %>%
  mutate(cum_area_protected = fill_initial_na(cumsum(tot_protected)),
         cum_percent_protected = fill_initial_na(cumsum(percent_protected)),
         type = "All conservation lands",
         prot_date_full = paste0(prot_date, "-01-01")) %>%
  filter(!is.na(cum_area_protected))

ggplot(cum_summary, aes(x = prot_date, y = cum_percent_protected)) +
  geom_path() +
  facet_wrap(~ecoregion)

ggplot(cum_summary, aes(x = prot_date, y = cum_area_protected)) +
  geom_path() +
  facet_wrap(~ecoregion)

bc_designation_summary <- bc_carts_t@data %>%
  group_by(Designation = TYPE_E) %>%
  summarise(total_area_ha = sum(O_AREA),
            percent_of_bc = total_area_ha / (bc_area_sq_m / 1e4) * 100) %>%
  bind_rows(data_frame(Designation = "British Columbia Total",
                       total_area_ha = sum(.$total_area_ha),
                       percent_of_bc = sum(.$percent_of_bc))) %>%
  mutate(percent_of_bc = round(percent_of_bc, 4))

bc_designation_iucn_summary <- bc_carts_t@data %>%
  group_by(TYPE_E, IUCN_CAT) %>%
  summarise(total_area_ha = sum(O_AREA),
            percent_of_bc = total_area_ha / (bc_area_sq_m / 1e4) * 100) %>%
  bind_rows(data_frame(IUCN_CAT = "British Columbia Total",
                       total_area_ha = sum(.$total_area_ha),
                       percent_of_bc = sum(.$percent_of_bc))) %>%
  mutate(percent_of_bc = round(percent_of_bc, 4))

bc_iucn_summary <- bc_carts_t@data %>%
  group_by(IUCN_CAT) %>%
  summarise(total_area_ha = sum(O_AREA),
            percent_of_bc = total_area_ha / (bc_area_sq_m / 1e4) * 100) %>%
  bind_rows(data_frame(IUCN_CAT = "British Columbia Total",
                       total_area_ha = sum(.$total_area_ha),
                       percent_of_bc = sum(.$percent_of_bc))) %>%
  mutate(percent_of_bc = round(percent_of_bc, 4))

write_csv(cum_summary, path = "out/ecoregion_cons_lands_trends.csv")
write_csv(bc_designation_summary, path = "out/bc_carts_designation_summary.csv")
write_csv(bc_iucn_summary, path = "out/bc_carts_iucn_summary.csv")

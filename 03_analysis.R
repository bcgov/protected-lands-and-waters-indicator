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

# data manipulation packages
library(dplyr) # summarizing data frames
library(tidyr) # for 'complete' function

## Load the functions we need
source("fun.R")

## Load the cleanup up data from 02_clean.R
load("tmp/prot_areas_clean.rda")
load("tmp/ecoregions_clean.rda")
reg_int_ecoreg_summary <- read.csv("data/reg_interests_ecoreg_summary_year.csv", stringsAsFactors = FALSE)

# Terrestrial Ecoregion analysis -----------------------------------------------

## Intersect ecoregions with protected areas
prot_areas_eco_t <- raster::intersect(ecoregions_t, prot_areas_agg)
prot_areas_eco_t <- rgeos::createSPComment(prot_areas_eco_t) # Ensure polygon holes are properly identified

## Calculate size of protected areas in each ecoregion
prot_areas_eco_t$prot_area <- rgeos::gArea(prot_areas_eco_t, byid = TRUE)

bc_area_sq_m <- bc_area(units = "m2")

## Summarize
prot_areas_eco_t_summary_by_year <- prot_areas_eco_t@data %>%
  filter(prot_date > 0) %>%
  complete(nesting(CRGNNM, CRGNCD, area), prot_date, fill = list(prot_area = 0)) %>%
  group_by(ecoregion = CRGNNM, ecoregion_code = CRGNCD, prot_date) %>%
  summarise(ecoregion_area = min(area),
            tot_protected = sum(prot_area)) %>%
  ungroup() %>%
  left_join(select(reg_int_ecoreg_summary, ecoregion_code, prot_date, tot_protected),
            by = c("ecoregion_code", "prot_date")) %>%
  mutate(tot_protected.y = ifelse(is.na(tot_protected.y), 0, tot_protected.y),
         tot_protected = (tot_protected.x + tot_protected.y),
         percent_protected = tot_protected / ecoregion_area * 100) %>%
  select(-tot_protected.x, -tot_protected.y)


## Provincial summary
prot_areas_bc_t_summary_by_year <- prot_areas_eco_t_summary_by_year %>%
  group_by(prot_date) %>%
  summarise(ecoregion = "British Columbia",
            ecoregion_code = "BC",
            ecoregion_area = bc_area_sq_m,
            tot_protected = sum(tot_protected),
            percent_protected = tot_protected / ecoregion_area * 100)

cum_summary_t <- bind_rows(prot_areas_eco_t_summary_by_year, prot_areas_bc_t_summary_by_year) %>%
  group_by(ecoregion, ecoregion_code) %>%
  arrange(prot_date) %>%
  mutate(cum_area_protected = cumsum(tot_protected),
         cum_percent_protected = cumsum(percent_protected),
         type = "All conservation lands",
         prot_date_full = paste0(prot_date, "-01-01")) %>%
  filter(!is.na(cum_area_protected)) %>%
  ungroup()

# Marine Ecoregion analysis ----------------------------------------------------

## Intersect ecoregions with protected areas
prot_areas_eco_m <- raster::intersect(ecoregions_m, prot_areas_agg)
prot_areas_eco_m <- rgeos::createSPComment(prot_areas_eco_m) # Ensure polygon holes are properly identified

## Calculate size of protected areas in each ecoregion
prot_areas_eco_m$prot_area <- rgeos::gArea(prot_areas_eco_m, byid = TRUE)

#bc_area_sq_m <- bc_area(units = "m2")

## Summarize
missing_m_ecoregions <- ecoregions_m@data %>%
  select(CRGNNM, CRGNCD, area) %>%
  filter(CRGNCD %in% c("TPC", "SBC")) %>%
  mutate(prot_date = 2013, prot_area = 0)

prot_areas_eco_m_summary_by_year <- prot_areas_eco_m@data %>%
  filter(prot_date > 0) %>%
  bind_rows(missing_m_ecoregions) %>%
  complete(nesting(CRGNNM, CRGNCD, area), prot_date, fill = list(prot_area = 0)) %>%
  group_by(ecoregion = CRGNNM, ecoregion_code = CRGNCD, prot_date) %>%
  summarise(ecoregion_area = min(area),
            tot_protected = sum(prot_area),
            percent_protected = tot_protected / ecoregion_area * 100)

## Provincial summary
prot_areas_bc_m_summary_by_year <- prot_areas_eco_m_summary_by_year %>%
  group_by(prot_date) %>%
  summarise(ecoregion = "British Columbia",
            ecoregion_code = "BC",
            ecoregion_area = sum(ecoregions_m$area),
            tot_protected = sum(tot_protected),
            percent_protected = tot_protected / ecoregion_area * 100)

cum_summary_m <- bind_rows(prot_areas_eco_m_summary_by_year, prot_areas_bc_m_summary_by_year) %>%
  group_by(ecoregion, ecoregion_code) %>%
  arrange(prot_date) %>%
  mutate(cum_area_protected = cumsum(tot_protected),
         cum_percent_protected = cumsum(percent_protected),
         type = "All conservation lands",
         prot_date_full = paste0(prot_date, "-01-01")) %>%
  filter(!is.na(cum_area_protected))


# Terrrestrial BEC -------------------------------------------------------------

## Get a simple percent protected of each Biogeoclimatic Zone

load("tmp/bec_clean.rda")
reg_int_bec_summary <- read.csv("data/reg_interests_bec_summary.csv")

# Intersect terrestrial CARTS and BEC and get area
prot_areas_bec <- raster::intersect(bec_t, prot_areas_agg)
prot_areas_bec$prot_area <- rgeos::gArea(prot_areas_bec, byid = TRUE)

# Get total size of terrestrial area of each zone
bec_t_summary <- bec_t@data %>%
  group_by(ZONE_NAME) %>%
  summarize(total_area = sum(area))

# Summarize
prot_areas_bec_summary <- prot_areas_bec@data %>%
  group_by(ZONE_NAME) %>%
  summarize(prot_area = sum(prot_area)) %>%
  left_join(reg_int_bec_summary, by = "ZONE_NAME") %>%
  left_join(bec_t_summary, by = "ZONE_NAME") %>%
  mutate(prot_area.y = ifelse(is.na(prot_area.y), 0, prot_area.y),
         prot_area = prot_area.x + prot_area.y,
         percent_protected = prot_area / total_area * 100) %>%
  select(-prot_area.x, -prot_area.y)

save.image(file = "tmp/analyzed.rda")

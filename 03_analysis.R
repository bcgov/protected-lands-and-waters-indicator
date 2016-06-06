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
library(readr)

## Load the functions we need
source("fun.R")

## Load the cleanup up data from 02_clean.R
load("tmp/prot_areas_clean.rda")
load("tmp/ecoregions_clean.rda")
load("tmp/bec_clean.rda")
reg_int_bec_summary <- read.csv("data/reg_interests_bec_summary.csv", stringsAsFactors = FALSE)
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
  left_join(select(reg_int_ecoreg_summary, ecoregion_code, prot_date, tot_protected_overlaps_removed),
            by = c("ecoregion_code", "prot_date")) %>%
  mutate(tot_protected_overlaps_removed = ifelse(is.na(tot_protected_overlaps_removed),
                                                 0, tot_protected_overlaps_removed),
         tot_protected = (tot_protected + tot_protected_overlaps_removed),
         percent_protected = tot_protected / ecoregion_area * 100) %>%
  select(-tot_protected_overlaps_removed)


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
  mutate(prot_date = max(prot_areas_eco_m$prot_date), prot_area = 0)

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

# Get total size of terrestrial area of each zone
bec_t_summary <- bec_t@data %>%
  group_by(ZONE_NAME) %>%
  summarize(total_area = sum(area))

# Intersect terrestrial CARTS and BEC and get area
prot_areas_bec <- raster::intersect(bec_t, prot_areas_agg)
prot_areas_bec$prot_area <- rgeos::gArea(prot_areas_bec, byid = TRUE)

# Summarize
prot_areas_bec_summary <- prot_areas_bec@data %>%
  group_by(ZONE_NAME) %>%
  summarize(prot_area = sum(prot_area)) %>%
  left_join(select(reg_int_bec_summary, ZONE_NAME, prot_area_overlaps_removed), by = "ZONE_NAME") %>%
  left_join(bec_t_summary, by = "ZONE_NAME") %>%
  mutate(prot_area_overlaps_removed = ifelse(is.na(prot_area_overlaps_removed),
                                             0, prot_area_overlaps_removed),
         prot_area = prot_area + prot_area_overlaps_removed,
         percent_protected = prot_area / total_area * 100) %>%
  select(-prot_area_overlaps_removed)


# Individual land designations by BEC -------------------------------------

bec_t_summary$total_zone_area_ha <- bec_t_summary$total_area * 1e-4

## Aggregate layers by designation (bc_admin_lands_agg already done)
bc_carts_des <- raster::aggregate(bc_carts, by = "TYPE_E")
fee_simple_des <- raster::aggregate(fee_simple_ngo_lands, by = "SecType1")

## Intersect each with BEC and calculate area
fee_simple_bec <- raster::intersect(bec_t, fee_simple_des)
fee_simple_bec$prot_area <- rgeos::gArea(fee_simple_bec, byid = TRUE)

bc_admin_lands_bec <- raster::intersect(bec_t, bc_admin_lands_agg)
bc_admin_lands_bec$prot_area <- rgeos::gArea(bc_admin_lands_bec, byid = TRUE)

bc_carts_bec <- raster::intersect(bec_t, bc_carts_des)
bc_carts_bec$prot_area <- rgeos::gArea(bc_carts_bec, byid = TRUE)

reg_int_bec_summary <- reg_int_bec_summary %>%
  left_join(bec_t_summary, by = "ZONE_NAME") %>%
  mutate(category = "Private Conservation Lands",
         designation = "Registerable Interests",
         prot_area_ha = round(prot_area * 1e-4, 3),
         percent_protected = round((prot_area_ha / total_zone_area_ha * 100), 4)) %>%
  dplyr::select(-prot_area_overlaps_removed, -prot_area)

# Summarize fee simple
fee_simple_bec_summary <- fee_simple_bec@data %>%
  group_by(ZONE_NAME) %>%
  summarize(prot_area_ha = round(sum(prot_area) * 1e-4, 3)) %>%
  left_join(bec_t_summary, by = "ZONE_NAME") %>%
  mutate(category = "Private Conservation Lands",
         designation = "Fee Simple",
         percent_protected = round((prot_area_ha / total_zone_area_ha * 100), 4))

# Summarize admin areas
admin_lands_bec_summary <- bc_admin_lands_bec@data %>%
  group_by(ZONE_NAME, designation = TENURE_TYPE) %>%
  summarize(prot_area_ha = round(sum(prot_area) * 1e-4, 3)) %>%
  left_join(bec_t_summary, by = "ZONE_NAME") %>%
  mutate(category = "BC Administered Lands",
         percent_protected = round((prot_area_ha / total_zone_area_ha * 100), 4))

# Summarize carts data
bc_carts_bec_summary <- bc_carts_bec@data %>%
  group_by(ZONE_NAME, designation = TYPE_E) %>%
  summarize(prot_area_ha = round(sum(prot_area) * 1e-4, 3)) %>%
  left_join(bec_t_summary, by = "ZONE_NAME") %>%
  mutate(category = "Provincial and Federal Protected Lands",
         percent_protected = round((prot_area_ha / total_zone_area_ha * 100), 4))

designations_bec <- bind_rows(reg_int_bec_summary, fee_simple_bec_summary,
                              admin_lands_bec_summary, bc_carts_bec_summary)

# Individual Designations with Ecoregions ---------------------------------

ecoregions$area <- rgeos::gArea(ecoregions, byid = TRUE)
ecoregion_summary <- ecoregions@data %>%
  group_by(CRGNCD) %>%
  summarize(total_ecoregion_area_ha = sum(area) * 1e-4)

## Intersect each with Ecoregions and calculate area
fee_simple_eco <- raster::intersect(ecoregions, fee_simple_des)
fee_simple_eco$prot_area <- rgeos::gArea(fee_simple_eco, byid = TRUE)

bc_admin_lands_eco <- raster::intersect(ecoregions, bc_admin_lands_agg)
bc_admin_lands_eco$prot_area <- rgeos::gArea(bc_admin_lands_eco, byid = TRUE)

bc_carts_eco <- raster::intersect(ecoregions, bc_carts_des)
bc_carts_eco$prot_area <- rgeos::gArea(bc_carts_eco, byid = TRUE)

reg_int_eco_summary <- reg_int_ecoreg_summary %>%
  group_by(ecoregion, ecoregion_code) %>%
  summarize(tot_protected = sum(tot_protected)) %>%
  left_join(ecoregion_summary, by = c("ecoregion_code" = "CRGNCD")) %>%
  mutate(category = "Private Conservation Lands",
         designation = "Registerable Interests",
         prot_area_ha = round(tot_protected * 1e-4, 3),
         percent_protected = round((prot_area_ha / total_ecoregion_area_ha * 100), 4)) %>%
  dplyr::select(-tot_protected)

# Summarize fee simple
fee_simple_eco_summary <- fee_simple_eco@data %>%
  group_by(CRGNNM, CRGNCD) %>%
  summarize(prot_area_ha = round(sum(prot_area) * 1e-4, 3)) %>%
  left_join(ecoregion_summary, by = "CRGNCD") %>%
  mutate(category = "Private Conservation Lands",
         designation = "Fee Simple",
         percent_protected = round((prot_area_ha / total_ecoregion_area_ha * 100), 4))

# Summarize admin areas
admin_lands_eco_summary <- bc_admin_lands_eco@data %>%
  group_by(CRGNNM, CRGNCD, designation = TENURE_TYPE) %>%
  summarize(prot_area_ha = round(sum(prot_area) * 1e-4, 3)) %>%
  left_join(ecoregion_summary, by = "CRGNCD") %>%
  mutate(category = "BC Administered Lands",
         percent_protected = round((prot_area_ha / total_ecoregion_area_ha * 100), 4))

# Summarize carts data
bc_carts_eco_summary <- bc_carts_eco@data %>%
  group_by(CRGNNM, CRGNCD, designation = TYPE_E) %>%
  summarize(prot_area_ha = round(sum(prot_area) * 1e-4, 3)) %>%
  left_join(ecoregion_summary, by = "CRGNCD") %>%
  mutate(category = "Provincial and Federal Protected Lands",
         percent_protected = round((prot_area_ha / total_ecoregion_area_ha * 100), 4))

designations_eco <- bind_rows(reg_int_eco_summary, fee_simple_eco_summary,
                              admin_lands_eco_summary, bc_carts_eco_summary) %>%
  mutate(ecoregion = ifelse(is.na(ecoregion), CRGNNM, ecoregion),
         ecoregion_code = ifelse(is.na(ecoregion_code), CRGNCD, ecoregion_code)) %>%
  select(-CRGNNM, -CRGNCD)

to_save <- c("reg_int_ecoreg_summary", "prot_areas_eco_t",
             "prot_areas_eco_t_summary_by_year", "prot_areas_bc_t_summary_by_year",
             "cum_summary_t", "prot_areas_eco_m", "missing_m_ecoregions",
             "prot_areas_eco_m_summary_by_year", "prot_areas_bc_m_summary_by_year",
             "cum_summary_m", "reg_int_bec_summary", "prot_areas_bec",
             "bec_t_summary", "prot_areas_bec_summary", "bc_carts_des",
             "fee_simple_bec", "bc_admin_lands_bec", "bc_carts_bec",
             "reg_int_bec_summary", "fee_simple_bec_summary",
             "admin_lands_bec_summary", "bc_carts_bec_summary",
             "designations_bec", "fee_simple_eco","bc_admin_lands_eco",
             "bc_carts_eco", "fee_simple_eco_summary", "bc_carts_eco_summary",
             "reg_int_eco_summary", "admin_lands_eco_summary", "designations_bec",
             "designations_eco")

save(list = to_save, file = "tmp/analyzed.rda")

## Output csv files
options(scipen = 5)

## Prep summary for interactive web viz
cum_summary_t$ecoregion <- tools::toTitleCase(tolower(cum_summary_t$ecoregion))
cum_summary_t_viz <- cum_summary_t[cum_summary_t$tot_protected > 0, ]
write_csv(cum_summary_t_viz, path = "out/ecoregion_cons_lands_trends.csv")


## Write out designation by BEC/Ecoregion summaries
write_csv(designations_bec, "out/land_designations_bec_zone.csv")
write_csv(designations_eco, "out/land_designations_ecoregion.csv")

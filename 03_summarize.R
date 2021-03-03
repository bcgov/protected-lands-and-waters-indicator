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

# Load packages etc.
source("00_setup.R")

marine_eco <- c("HCS", "IPS", "OPS", "SBC", "TPC", "GPB")

# Load data
eco <- ecoregions(ask = FALSE) %>%
  rename_all(tolower) %>%
  select(ecoregion_code, ecoregion_name) %>%
  mutate(ecoregion_name = tools::toTitleCase(tolower(ecoregion_name)),
         type = if_else(ecoregion_code %in% marine_eco, "water", "land"))

bec <- bec(ask = FALSE) %>%
  rename_all(tolower) %>%
  select(zone, subzone, zone_name, subzone_name, natural_disturbance_name)

pa <- read_rds("data/CPCAD_Dec2020_BC_clean_no_ovlps.rds")

# Add ecoregions ------------------------------------------------------------
message("Add eco regions")
pa_eco <- st_intersection(eco, pa)
write_rds(pa_eco, "data/CPCAD_Dec2020_BC_clean_no_ovlps_ecoregions.rds")

# Add bec zones ------------------------------------------------------------
message("Add bec zones")
pa_bec <- st_intersection(bec, pa)
write_rds(pa_bec, "data/CPCAD_Dec2020_BC_clean_no_ovlps_beczones.rds")

# Remove LINESTRING shapes ------------------------------------------------
# see share/99_checks_non_polygons.html for rational
#pa_eco <- read_rds("data/CPCAD_Dec2020_BC_clean_no_ovlps_ecoregions.rds")
#pa_bec <- read_rds("data/CPCAD_Dec2020_BC_clean_no_ovlps_beczones.rds")

message("Fix linestrings")
pa_eco_poly <- pa_eco %>%
  filter(!str_detect(st_geometry_type(.), "POLYGON")) %>% # Get GEOMETRYCOLLECTIONS
  st_collection_extract(type = "POLYGON")  # Get only POLYGONS from collections (omit LINESTRINGS)

pa_eco <- pa_eco %>%
  filter(str_detect(st_geometry_type(.), "POLYGON")) %>%  # Remove GEOMETRYCOLLECTIONS
  rbind(pa_eco_poly) %>%                                  # Add POLYGONS from collections
  verify(all(str_detect(st_geometry_type(.), "POLYGON"))) # Double check

rm(pa_eco_poly)

pa_bec_poly <- pa_bec %>%
  filter(!str_detect(st_geometry_type(.), "POLYGON")) %>% # Get GEOMETRYCOLLECTIONS
  st_collection_extract(type = "POLYGON")  # Get only POLYGONS from collections (omit LINESTRINGS)

pa_bec <- pa_bec %>%
  filter(str_detect(st_geometry_type(.), "POLYGON")) %>%  # Remove GEOMETRYCOLLECTIONS
  rbind(pa_bec_poly) %>%                                  # Add POLYGONS from collections
  verify(all(str_detect(st_geometry_type(.), "POLYGON"))) # Double check

rm(pa_bec_poly)

# Simplify data for plotting  ---------------------------------------------
#
# Simplification is great for plotting - see ./share/99_checks_simplification.html
# for justification
#
# Run by region/zone - Much faster and no crashing (on my computer at least)

message("Simplify - Eco regions")
eco_simp <- data.frame()
for(e in unique(pa_eco$ecoregion_code)) {
  message(e)
  region <- rmapshaper::ms_simplify(filter(pa_eco, ecoregion_code == e))
  eco_simp <- rbind(eco_simp, region)
}
rm(pa_eco)
write_rds(eco_simp, "data/CPCAD_Dec2020_eco_simp.rds")

message("Simplify - Bec Zones")
bec_simp <- data.frame()
for(z in unique(pa_bec$zone)) {
  message(z)
  zone <- rmapshaper::ms_simplify(filter(pa_bec, zone == z))
  bec_simp <- rbind(bec_simp, zone)
}
rm(pa_bec)
write_rds(bec_simp, "data/CPCAD_Dec2020_bec_simp.rds")

eco <- rmapshaper::ms_simplify(eco)
write_rds(eco, "data/eco_simp.rds")

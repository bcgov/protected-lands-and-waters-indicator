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

# NOTES
# - Working with BEC zones requires using mapshaper-xl on the command line
# - Install Node (https://nodejs.org)
# - Then install mapshaper (includes mapshaper-xl)
#   'npm install -g mapshaper'


# Load packages etc.
source("00_setup.R")

marine_eco <- c("HCS", "IPS", "OPS", "SBC", "TPC", "GPB")

# Load data
pa <- read_rds("data/CPCAD_Dec2020_BC_clean_no_ovlps.rds")

eco <- ecoregions(ask = FALSE) %>%
  rename_all(tolower) %>%
  select(ecoregion_code, ecoregion_name) %>%
  mutate(ecoregion_name = tools::toTitleCase(tolower(ecoregion_name)),
         type = if_else(ecoregion_code %in% marine_eco, "water", "land"))

bec <- bec(ask = FALSE) %>%
  rename_all(tolower) %>%
  select(zone, subzone, zone_name, subzone_name, natural_disturbance_name)

bc <- bc_bound_hres(ask = FALSE)

# Clip BEC to BC outline ----------------------------------------------------
# We'll need simplified becs for plotting later
# NOTE: geojson doesn't have CRS so have to remind R that CRS is BC Albers
#       (It will warn that it's not transforming)
message("Clip BEC to BC outline")

geojson_write(bec, file = "data/bec.geojson")
geojson_write(bc, file = "data/bc.geojson")

system(glue("mapshaper-xl data/bec.geojson ",
            "-clip data/bc.geojson remove-slivers ",
            "-o data/bec_clipped.geojson"))

system(glue("mapshaper-xl data/bec_clipped.geojson ",
            "-simplify 50% ",
            "-o data/bec_clipped_simp.geojson"))

# Add ecoregions to PA ------------------------------------------------------
message("Add eco regions")
pa_eco <- st_intersection(eco, pa)

# Add bec zones to PA -------------------------------------------------------
message("Add bec zones")
bec <- st_read("data/bec_clipped_simp.geojson", crs = 3005) %>%
  st_make_valid()
pa_bec <- st_intersection(bec, pa)

# Remove LINESTRING shapes ------------------------------------------------
# see share/99_checks_non_polygons.html for rational
message("Fix linestrings")

pa_eco <- st_collection_extract(pa_eco, type = "POLYGON")
pa_bec <- st_collection_extract(pa_bec, type = "POLYGON")

write_rds(pa_eco, "data/CPCAD_Dec2020_BC_clean_no_ovlps_ecoregions.rds")
write_rds(pa_bec, "data/CPCAD_Dec2020_BC_clean_no_ovlps_beczones.rds")

# pa_eco <- read_rds("data/CPCAD_Dec2020_BC_clean_no_ovlps_ecoregions.rds")
# pa_bec <- read_rds("data/CPCAD_Dec2020_BC_clean_no_ovlps_beczones.rds")


# Simplify ecoregions for plotting  -------------------------------------------
#
# Simplification is great for plotting
#  - see ./share/99_checks_simplification.html for justification
#
# Run by region/zone
#  - Much faster and no crashing (on my computer at least)
#  - Allows simplifying to different degrees for different regions

message("Simplify - Eco regions")

eco_simp <- slice(pa_eco, 0)
for(e in unique(pa_eco$ecoregion_code)) {
  message(e)
  temp <- filter(pa_eco, ecoregion_code == e)
  keep_shapes <- if_else(nrow(temp) <= 1000, TRUE, FALSE)
  keep <- case_when(nrow(temp) < 50 ~ 1,
                    nrow(temp) < 1000 ~ 0.1,
                    TRUE ~ 0.05)
  if(keep == 1) region <- temp else region <- ms_simplify(temp, keep = keep,
                                                          keep_shapes = keep_shapes)
  eco_simp <- rbind(eco_simp, region)
}
eco_simp <- filter(eco_simp, !st_is_empty(eco_simp))
write_rds(eco_simp, "out/CPCAD_Dec2020_eco_simp.rds")
rm(pa_eco)

# Simplify bec zones for plotting  --------------------------------------------
message("Simplify - Bec Zones")

geojson_write(pa_bec, file = "data/pa_bec.geojson")

system(glue("mapshaper-xl data/pa_bec.geojson ",
            "-simplify 5% keep-shapes ",
            "-o out/CPCAD_Dec2020_bec_simp.geojson"))

# Simplify ecoregions background map ------------------------------------------
eco <- ms_simplify(eco, keep = 0.01)
write_rds(eco, "out/eco_simp.rds")

# Simplify bec zones background map ------------------------------------------
system(glue("mapshaper-xl data/bec_clipped.geojson ",
            "-simplify 1% keep-shapes ",
            "-o out/bec_simp.geojson"))

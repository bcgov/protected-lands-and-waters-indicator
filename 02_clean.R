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

# Setup ----------------------------------------------------------------------
source("00_setup.R")

# Load and fix -------------------------------------------------------------
# Load data
ff <- "data/CPCAD-BDCAPC_Dec2020.gdb"
st_layers(ff)

pa <- st_read(ff, layer = "CPCAD_Dec2020") %>%
  rename_all(tolower)

# Filter to listed in BC or Pacific Ocean
unique(pa$loc_e)
pa <- filter(pa, str_detect(loc_e, "Pacific|British Columbia"))
unique(pa$loc_e)

# Remove those that are NOT AICHI_T11 and NOT OECM
filter(pa, (aichi_t11 == "No" & oecm == "No")) %>%
  pull(shape_area) %>%
  sum() / 10000 # Total removed in hectares

pa <- filter(pa, !(aichi_t11 == "No" & oecm == "No"))

# Fix problems
pa <- st_make_valid(pa)        # Fix Ring Self-intersections

# Clean Up --------------------------------------------------------------------
# - Calculate actual area, order IUCN codes
# - Sort in order of priority (most to least interesting)
#    oecm (No > Yes), iucn_cat (Ia > Ib > II etc.), protdate (older > earlier),
#    area_all (smaller > bigger)
pa <- pa %>%
  mutate(area_all = as.numeric(st_area(.)),
         iucn_cat = factor(iucn_cat, levels = c("Ia", "Ib", "II", "III", "IV",
                                                "V", "VI", "Yes", "N/A")),
         name_e = str_replace(name_e, "Widllife", "Wildlife")) %>%
  arrange(desc(oecm), iucn_cat, protdate, area_all)

# Save file for comparisons
write_rds(pa, "data/CPCAD_Dec2020_BC_clean.rds")

# Remove overlaps -------------------------------------------------------------
# Split multipolygons
# (faster to split - https://github.com/r-spatial/sf/issues/575#issuecomment-368960968)

#pa <- read_rds("data/CPCAD_Dec2020_BC_clean.rds")

pa_mult <- pa %>%
  st_cast(to = "POLYGON", warn = FALSE) %>%        # Split geometries
  mutate(area_single = as.numeric(st_area(.))) %>% # Calculate indiv area
  st_difference()                                  # Remove overlaps (~45min)
write_rds(pa_mult, "data/CPCAD_Dec2020_BC_no_ovlps.rds")

#pa_mult <- read_rds("data/CPCAD_Dec2020_BC_no_ovlps.rds")

# Add ecoregions ------------------------------------------------------------
eco <- ecoregions(ask = FALSE)
pa_mult <- st_transform(pa_mult, crs = st_crs(eco)) %>%
  st_make_valid()        # Fix Self-intersections (again!)
write_rds(pa_mult, "data/CPCAD_Dec2020_BC_clean_no_ovlps.rds")

pa_eco <- eco %>%
  clean_names() %>%
  select(ecoregion_code, ecoregion_name) %>%
  st_intersection(pa_mult)
write_rds(pa_eco, "data/CPCAD_Dec2020_BC_clean_no_ovlps_ecoregions.rds")

# Add bec zones ------------------------------------------------------------
pa_bec <- bec(ask = FALSE) %>%
  clean_names() %>%
  select(zone, subzone, zone_name, subzone_name, natural_disturbance_name) %>%
  st_intersection(pa_mult)
write_rds(pa_bec, "data/CPCAD_Dec2020_BC_clean_no_ovlps_beczones.rds")

write(as.character(Sys.time()), "time.log", append = TRUE)

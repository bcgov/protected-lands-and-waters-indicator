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

# Data is downloaded only if it doesn't already exist,
# optionally, clear existing data first
if(FALSE) {
  unlink(list.files("data", "CPCAD", full.names = TRUE))
  delete_cache()
}

# Get Protected Areas -----------------------------------------------------
# See https://www.canada.ca/en/environment-climate-change/services/national-wildlife-areas/protected-conserved-areas-database.html#toc1 for the current database

f <- "CPCAD-BDCAPC_Dec2020.gdb.zip"
ff <- file.path("data", str_remove(f, ".zip"))
if(!dir.exists(ff)){
  download.file(file.path("https://cws-scf.ca", f), destfile = f)
  unzip(f, exdir = "data")
  unlink(f)
}

# Fix -------------------------------------------------------------
# Load data
st_layers(ff)

pa <- st_read(ff, layer = "CPCAD_Dec2020") %>%
  rename_all(tolower)

# Filter to listed in BC or Pacific Ocean
pa <- filter(pa, str_detect(loc_e, "Pacific|British Columbia"))

# Remove those that are NOT AICHI_T11 and NOT OECM
filter(pa, (aichi_t11 == "No" & oecm == "No")) %>%
  pull(shape_area) %>%
  sum() / 10000 # Total removed in hectares

pa <- filter(pa, !(aichi_t11 == "No" & oecm == "No"))

# Fix problems
pa <- st_make_valid(pa)        # Fix Ring Self-intersections

# Save file for comparisons
write_rds(pa, "data/CPCAD_Dec2020_BC_fixed.rds")

# Pre-download maps from bcmaps  ----------------------------------------------
cache <- show_cached_files()$file
if(!any(str_detect(cache, "ecoregion"))) ecoregions(ask = FALSE)
if(!any(str_detect(cache, "bec"))) bec(ask = FALSE)
if(!any(str_detect(cache, "bc_bound_hres"))) bc_bound_hres(ask = FALSE)


# Download extra spatial for dates ----------------------------------------
if(!file.exists("data/wha.rds")) {
  bcdc_get_data("WHSE_WILDLIFE_MANAGEMENT.WCP_WILDLIFE_HABITAT_AREA_POLY") %>%
    write_rds("data/wha.rds")
}

if(!file.exists("data/ogma.rds")) {
  bcdc_query_geodata("WHSE_LAND_USE_PLANNING.RMP_OGMA_LEGAL_CURRENT_SVW") %>%
    collect() %>%
    write_rds("data/ogma.rds")
}


# Copyright 2025 Province of British Columbia
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
source(here("background_scripts", "00_setup.R"))

# Data is downloaded only if it doesn't already exist,
# optionally, clear existing data first
if(FALSE) {
  unlink(list.files("data", "CPCAD", full.names = TRUE))
  delete_cache()
}

# Get Protected Areas -----------------------------------------------------
# See https://www.canada.ca/en/environment-climate-change/services/national-wildlife-areas/protected-conserved-areas-database.html#toc1 for the current database

f <- "ProtectedConservedArea_2024.zip"
ff <- file.path("data", paste0(str_remove(f, ".zip"),".gdb"))
if(!dir.exists(ff)){
  download.file(paste0("https://data-donnees.az.ec.gc.ca/api/file?path=%2Fspecies%2Fprotectrestore%2Fcanadian-protected-conserved-areas-database%2FDatabases%2F", f), destfile = f)
  archive_extract(f, dir = "data")
  unlink(f)
}

# Fix -------------------------------------------------------------
# Load data
st_layers(ff)

pa <- st_read(ff, layer = "ProtectedConservedArea_2024") %>%
  rename_all(tolower)

# Filter to listed in BC or Pacific Ocean
pa <- pa %>%
  dplyr::filter(loc %in% c(2, 16, 19)) # 2 = British Columbia, 16 = Coastal Pacific Marine, 19 = Offshore Pacific Marine

# Remove those that are NOT AICHI_T11 and NOT OECM
dplyr::filter(pa, (pa_oecm_df %in% c(1:4))) %>% This includes PA, OECM, interim PA, and interim OECM.
  pull(shape_area) %>%
  sum() / 10000 # Total removed in hectares

pa <- dplyr::filter(pa, !(pa_oecm_df == 5))

# Fix problems
pa <- st_make_valid(pa)        # Fix Ring Self-intersections

# Save file for comparisons
write_rds(pa, "data/CPCAD_Dec2024_BC_fixed.rds")

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


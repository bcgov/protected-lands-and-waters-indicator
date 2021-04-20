# Copyright 2021 Province of British Columbia
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


get_cpcad_bc_data <- function() {
  f <- "CPCAD-BDCAPC_Dec2020.gdb.zip"
  ff <- file.path("data", str_remove(f, ".zip"))
  if(!dir.exists(ff)){
    download.file(file.path("https://cws-scf.ca", f), destfile = f)
    unzip(f, exdir = "data")
    unlink(f)
  }
  st_layers(ff) #list available layers

  pa <- st_read(ff, layer = "CPCAD_Dec2020") %>%
    rename_all(tolower)

  pa <- filter(pa, str_detect(loc_e, "Pacific|British Columbia"))

  pa <- filter(pa, !(aichi_t11 == "No" & oecm == "No"))

  # Fix problems
  pa <- st_make_valid(pa)        # Fix Ring Self-intersections

  # Save file for comparisons
  write_rds(pa, "data/CPCAD_Dec2020_BC_fixed.rds")

  return(pa)
}

get_wha_data <- function(){
  if(!file.exists("data/wha.rds")) {
   bcdc_get_data("WHSE_WILDLIFE_MANAGEMENT.WCP_WILDLIFE_HABITAT_AREA_POLY") %>%
      write_rds("data/wha.rds")
}
}

get_ogma_data <- function (){
  if(!file.exists("data/ogma.rds")) {
  bcdc_query_geodata("WHSE_LAND_USE_PLANNING.RMP_OGMA_LEGAL_CURRENT_SVW") %>%
    collect() %>%
    write_rds("data/ogma.rds")
  }
}

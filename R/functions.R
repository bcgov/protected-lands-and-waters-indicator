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


# Loading data functions --------------------------------------------------

get_wha_data <- function(){
  wha_data <- bcdc_get_data("WHSE_WILDLIFE_MANAGEMENT.WCP_WILDLIFE_HABITAT_AREA_POLY") %>%
      rename_with(tolower) %>%
      write_rds("data/wha.rds")
  return(wha_data)
}

get_ogma_data <- function(){
  ogma_data <- bcdc_query_geodata("WHSE_LAND_USE_PLANNING.RMP_OGMA_LEGAL_CURRENT_SVW") %>%
    collect() %>%
    rename_with(tolower) %>%
    write_rds("data/ogma.rds")
  return(ogma_data)
}

get_cpcad_bc_data <- function(crs) {

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
  pa <- st_make_valid(pa)

  # Apply crs from wildlife habitat area for direct comparison
  pa <- pa %>%
    st_transform(st_crs(read_rds(crs))) %>%
    mutate(area_all = as.numeric(st_area(.)))

  pa_data <- st_cast(pa, to = "POLYGON", warn = FALSE)

  # Save file for comparisons
  write_rds(pa_data, "data/CPCAD_Dec2020_BC_fixed.rds")
}
# Intersections with wha and ogma data to add dates -----------------------------------------

fill_in_dates <- function(data, column, join, landtype, output){

  output <- data %>%
  select(all_of(column)) %>%
  filter(!is.na(column)) %>%
  st_cast(to = "POLYGON", warn = FALSE) %>%
  st_point_on_surface() %>%
    st_join(
      filter(join, name_e == landtype) %>%
        tibble::rownames_to_column(), .
    ) %>%
    group_by(rowname) %>%
    slice_max(column, with_ties = FALSE)
}

# Clean up protected areas db ------------------------------------

clean_up_dates <- function(data, input1, input2, output){
  output <- data %>%
    filter(!name_e %in% c("Wildlife Habitat Areas",
                          "Old Growth Management Areas (Mapped Legal)")) %>%
    bind_rows(input1, input2)

  output <- output %>%
    mutate(
      date = case_when(!is.na(protdate) ~ protdate,
                       !is.na(approval_date) ~ as.integer(year(approval_date)),
                       !is.na(legalization_frpa_date) ~ as.integer(year(legalization_frpa_date)),
                       name_e == "Lazo Marsh-North East Comox Wildlife Management Area" ~ 2001L,
                       name_e == "S'Amunu Wildlife Management Area" ~ 2018L,
                       name_e == "Swan Lake Wildlife Management Area" ~ 2018L,
                       name_e == "Mctaggart-Cowan/Nsek'Iniw'T Wildlife Management Area" ~ 2013L,
                       name_e == "Sea To Sky Wildland Zones" ~ 2011L),
      iucn_cat = factor(iucn_cat, levels = c("Ia", "Ib", "II", "III", "IV",
                                             "V", "VI", "Yes", "N/A")),
      name_e = str_replace(name_e, "Widllife", "Wildlife"),
      park_type = if_else(oecm == "Yes", "OECM", "PPA")) %>%
    arrange(desc(oecm), iucn_cat, date, area_all)

  # Save file for comparisons
  write_rds(output, "data/CPCAD_Dec2020_BC_clean.rds")
}

remove_overlaps <- function(data, output){
  output <- data %>%
    mutate(area_single = as.numeric(st_area(.))) %>% # Calculate indiv area
    st_make_valid() %>%
    st_difference() %>%                             # Remove overlaps (~45min)
    st_make_valid()        # Fix Self-intersections (again!)
  write_rds(output, "data/CPCAD_Dec2020_BC_clean_no_ovlps.rds")
}

# Calculate ecoregion and bec zone protected areas ------------------------

load_ecoregions <- function(){
  marine_eco <- c("HCS", "IPS", "OPS", "SBC", "TPC", "GPB") #separate land & water ecoregions
  ecoregions <- ecoregions(ask = FALSE) %>%
    rename_all(tolower) %>%
    select(ecoregion_code, ecoregion_name) %>%
    mutate(ecoregion_name = tools::toTitleCase(tolower(ecoregion_name)),
           type = if_else(ecoregion_code %in% marine_eco, "water", "land"))
}

load_bec <- function(){
  bec <- bec(ask = FALSE) %>%
    rename_all(tolower) %>%
    select(zone, subzone, zone_name, subzone_name, natural_disturbance_name)
}

clip_bec_to_bc_boundary<- function(data){
  # Clip BEC to BC outline ----------------------------------------------------
  # We'll need simplified becs for plotting later
  # NOTE: geojson doesn't have CRS so have to remind R that CRS is BC Albers
  #       (It will warn that it's not transforming)
  bc <- bc_bound_hres(ask = FALSE)

  message("Clip BEC to BC outline")

  geojson_write(data, file = "data/bec.geojson")
  geojson_write(bc, file = "data/bc.geojson")

  system(glue("mapshaper-xl data/bec.geojson ",
              "-clip data/bc.geojson remove-slivers ",
              "-o data/bec_clipped.geojson"))

  system(glue("mapshaper-xl data/bec_clipped.geojson ",
              "-simplify 50% ",
              "-o data/bec_clipped_simp.geojson"))
}

intersect_eco_pa <- function(input1, input2){
  pa_eco <- st_intersection(input1, input2) %>%
    st_collection_extract(type = "POLYGON")
  write_rds(pa_eco, "data/CPCAD_Dec2020_BC_clean_no_ovlps_ecoregions.rds")
}

intersect_eco_bec <- function(input1, input2){
  # Add bec zones to PA -------------------------------------------------------
  message("Add bec zones")
  bec <- st_read(input1, crs = 3005) %>%
    st_make_valid()
  pa_bec <- st_intersection(input1, input2)%>%
    st_collection_extract(type = "POLYGON")
  write_rds(pa_bec, "data/CPCAD_Dec2020_BC_clean_no_ovlps_beczones.rds")
}


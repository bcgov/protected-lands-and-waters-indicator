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
      rename_with(tolower)
  wha_data
}

get_ogma_data <- function(){
  ogma_data <- bcdc_query_geodata("WHSE_LAND_USE_PLANNING.RMP_OGMA_LEGAL_CURRENT_SVW") %>%
    collect() %>%
    rename_with(tolower)
  ogma_data
}

get_cpcad_bc_data <- function(crs) {
  f <- "CPCAD-BDCAPC_Dec2020.gdb.zip"
  ff <- file.path("data", str_remove(f, ".zip"))
  if(!dir.exists(ff)){
    download.file(file.path("https://cws-scf.ca", f), destfile = f)
    unzip(f, exdir = "data")
    unlink(f)
  }

  pa <- st_read(ff, layer = "CPCAD_Dec2020") %>%
    rename_all(tolower) %>%
    filter(str_detect(loc_e, "Pacific|British Columbia")) %>%
    filter(!(aichi_t11 == "No" & oecm == "No")) %>%
    st_make_valid() %>%
    st_transform(st_crs(read_rds(crs))) %>% # Apply crs from wildlife habitat area for direct comparison
    mutate(area_all = as.numeric(st_area(.))) %>%
    st_cast(to = "POLYGON", warn = FALSE)
  pa
}

load_ecoregions <- function(){
  marine_eco <- c("HCS", "IPS", "OPS", "SBC", "TPC", "GPB") #separate land & water ecoregions
  eco <- ecoregions(ask = FALSE) %>%
    rename_all(tolower) %>%
    select(ecoregion_code, ecoregion_name) %>%
    mutate(ecoregion_name = tools::toTitleCase(tolower(ecoregion_name)),
           type = if_else(ecoregion_code %in% marine_eco, "water", "land"))
  eco
}

load_bec <- function(){
  bec <- bec(ask = FALSE) %>%
    rename_all(tolower) %>%
    select(zone, subzone, zone_name, subzone_name, natural_disturbance_name)
  bec
}

# Intersections with wha and ogma data to add dates -----------------------------------------

fill_in_dates <- function(data, column, join, landtype, output){
  output <- data %>%
    select(all_of(column)) %>%
    filter(!is.na(column)) %>%
    st_cast(to = "POLYGON", warn = FALSE) %>%
    st_join(
      filter(join, name_e == landtype) %>%
        tibble::rownames_to_column(), .
    ) %>%
    group_by(rowname) %>%
    slice_max(column, with_ties = FALSE)
  output
}

# Clean up data ------------------------------------

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
  output
}

remove_overlaps <- function(data, output){
  output <- data %>%
    mutate(area_single = as.numeric(st_area(.))) %>% # Calculate indiv area
    st_make_valid() %>%
    st_difference() %>%                             # Remove overlaps (~45min)
    st_make_valid()        # Fix Self-intersections (again!)
  output
  write_rds(output, "data/CPCAD_Dec2020_BC_clean_no_ovlps.rds") #save to disk for date checks
}

intersect_pa <- function(input1, input2, output){
  output <- st_intersection(input1, input2) %>%
    st_collection_extract(type = "POLYGON")
  write_rds(output, paste0("data/CPCAD_Dec2020_BC_clean_no_ovlps_", output, ".rds"))
  output
}

# Simplify spatial data for visualization---------------------------------------------------

clip_bec_to_bc_boundary<- function(data){# Clip BEC to BC outline ---
  bc <- bc_bound_hres(ask = FALSE)
  geojson_write(data, file = "data/bec.geojson")
  geojson_write(bc, file = "data/bc.geojson")

  system(glue("mapshaper-xl data/bec.geojson ",
              "-clip data/bc.geojson remove-slivers ",
              "-o data/bec_clipped.geojson"))

  system(glue("mapshaper-xl data/bec_clipped.geojson ",
              "-simplify 50% ",
              "-o data/bec_clipped_simp.geojson"))
  output <- st_read("data/bec_clipped_simp.geojson", crs=3005)%>% # geojson doesn't have CRS so have to remind R that CRS is BC Albers
    st_make_valid()
  output
}
# Run by region/zone
#  - Much faster and no crashing (on my computer at least)
#  - Allows simplifying to different degrees for different regions

simplify_ecoregions<- function(data){# Simplify ecoregions for plotting  ---
  eco_simp <- slice(data, 0)
  for(e in unique(data$ecoregion_code)) {
    message(e)
    temp <- filter(data, ecoregion_code == e)
    keep_shapes <- if_else(nrow(temp) <= 1000, TRUE, FALSE)
    keep <- case_when(nrow(temp) < 50 ~ 1,
                      nrow(temp) < 1000 ~ 0.1,
                      TRUE ~ 0.05)
    if(keep == 1) region <- temp else region <- ms_simplify(temp, keep = keep,
                                                            keep_shapes = keep_shapes)
    eco_simp <- rbind(eco_simp, region)
  }
  output <- filter(eco_simp, !st_is_empty(eco_simp))
  write_rds(eco_simp, "out/CPCAD_Dec2020_eco_simp.rds")
  output
}

simplify_beczones<-function(data){# Simplify bec zones for plotting  ---
  geojson_write(data, file = "data/pa_bec.geojson")

  system(glue("mapshaper-xl data/pa_bec.geojson ",
              "-simplify 5% keep-shapes ",
              "-o out/CPCAD_Dec2020_bec_simp.geojson"))
  output<-st_read("out/CPCAD_Dec2020_bec_simp.geojson", crs=3005)%>% # geojson doesn't have CRS so have to remind R that CRS is BC Albers
    st_make_valid()
  output
}

simplify_eco_background<- function(data){# Simplify ecoregions background map ---
  output<- ms_simplify(data, keep = 0.01)
  write_rds(eco, "out/eco_simp.rds")
  output
}

simplify_bec_background<-function(){# Simplify bec zones background map ---
  system(glue("mapshaper-xl data/bec_clipped.geojson ",
              "-o out/bec_simp.geojson"))
  output<-st_read("out/bec_simp.geojson", crs=3005)%>% # geojson doesn't have CRS so have to remind R that CRS is BC Albers
    st_make_valid()
  output
}

# Calculate ecoregion and bec zone protected areas ------------------------

find_ecoregion_size <- function(data) {
# Summarize by eco region
  output <- data %>%
    mutate(area = as.numeric(st_area(geometry))) %>%
    st_set_geometry(NULL) %>%
    group_by(ecoregion_code) %>%
    summarize(total = sum(area) / 10000, .groups = "drop")
  output
}

protected_area_by_eco <- function(data, eco_input){
  output <- data %>%
    mutate(total_area = as.numeric(st_area(geometry))) %>%
    st_set_geometry(NULL) %>%
    group_by(ecoregion_code, ecoregion_name, type, date) %>%
    complete(park_type = c("OECM", "PPA"),
             fill = list(total_area = 0)) %>%
    ungroup() %>%
    # Add placeholder for missing dates for plots (max year plus 1)
    mutate(d_max = max(date, na.rm = TRUE),
           missing = is.na(date),
           date = if_else(is.na(date), d_max + 1L, date)) %>%
    group_by(ecoregion_code) %>%
    mutate(d_max = max(c(date, d_max))) %>%
    group_by(ecoregion_code, ecoregion_name, park_type, type) %>%
    # Fill in missing dates all the way to max
    complete(date = seq(min(date, na.rm = TRUE), d_max[1]),
             fill = list(total_area = 0, missing = FALSE)) %>%
    group_by(ecoregion_code, ecoregion_name, park_type, type, missing, date) %>%
    summarize(total_area = as.numeric(sum(total_area)) / 10000, .groups = "drop") %>%
    group_by(ecoregion_code, ecoregion_name, park_type, type) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(cum_type = cumsum(total_area),
           total_type = sum(total_area)) %>%
    group_by(ecoregion_code) %>%
    mutate(total_region = sum(total_area)) %>%
    left_join(eco_input, by = "ecoregion_code") %>%
    # Get regional values
    group_by(ecoregion_code) %>%
    mutate(p_type = total_type / total * 100,
           p_region = total_region / total * 100,
           cum_p_type = cum_type / total * 100) %>%
    ungroup() %>%
    arrange(desc(type), p_region) %>%
    mutate(ecoregion_name = factor(ecoregion_name, levels = unique(ecoregion_name)))
  write_rds(output, "out/eco_area.rds")
  output
}

protected_area_totals<- function(data, eco_area_data){
  pa_eco_all_df <- data %>%
    mutate(total_area = st_area(geometry),
           d_max = max(date, na.rm = TRUE)) %>%
    st_set_geometry(NULL) %>%
    # Add placeholder for missing dates for plots (max year plus 1)
    mutate(missing = is.na(date),
           date = if_else(is.na(date), d_max + 1L, date),
           d_max = max(c(date, d_max))) %>%
    group_by(park_type, type) %>%
    # Fill in missing dates all the way to present plus 1 year (ensures plots go to present smoothly)
    complete(date = seq(min(date, na.rm = TRUE), d_max[1]),
             fill = list(total_area = 0, missing = FALSE)) %>%
    group_by(park_type, type, missing, date) %>%
    summarize(total_area = as.numeric(sum(total_area)) / 10000, .groups = "drop") %>%
    group_by(park_type, type) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(cum_type = cumsum(total_area),
           total_type = sum(total_area)) %>%
    ungroup() %>%
    mutate(total = sum(eco_totals$total),
           p_type = total_type / total * 100,
           cum_p_type = cum_type / total * 100) %>%
    group_by(date) %>%
    complete(type = c("land", "water"), park_type = c("OECM", "PPA"),
             fill = list(cum_p_type = 0, missing = FALSE)) %>%
    ungroup()
  #write_rds(pa_eco_all_df, "out/eco_area_all.rds")

  #find total area of land/water by adding ecoregion area
  bc_totals<- eco_area_data %>%
    group_by(ecoregion_name) %>%
    slice_head(n=1) %>%
    ungroup() %>%
    group_by(type) %>%
    summarize(bc_total_by_type = sum(total)) %>%
    ungroup()

  #find sum of park type by year for area plot
  output <- pa_eco_all_df %>%
    replace_na(list(total_area=0)) %>%
    group_by(date, park_type, type) %>%
    summarize(total_annual_by_park_type = sum(total_area), .groups = "drop") %>%
    left_join(bc_totals)%>%
    group_by(type, park_type) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(cumulative_by_type=cumsum(total_annual_by_park_type),
           cum_p_type = cumulative_by_type/bc_total_by_type*100) %>%
    ungroup()
  write_rds(output, "out/pa_eco_sum.rds")
  output
}

protected_area_by_bec<-function(bec_data, data){# Summarize by bec zone region
  bec_totals <- bec_data %>%
    mutate(area = as.numeric(st_area(geometry))) %>%
    st_set_geometry(NULL) %>%
    group_by(zone) %>%
    summarize(total = sum(area) / 10000, .groups = "drop")

  output <- data %>%
    mutate(total_area = st_area(geometry)) %>%
    st_set_geometry(NULL) %>%
    group_by(zone, zone_name, park_type) %>%
    summarize(total_area = as.numeric(sum(total_area) / 10000), .groups = "drop") %>%
    group_by(zone_name) %>%
    mutate(total_zone = sum(total_area)) %>%
    ungroup() %>%
    left_join(bec_totals, by = "zone") %>%
    mutate(p_area = total_area / total * 100,
           p_zone = total_zone / total * 100) %>%
    arrange(p_zone) %>%
    mutate(zone_name = str_replace_all(zone_name, "--", " — "),
           zone_name = factor(zone_name, levels = unique(zone_name)))

  write_rds(pa_bec_df, "out/bec_area.rds")
  output
}


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

get_cpcad_bc_data <- function() {
  f <- "CPCAD-BDCAPC_Dec2020.gdb.zip"
  ff <- file.path("data", str_remove(f, ".zip"))
  if(!dir.exists(ff)){
    download.file(file.path("https://cws-scf.ca", f), destfile = f)
    unzip(f, exdir = "data")
    unlink(f)
  }

  pa <- st_read(ff, layer = "CPCAD_Dec2020") %>%
    rename_all(tolower) %>%
    dplyr::filter(str_detect(loc_e, "Pacific|British Columbia")) %>%
    dplyr::filter(!(aichi_t11 == "No" & oecm == "No")) %>%
    st_make_valid() %>%
    st_transform(st_crs(3005)) %>% # Apply crs from wildlife habitat area for direct comparison
    mutate(area_all = as.numeric(st_area(.))) %>%
    st_cast(to = "POLYGON", warn = FALSE)
  pa
}

load_ecoregions <- function(){
  #marine_eco <- c("HCS", "IPS", "OPS", "SBC", "TPC", "GPB") #separate land & water ecoregions
  eco <- ecoregions(ask = FALSE) %>%
    rename_all(tolower) %>%
    select(ecoregion_code, ecoregion_name) %>%
    mutate(ecoregion_name = tools::toTitleCase(tolower(ecoregion_name))) %>%
    st_cast(to="POLYGON", warn = FALSE)
  eco
}

load_bec <- function(){
  bec <- bec(ask = FALSE) %>%
    rename_all(tolower) %>%
    select(zone, subzone, zone_name, subzone_name, natural_disturbance_name) %>%
    st_cast(to="POLYGON", warn = FALSE)
  bec$zone_name <- str_replace_all(bec$zone_name, "-- ", "")
  bec
}


# Intersections with wha and ogma data to add dates -----------------------------------------

fill_in_dates <- function(data, column, join, landtype, output){
  output <- data %>%
    select(all_of(column)) %>%
    dplyr::filter(!is.na(column)) %>%
    st_cast(to = "POLYGON", warn = FALSE) %>%
    st_join(
      dplyr::filter(join, name_e == landtype) %>%
        tibble::rownames_to_column(), .
    ) %>%
    group_by(rowname) %>%
    slice_max(column, with_ties = FALSE)
  output
}

# Clean up data ------------------------------------

clean_up_dates <- function(data, input1, input2, output){
  output <- data %>%
    dplyr::filter(!name_e %in% c("Wildlife Habitat Areas",
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
    arrange(desc(oecm), iucn_cat, date, area_all) %>%
    st_cast() %>%
    st_cast(to="POLYGON", warn = FALSE)
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


# intersect data ----------------------------------------------------------

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
    st_make_valid() %>%
    st_cast() %>%
    st_cast(to="POLYGON", warn = FALSE)
  output
}

fix_gpd_ecoregions <- function(data){
  m_ecoregions <- c("HCS", "IPS", "OPS", "SBC", "TPC")

  ecoregions <- data
  bc_bound_hres <- bcmaps::bc_bound_hres()

  ## Extract the terrestrial and marine portions of GPB into separate objects
  gpb_terrestrial <- ms_clip(ecoregions[ecoregions$ecoregion_code == "GPB",],
                             bc_bound_hres)
  gpb_marine <- ms_erase(ecoregions[ecoregions$ecoregion_code == "GPB",],
                         bc_bound_hres)
  ## Fix it up:
  gpb_terrestrial <- fix_geo_problems(gpb_terrestrial)
  gpb_marine <- fix_geo_problems(gpb_marine)

  ## Add terrestrial portion of GPB back to terrestrial ecoregions
  ecoregions_t <- rbind(ecoregions[!ecoregions$ecoregion_code %in% c("GPB", m_ecoregions), ],
                        gpb_terrestrial[, setdiff(names(gpb_terrestrial), "rmapshaperid")])

  ## Add marine portion of GPB back to marine ecoregions
  ecoregions_m <- rbind(ecoregions[ecoregions$ecoregion_code %in% m_ecoregions, ],
                        gpb_marine[, setdiff(names(gpb_terrestrial), "rmapshaperid")])

  ## Calcualte the area of the polygons
  ecoregions_t <- ecoregions_t %>%
    mutate(area = as.numeric(st_area(geometry))) %>%
    mutate(type = "land")


  ecoregions_m <- ecoregions_m %>%
    mutate(area = as.numeric(st_area(geometry))) %>%
    mutate(type = "water")

  ## Create simplified versions for visualization
  ecoregions_comb <- rbind(ecoregions_m, ecoregions_t)
  ecoregions_comb
}

intersect_pa <- function(input1, input2, output){
  output <- st_intersection(input1, input2) %>%
    st_collection_extract(type = "POLYGON")
  output
}

# Simplify spatial data for visualization---------------------------------------------------

# Run by region/zone
#  - Much faster and no crashing (on my computer at least)
#  - Allows simplifying to different degrees for different regions

simplify_ecoregions<- function(data){# Simplify ecoregions for plotting  ---
  eco_simp <- slice(data, 0)
  for(e in unique(data$ecoregion_code)) {
    message(e)
    temp <- dplyr::filter(data, ecoregion_code == e)
    keep_shapes <- if_else(nrow(temp) <= 1000, TRUE, FALSE)
    keep <- case_when(nrow(temp) < 50 ~ 1,
                      nrow(temp) < 1000 ~ 0.1,
                      TRUE ~ 0.05)
    if(keep == 1) region <- temp else region <- ms_simplify(temp, keep = keep,
                                                            keep_shapes = keep_shapes)
    eco_simp <- rbind(eco_simp, region)
  }
  output <- dplyr::filter(eco_simp, !st_is_empty(eco_simp))
  write_rds(eco_simp, "out/CPCAD_Dec2020_eco_simp.rds")
  output
}

simplify_beczones<-function(data){# Simplify bec zones for plotting  ---
  geojson_write(data, file = "data/pa_bec.geojson")

  system(glue("mapshaper-xl data/pa_bec.geojson ",
              "-simplify 5% keep-shapes ",
              "-o out/CPCAD_Dec2020_bec_simp.geojson"))
  output<-st_read("out/CPCAD_Dec2020_bec_simp.geojson", crs=3005) # geojson doesn't have CRS so have to remind R that CRS is BC Albers
  output
}

simplify_eco_background<- function(data){# Simplify ecoregions background map ---
  output<- ms_simplify(data, keep = 0.01)
  write_rds(output, "out/eco_simp.rds")
  output
}

simplify_bec_background<-function(){# Simplify bec zones background map ---
  system(glue("mapshaper-xl data/bec_clipped.geojson ",
              "-simplify 1% keep-shapes ",
              "-o out/bec_simp.geojson"))
  output<-st_read("out/bec_simp.geojson", crs=3005) # geojson doesn't have CRS so have to remind R that CRS is BC Albers
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
    mutate(total = sum(eco_area_data$total),
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
    summarize(sum_type_by_zone = as.numeric(sum(total_area) / 10000), .groups = "drop") %>%
    group_by(zone_name) %>%
    mutate(sum_zone = sum(sum_type_by_zone)) %>%
    ungroup() %>%
    left_join(bec_totals, by = "zone") %>%
    mutate(perc_type_zone = sum_type_by_zone / total * 100,
           perc_zone = sum_zone / total * 100) %>%
    arrange(perc_zone) %>%
    mutate(zone_name = str_replace_all(zone_name, "--", " — "),
           zone_name = factor(zone_name, levels = unique(zone_name)))

  write_rds(output, "out/bec_area.rds")
  output
}

# Supplemental plots ------------------------------------------------------

plot_by_bec_zone <- function(data){
  bar1 <- ggplot(data,
                 aes(x = perc_type_zone, y = zone_name, fill = zone, alpha = park_type)) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank(),
          legend.position = c(0.7, 0.3)) +
    geom_bar(width = 0.9, stat = "identity") +
    labs(x = "Percent Area Protected", y = "Biogeoclimatic Zone") +
    scale_fill_manual(values = bec_colours(), guide = FALSE) +
    scale_alpha_manual(name = "Type", values = c("OECM" = 0.5, "PA" = 1)) +
    scale_x_continuous(expand = c(0,0)) +
    guides(alpha = guide_legend(override.aes = list(fill = "black")))
  ggsave("out/bec_bar1.png", bar1, width = 6, height = 6, dpi = 300)
  bar1
}

plot_bec_zone_totals<- function(data){
  bar2 <- ggplot(data %>%  select(zone_name, zone, perc_zone) %>% distinct(),
                 aes(x = perc_zone, y = zone_name, fill = zone)) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank()) +
    geom_bar(width = 0.9, stat = "identity") +
    labs(x = "Percent Area Protected (%)", y = "Biogeoclimatic Zone") +
    scale_fill_manual(values = bec_colours(), guide = FALSE) +
    scale_x_continuous(expand = c(0,0))
  ggsave("out/bec_bar2.png", bar2, width = 6, height = 6, dpi = 300)
  bar2

  bec_totals <- data %>%
    dplyr::filter(park_type == "PPA") %>%
    mutate(total_bc = sum(total)) %>%
    mutate(bec_rep = total/total_bc *100) %>%
    select(zone, zone_name, perc_zone, total, total_bc, bec_rep) %>%
    arrange(desc(perc_zone))

  bar3 <- ggplot(bec_totals, aes(x=bec_rep, y= zone_name, fill=zone))+
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank()) +
    geom_bar(width = 0.9, stat = "identity") +
    labs(x = "BEC Zone Distribution Across B.C. (%)", y = "") +
    scale_fill_manual(values = bec_colours(), guide = FALSE) +
    scale_x_continuous(expand = c(0,0))
  ggsave("out/bec_bar3.png", bar3, width = 6, height = 6, dpi = 300)

  joined_bar<-plot_grid(bar2, bar3, align="h")
  ggsave("out/bec_join.png", joined_bar, width = 12, height = 6, dpi = 300)
}

bec_zone_map <- function(data){

  map<-ggplot() +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 25)) +
    geom_sf(data = data, aes(fill = zone), colour = NA)+
    geom_sf(data = bc_bound_hres(), aes(fill=NA))+
    scale_fill_manual(values = bec_colours()) +
    theme(legend.title=element_blank()) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = "Distribution of BEC Zones Across B.C.")
  ggsave("out/bec_map.png", map, width = 11, height = 10, dpi = 300)
  map
}

create_bc_button <- function(){
  output <- bc_bound() %>%
    st_geometry() %>%
    ms_simplify(0.02, explode = TRUE, keep_shapes = FALSE) %>%
    ggplot() +
    theme_void() +
    ggiraph::geom_sf_interactive(fill = "black", data_id = "reset")
  write_rds(output, "out/bc_button.rds")
  output
}

bc_map <- function(data){

  ld_cities <- bcmaps::bc_cities() %>%
    dplyr::filter(NAME == "Victoria" |
                    NAME == "Prince Rupert"|
                    NAME == "Smithers"|
                    NAME == "Fort St. John"|
                    NAME == "Kamloops"|
                    NAME == "Prince George"|
                    NAME == "Vancouver"|
                    NAME == "Cranbrook")%>%
    dplyr::select(NAME, geometry)

  scale_land <- c("OECM" = "#93c288", "PPA" = "#004529")
  scale_water <- c("OECM" = "#8bc3d5", "PPA" = "#063c4e")
  scale_combo <- setNames(c(scale_land, scale_water),
                          c("Land - OECM", "Land - PPA",
                            "Water - OECM", "Water - PPA"))
  output <- data %>%
    mutate(type_combo = glue("{tools::toTitleCase(type)} - {park_type}"),
           type_combo = factor(type_combo,
                               levels = c("Land - OECM", "Land - PPA",
                                          "Water - OECM", "Water - PPA"))) %>%
    group_by(date, type) %>%
    ungroup()


  map<-ggplot() +
    theme_void() +
    theme(plot.title = element_text(hjust =0.5, size = 25)) +
    geom_sf(data = output, aes(fill = type_combo), colour = NA)+
    geom_sf(data = bc_bound(), aes(fill=NA))+
    geom_sf(data=ld_cities)+
    ggrepel::geom_text_repel(data=ld_cities, aes(label=NAME, geometry=geometry),
                              stat="sf_coordinates")+
    #geom_sf_label(data=ld_cities, aes(label=NAME), nudge_y=2)+
    scale_fill_manual(values = scale_combo) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = "Distribution of Protected Areas in B.C.") +
    theme(legend.title=element_blank())
  ggsave("out/prov_map.png", map, width = 11, height = 10, dpi = 300)
  map
}

eco_static <- function(data, input){

  input <- input %>%
    group_by(ecoregion_name, type) %>%
    dplyr::filter(date == 2020) %>%
    select(ecoregion_name, type, p_region)

  data <- data %>%
    mutate(ecoregion_name = as.factor(ecoregion_name)) %>%
    mutate(type=as.factor(type)) %>%
    left_join(input, by = c("ecoregion_name", "type"))
  scale_map <- c("land" = "#056100", "water" = "#0a7bd1")

  g <- ggplot() +
    theme_void() +
    geom_sf(data=data, mapping=aes(fill = type, alpha = p_region), size = 0.1, colour = "black")+
    geom_sf(data=bc_bound_hres(), mapping=aes(fill=NA))+
    theme(plot.margin = unit(c(0,0,0,0), "pt")) +
    scale_fill_manual(values = scale_map, name = "") +
    scale_alpha_continuous(range = c(0.25, 1), n.breaks = 5, limits = c(0, 100), name="% Protected") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = "Area Protected by Ecoregion") +
    theme(plot.title = element_text(hjust=0.5, size = 25)) +
    guides(alpha = guide_legend(override.aes = list(fill = "black")))#+
    #guides(alpha = guide_legend(override.aes = list(fill = scale_map["water"])))
  ggsave("out/ecoregion_map.png", g, width = 11, height = 10, dpi = 300)
  g
}

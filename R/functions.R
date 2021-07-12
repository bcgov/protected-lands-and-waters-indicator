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

# classify_land_type <- function(data){
#
#     bc_bound_hres <- bcmaps::bc_bound_hres()
#
#     ## Extract the terrestrial and marine portions of GPB into separate objects
#     pa_terrestrial <- ms_clip(data, bc_bound_hres)
#     pa_marine <- ms_erase(data, bc_bound_hres)
#
#     ## Fix it up:
#     pa_terrestrial <- fix_geo_problems(pa_terrestrial)
#     pa_marine <- fix_geo_problems(pa_marine)
#
#
#     pa_terrestrial <- pa_terrestrial %>%
#       mutate(type = "land")
#
#
#     pa_marine <- pa_marine %>%
#       mutate(type = "water")
#
#     ## Create simplified versions for visualization
#     pa_comb <- rbind(pa_terrestrial, pa_marine)
#     pa_comb
# }


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

fix_ecoregions <- function(data){
  #m_ecoregions <- c("HCS", "IPS", "OPS", "SBC", "TPC", "GPB")

  m_ecoregions <- c("SBC", "TPC", "OPS", "IPS")
  m_t_ecoregions <- c("GPB", "COG", "NRA", "HCS")

  eco_m_t_sites <- data %>%
    dplyr::filter(ecoregion_code %in% m_t_ecoregions)

  eco_other <- data %>%
    dplyr::filter(!ecoregion_code %in% m_t_ecoregions)

  bc_bound_hres <- bcmaps::bc_bound_hres()

  ## Extract the terrestrial and marine portions of GPB into separate objects
  eco_mixed_terrestrial <- ms_clip(eco_m_t_sites, bc_bound_hres)
  eco_mixed_marine <- ms_erase(eco_m_t_sites, bc_bound_hres)

  ## Fix it up:
  eco_mixed_terrestrial <- fix_geo_problems(eco_mixed_terrestrial)
  eco_mixed_marine <- fix_geo_problems(eco_mixed_marine)
  eco_other <- fix_geo_problems(eco_other)

  #casewhen block here to determine type, if in m__ecor

  eco_mixed_terrestrial <- eco_mixed_terrestrial %>%
    mutate(ecoregion_area = as.numeric(st_area(geometry)),
           total_ecoregion_by_type = as.numeric(units::set_units(ecoregion_area, km^2)),
           type = "land") %>%
    group_by(ecoregion_code, ecoregion_name, type) %>%
    summarise(total_ecoregion_by_type = sum(total_ecoregion_by_type))%>%
    ungroup()

  eco_mixed_marine <- eco_mixed_marine %>%
    mutate(ecoregion_area = as.numeric(st_area(geometry)),
           total_ecoregion_by_type = as.numeric(units::set_units(ecoregion_area, km^2)),
           type = "water") %>%
    group_by(ecoregion_code, ecoregion_name, type) %>%
    summarise(total_ecoregion_by_type = sum(total_ecoregion_by_type))%>%
    ungroup()

  eco_other <- eco_other %>%
    mutate(ecoregion_area = as.numeric(st_area(geometry)),
           total_ecoregion_by_type = as.numeric(units::set_units(ecoregion_area, km^2)),
           type = case_when(ecoregion_code %in% m_ecoregions ~ "water",
                            !ecoregion_code %in% c(m_ecoregions, m_t_ecoregions) ~ "land")) %>%
    group_by(ecoregion_code, ecoregion_name, type) %>%
    summarise(total_ecoregion_by_type = sum(total_ecoregion_by_type))%>%
    ungroup()

  ## Create simplified versions for visualization
  ecoregions_comb <- rbind(eco_other, eco_mixed_marine, eco_mixed_terrestrial)
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

# find_ecoregion_size <- function(data) {
# # Summarize by eco region
#   output <- data %>%
#     mutate(area = as.numeric(st_area(geometry))) %>%
#     st_set_geometry(NULL) %>%
#     group_by(ecoregion_code) %>%
#     summarize(total = sum(area) / 10000, .groups = "drop")
#   output
# }

protected_area_by_eco <- function(data, eco_totals){
  eco_totals<- eco_totals %>%
    st_set_geometry(NULL)
  output <- data %>%
    mutate(total_area = as.numeric(st_area(geometry)),
           total_area = set_units(total_area, km^2)) %>%
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
    summarize(total_area = as.numeric(sum(total_area)), .groups = "drop") %>%
    group_by(ecoregion_code, ecoregion_name, park_type, type) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(cum_type = cumsum(total_area),
           total_type = sum(total_area)) %>%
    ungroup() %>%
    left_join(eco_totals, by = c("ecoregion_code", "ecoregion_name" ,"type")) %>%
    # Get regional values
    group_by(ecoregion_code, type) %>%
    mutate(both_park_type_sum = sum(total_area),
           p_type = total_type / total_ecoregion_by_type * 100,
           cum_p_type = cum_type / total_ecoregion_by_type * 100,
           p_region = both_park_type_sum/total_ecoregion_by_type * 100) %>%
    ungroup() %>%
    arrange(desc(type), p_type) %>%
    mutate(ecoregion_name = factor(ecoregion_name, levels = unique(ecoregion_name)))
  write_rds(output, "out/pa_eco_sum.rds")
  output
}

protected_area_totals<- function(data, eco_area_data){
  pa_eco_all_df <- data %>%
    mutate(total_area = st_area(geometry),
           total_area = set_units(total_area, km^2),
           d_max = max(date, na.rm = TRUE)) %>%
    st_set_geometry(NULL) %>%
    # Add placeholder for missing dates for plots (max year plus 1)
    mutate(d_max = max(date, na.rm = TRUE),
           missing = is.na(date),
           date = if_else(is.na(date), d_max + 1L, date)) %>%
    group_by(park_type, type) %>%
    # Fill in missing dates all the way to present plus 1 year (ensures plots go to present smoothly)
    complete(date = seq(min(date, na.rm = TRUE), d_max[1]),
             fill = list(total_area = 0, missing = FALSE)) %>%
    ungroup() %>%
    group_by(date) %>%
    complete(type = c("land", "water"), park_type = c("OECM", "PPA"),
             fill = list(total_area = 0, missing = FALSE)) %>%
    group_by(park_type, type, missing, date) %>%
    summarize(total_area = as.numeric(sum(total_area)), .groups = "drop") %>%
    group_by(park_type, type) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(cum_type = cumsum(total_area),
           total_type = sum(total_area))

  bc_water_total<- eco_area_data %>%
    group_by(ecoregion_name) %>%
    slice_head(n=1) %>%
    ungroup() %>%
    dplyr::filter(type=="water") %>%
    summarize(bc_water_total = sum(total_ecoregion_by_type))

  output <- pa_eco_all_df %>%
    mutate(bc_total_area = case_when(type=="water" ~ bc_water_total$bc_water_total/1000000,
                                     type=="land" ~ bcmaps::bc_area())) %>%
    group_by(date, park_type, type) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(perc_year_type = total_area/bc_total_area*100,
           cum_year_type = cum_type/bc_total_area*100,
           summary_total = total_type/bc_total_area*100)
  write_rds(output, "out/total_prot_area.rds")
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
    labs(x = "Percent Area Conserved (%)", y = "Biogeoclimatic Zone") +
    scale_fill_manual(values = bec_colours(), guide = FALSE) +
    scale_alpha_manual(name = "Type", values = c("OECM" = 0.5, "PA" = 1)) +
    scale_x_continuous(expand = c(0,0)) +
    guides(alpha = guide_legend(override.aes = list(fill = "black")))
  ggsave("out/bec_bar1.png", bar1, width = 6, height = 6, dpi = 300)
  bar1
}

plot_bec_zone_totals<- function(data, data2){

  bec_totals <- data %>%
    dplyr::filter(park_type == "PPA") %>%
    mutate(total_bc = bcmaps::bc_area()) %>%
    mutate(bec_rep = total/total_bc) %>%
    select(zone, zone_name, perc_zone, total, total_bc, bec_rep) %>%
    arrange(desc(perc_zone))

  scatterplot <- ggplot(bec_totals, aes(x=bec_rep, y= perc_zone, label= zone_name))+
    theme_minimal(base_size = 14) +
    #theme(panel.grid.major.y = element_blank()) +
    geom_point(size=2, aes(color=zone))+
    ggrepel::geom_text_repel()+
    theme(legend.position = "none") +
    scale_color_manual(values = bec_colours(), guide = FALSE) +
    labs(x = "BEC Zone Composition (%)", y = "Percentage of BEC Zone Conserved (%)")
  ggsave("out/bec_scatter.png", scatterplot, width = 6, height = 6, dpi = 300)
  write_rds(scatterplot, "out/bec_scatter.rds")

  map<-ggplot() +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 15)) +
    geom_sf(data = data2, aes(fill = zone), colour = NA)+
    geom_sf(data = bc_bound_hres(), aes(fill=NA))+
    scale_fill_manual(values = bec_colours()) +
    theme(legend.title=element_blank()) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = "BEC Zones in B.C.")
  ggsave("out/bec_map.png", map, width = 11, height = 10, dpi = 300)
  map

  combined <- plot_grid(map, scatterplot, ncol=1, align="v", rel_heights=c(1.25,1))

  ggsave("out/bec_comb.png", combined, width = 8, height = 10, dpi = 300)
  combined

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

  #manually setting label location
  ld_cities$longitude <- c(925299, 627354, 1205857, 1295775, 1399598, 1741864, 1270416, 1245673)
  ld_cities$latitude <- c(1069703, 1050342, 979165, 1241672, 626000, 570917, 435953, 380451)

  scale_land <- c("OECM" = "#74c476", "PPA" = "#006d2c")
  scale_water <- c("OECM" = "#43a2ca", "PPA" = "#0868ac")
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
    geom_sf(data = bc_bound_hres(), aes(fill=NA))+
    geom_sf(data=ld_cities)+
    geom_text(data=ld_cities, aes(x=longitude, y=latitude, label=NAME))+
    scale_fill_manual(values = scale_combo) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = "Distribution of Conserved Areas in B.C.") +
    theme(legend.title=element_blank())+
    theme(legend.justification=c("center"),
          legend.position=c(0.9, 0.6))
  ggsave("out/prov_map.png", map, width = 11, height = 10, dpi = 300)
  map
}

eco_static <- function(data, input){

  input <- input %>%
    dplyr::filter(park_type == "PPA") %>%
    group_by(ecoregion_name, ecoregion_code, type) %>%
    dplyr::filter(date == 2020) %>%
    select(ecoregion_name, ecoregion_code, type, p_region)


  #data <- cbind(data, st_coordinates(st_centroid(data)))
  label <- data %>%
    group_by(ecoregion_name, ecoregion_code) %>%
    slice_max(total_ecoregion_by_type) %>%
    ungroup()

  data <- data %>%
    mutate(ecoregion_name = as.factor(ecoregion_name),
           type=as.factor(type)) %>%
    left_join(input, by = c("ecoregion_name", "ecoregion_code", "type"))


  scale_map <- c("land" = "#056100", "water" = "#0a7bd1")

  g <- ggplot(data) +
    theme_void() +
    geom_sf(data=data, mapping=aes(fill = type, alpha = p_region), size = 0.1, colour = "black")+
    geom_sf(data=bc_bound_hres(), mapping=aes(fill=NA))+
    theme(plot.margin = unit(c(0,0,0,0), "pt")) +
    #geom_text(data=data, aes(X, Y, label=ecoregion_name))+
    #geom_sf_text_repel(aes(label=ecoregion_name))+
    ggrepel::geom_text_repel(data=label, aes(label=ecoregion_name, geometry=geometry),
                             stat="sf_coordinates",
                             min.segment.length=0)+
    scale_fill_manual(values = scale_map, guide=NULL) +
    scale_alpha_continuous(range = c(0.25, 1), n.breaks = 5, limits = c(0, 100), name="% Conserved") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = "Area Conserved by Ecoregion") +
    theme(plot.title = element_text(hjust=0.5, size = 25)) +
    theme(legend.justification=c("center"),
          legend.position=c(0.9, 0.6))+
    guides(alpha = guide_legend(override.aes = list(fill = "black")))#+
  #guides(alpha = guide_legend(override.aes = list(fill = scale_map["water"])))
  ggsave("out/ecoregion_map.png", g, width = 11, height = 10, dpi = 300)
  g
}

eco_bar <- function(data){

  data <- data %>%
    group_by(ecoregion_name, ecoregion_code, type, park_type) %>%
    dplyr::filter(date == 2020) %>%
    select(ecoregion_name, ecoregion_code, type, park_type, p_type, p_region) %>%
    arrange(desc(p_type)) %>%
    mutate(type_combo = glue("{tools::toTitleCase(type)} - {park_type}"),
         type_combo = factor(type_combo,
                             levels = c("Land - OECM", "Land - PPA",
                                        "Water - OECM", "Water - PPA")),
         ecoregion_type_combo = glue("{ecoregion_name} - {tools::toTitleCase(type)}"),
         ecoregion_name = as.factor(ecoregion_name)) %>%
    ungroup()

  scale_land <- c("OECM" = "#93c288", "PPA" = "#004529")
  scale_water <- c("OECM" = "#8bc3d5", "PPA" = "#063c4e")
  scale_map <- c("land" = "#056100", "water" = "#0a7bd1")
  scale_combo <- setNames(c(scale_land, scale_water),
                          c("Land - OECM", "Land - PPA",
                            "Water - OECM", "Water - PPA"))

    land <- ggplot(data=dplyr::filter(data, type=="land"),
                   aes(x = round(p_type,2), y = fct_reorder(ecoregion_name, p_region, .desc=FALSE),
                       fill = type, alpha = park_type)) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.y = element_blank(),
            legend.position = c(0.7, 0.3)) +
      geom_bar(width = 0.9, stat = "identity") +
      labs(y = "Ecoregion") +
      theme(axis.title.x=element_blank())+
      scale_fill_manual(values = scale_map, guide = FALSE) +
      scale_alpha_manual(name = "Type", values = c("OECM" = 0.5, "PA" = 1)) +
      scale_x_continuous(expand = c(0,0), limits=c(0,110)) +
      guides(alpha = guide_legend(override.aes = list(fill = "black"))) #+
    land

    water <-ggplot(data=dplyr::filter(data, type=="water"),
                       aes(x = round(p_type,2), y = fct_reorder(ecoregion_name, p_region, .desc=FALSE),
                           fill = type, alpha = park_type)) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.y = element_blank(),
            legend.position = c(0.7, 0.5)) +
      geom_bar(width = 0.9, stat = "identity") +
      labs(x = "Percent Conserved Within Ecoregion (%)") +
      theme(axis.title.y=element_blank())+
      scale_fill_manual(values = scale_map, guide = FALSE) +
      scale_alpha_manual(name = "Type", values = c("OECM" = 0.5, "PA" = 1)) +
      scale_x_continuous(expand = c(0,0), limits=c(0,110)) +
      theme(legend.position='none')
    water

    combined <- plot_grid(land, water, ncol=1, align="v", rel_heights=c(4,1))


    ggsave("out/eco_bar_all.png", combined, width = 9, height = 9, dpi = 300)
    combined
}

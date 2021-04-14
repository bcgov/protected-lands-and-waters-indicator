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

# Load data
pa_eco <- read_rds("data/CPCAD_Dec2020_BC_clean_no_ovlps_ecoregions.rds")
pa_bec <- read_rds("data/CPCAD_Dec2020_BC_clean_no_ovlps_beczones.rds")

eco <- ecoregions(ask = FALSE) %>%
  rename_all(tolower) %>%
  mutate(ecoregion_name = tools::toTitleCase(tolower(ecoregion_name)))

bec <- st_read("data/bec_clipped_simp.geojson", crs = 3005)

# Summarize by eco region
eco_totals <- eco %>%
  mutate(area = as.numeric(st_area(geometry))) %>%
  st_set_geometry(NULL) %>%
  group_by(ecoregion_code) %>%
  summarize(total = sum(area) / 10000, .groups = "drop")

pa_eco_df <- pa_eco %>%
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
  left_join(eco_totals, by = "ecoregion_code") %>%
  # Get regional values
  group_by(ecoregion_code) %>%
  mutate(p_type = total_type / total * 100,
         p_region = total_region / total * 100,
         cum_p_type = cum_type / total * 100) %>%
  ungroup() %>%
  arrange(desc(type), p_region) %>%
  mutate(ecoregion_name = factor(ecoregion_name, levels = unique(ecoregion_name)))
write_rds(pa_eco_df, "out/eco_area.rds")


pa_eco_all_df <- pa_eco %>%
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
write_rds(pa_eco_all_df, "out/eco_area_all.rds")

#find total area of land/water by adding ecoregion area
bc_totals<- pa_eco_df %>%
  group_by(ecoregion_name) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  group_by(type) %>%
  summarize(bc_total_by_type = sum(total)) %>%
  ungroup()

#find sum of park type by year for area plot
pa_eco_sum <- pa_eco_all_df %>%
  replace_na(list(total_area=0)) %>%
  group_by(date, park_type, type) %>%
  summarize(total_annual_by_park_type = sum(total_area), .groups = "drop") %>%
  left_join(bc_totals)%>%
  group_by(type, park_type) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(cumulative_by_type=cumsum(total_annual_by_park_type),
         cum_p_type = cumulative_by_type/bc_total_by_type*100) %>%
  ungroup()
write_rds(pa_eco_sum, "out/pa_eco_sum.rds")


# Summarize by bec zone region
bec_totals <- bec %>%
  mutate(area = as.numeric(st_area(geometry))) %>%
  st_set_geometry(NULL) %>%
  group_by(zone) %>%
  summarize(total = sum(area) / 10000, .groups = "drop")

pa_bec_df <- pa_bec %>%
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

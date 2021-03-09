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

bec <- read_rds("data/bec_clipped.rds")

# Summarize by eco region
eco_totals <- eco %>%
  mutate(area = as.numeric(st_area(geometry))) %>%
  st_set_geometry(NULL) %>%
  group_by(ecoregion_code) %>%
  summarize(total = sum(area) / 10000, .groups = "drop")

pa_eco_df <- pa_eco %>%
  mutate(total_area = st_area(geometry)) %>%
  st_set_geometry(NULL) %>%
  group_by(ecoregion_code, ecoregion_name, park_type, type, date) %>%
  arrange(date) %>%
  summarize(total_area = as.numeric(sum(total_area)) / 10000, .groups = "drop") %>%
  mutate(tooltip = glue("{format(round(total_area), big.mark = ',')} ha"))
write_rds(pa_eco_df, "out/eco_area.rds")

pa_eco_sum_df <- pa_eco_df %>%
  group_by(ecoregion_code, ecoregion_name, park_type, type) %>%
  summarize(total_area = sum(total_area), .groups = "drop") %>%
  mutate(tooltip = glue("{format(round(total_area), big.mark = ',')} ha"),
         type_combo = glue("{tools::toTitleCase(type)} - {park_type}"))
write_rds(pa_eco_sum_df, "out/eco_area_sum.rds")

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
  mutate(zone_name = str_replace(zone_name, "--", " — "),
         zone_name = factor(zone_name, levels = unique(zone_name)))

write_rds(pa_bec_df, "out/bec_area.rds")

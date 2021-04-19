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

# Load -----------------------------------------------------------------------
wha <- read_rds("data/wha.rds") %>%
  rename_with(tolower)

ogma <- read_rds("data/ogma.rds") %>%
  rename_with(tolower)

pa <- read_rds("data/CPCAD_Dec2020_BC_fixed.rds") %>%
  st_transform(st_crs(wha)) %>%
  mutate(area_all = as.numeric(st_area(.)))

# Add in missing dates where possible -----------------------------------------
# Split multipolygons
# - Split to match specific polygon to specific area with dates
# - Split anyway for removing overlaps below
#   (faster - https://github.com/r-spatial/sf/issues/575#issuecomment-368960968)

pa <- st_cast(pa, to = "POLYGON", warn = FALSE)

pa_wha <- wha %>%
  select(approval_date) %>%
  filter(!is.na(approval_date)) %>%
  st_cast(to = "POLYGON", warn = FALSE) %>%
  st_point_on_surface() %>%
  st_join(
    filter(pa, name_e == "Wildlife Habitat Areas") %>%
      tibble::rownames_to_column(), .
    ) %>%
  group_by(rowname) %>%
  slice_max(approval_date, with_ties = FALSE)

pa_ogma <- ogma %>%
  select(legalization_frpa_date) %>%  # Other two dates are numeric, not date
  filter(!is.na(legalization_frpa_date)) %>%
  st_cast(to = "POLYGON", warn = FALSE) %>%
  st_point_on_surface() %>%
  st_join(
    filter(pa, name_e == "Old Growth Management Areas (Mapped Legal)") %>%
      tibble::rownames_to_column(), .
  ) %>%
  group_by(rowname) %>%
  slice_max(legalization_frpa_date, with_ties = FALSE)

pa <- pa %>%
  filter(!name_e %in% c("Wildlife Habitat Areas",
                        "Old Growth Management Areas (Mapped Legal)")) %>%
  bind_rows(pa_wha, pa_ogma)

# Clean Up --------------------------------------------------------------------
# - Pick best dates
# - Order IUCN codes, calculate actual area,
# - Sort in order of priority (most to least interesting)
#    oecm (No > Yes), iucn_cat (Ia > Ib > II etc.), date (older > earlier),
#    area_all (smaller > bigger)

pa <- pa %>%
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
write_rds(pa, "data/CPCAD_Dec2020_BC_clean.rds")

# Remove overlaps -------------------------------------------------------------
#pa <- read_rds("data/CPCAD_Dec2020_BC_clean.rds")

pa_mult <- pa %>%
  mutate(area_single = as.numeric(st_area(.))) %>% # Calculate indiv area
  st_difference() %>%                             # Remove overlaps (~45min)
  st_make_valid()        # Fix Self-intersections (again!)
write_rds(pa_mult, "data/CPCAD_Dec2020_BC_clean_no_ovlps.rds")

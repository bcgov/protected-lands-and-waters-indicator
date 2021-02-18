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

# Load data
ff <- "data/CPCAD_Dec2019_BC_fixed.rds"
st_layers(ff)

pa <- st_read(ff, layer = "CPCAD_Dec2019") %>%
  clean_names()

# Filter to listed in BC or Pacific Ocean
unique(pa$loc_e)
pa <- filter(pa, str_detect(loc_e, "Pacific|British Columbia"))
unique(pa$loc_e)

# Fix Ring Self-intersections
pa <- st_make_valid(pa)

pa <- pa %>%
  # Fix geometry
  st_cast(type = "MULTIPOLYGON") %>%
  # Clean up - calculate actual area, order IUCN codes
  mutate(area1 = as.numeric(st_area(.)),
         iucn_cat = factor(iucn_cat, levels = c("Ia", "Ib", "II", "III", "IV",
                                                "V", "VI", "Yes", "N/A")),
         name_e = str_replace(name_e, "Widllife", "Wildlife"))
  # check that shape_area accurate - effectively the same size within 1 m^2
  verify(abs(shape_area - as.numeric(st_area(.))) < 1) %>%
  # Put in reverse order (start with least interesting)
  arrange(desc(oecm), desc(iucn_cat), protdate, desc(area1))

write_rds(pa, "data/CPCAD_Dec2019_BC_clean.rds")

# Split multipolygons
# (faster to split - https://github.com/r-spatial/sf/issues/575#issuecomment-368960968)

m <- st_cast(pa, to = "POLYGON", warn = FALSE)
m_diff <- st_difference(m) # ~40min
write_rds(m_diff, "data/CPCAD_Dec2019_BC_clean_no_ovlps.rds")


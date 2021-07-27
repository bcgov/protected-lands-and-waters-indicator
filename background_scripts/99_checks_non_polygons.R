#' Copyright 2021 Province of British Columbia
#'
#' Licensed under the Apache License, Version 2.0 (the "License");
#' you may not use this file except in compliance with the License.
#' You may obtain a copy of the License at
#'
#' http://www.apache.org/licenses/LICENSE-2.0
#'
#' Unless required by applicable law or agreed to in writing, software
#' distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
#' WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
#' License for the specific language governing permissions and limitations under
#' the License.
#'
#' > Render this file with:
#' >
#' > rmarkdown::render(input = "99_checks_non_polygons.R", output_dir = "share")
#'
#' ## Preamble
#' This file checks that the non-polygon shapes created by removing overlaps and
#' intersecting with ecoregions are safe to remove when creating figures.
#'
#' ## Load packages
#+ message = FALSE
library(tidyverse)
library(sf)
library(patchwork)

#' ## Load protected areas
pa_eco <- read_rds("data/CPCAD_Dec2020_BC_clean_no_ovlps_ecoregions.rds") %>%
  mutate(type = st_geometry_type(.))

#' Look for non-polygon shapes
np <- filter(pa_eco, !str_detect(type, "POLYGON"))
np_lines <- st_collection_extract(np, "LINESTRING") %>%
  mutate(length = as.numeric(st_length(geometry))) %>%
  filter(length > 25)

#' There are only `r nrow(np_lines)` LINESTRINGS (length > 25m) out
#' of ~`r nrow(pa_eco)` total shapes
#'
#' **THEREFORE:** Safe to remove all LINESTRINGS for the purpose of plotting

#' ## Plot shapes
for(i in 1:nrow(np_lines)) {
  bbx <- st_buffer(np_lines[i, ], dist = 100) %>%
    st_bbox()
  temp <- st_crop(pa_eco, bbx)
  g <- ggplot(data = temp, aes(fill = name_e)) +
    geom_sf() +
    geom_sf(data = np_lines[i, ], colour = "purple", size = 1)
  print(g)
}

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
#' > rmarkdown::render(input = "99_checks_simplification.R", output_dir = "share")
#'
#' ## Preamble
#' This file checks that the simplification of protected areas by eco and bec
#' regions/zones is acceptable for plotting purposes.
#'
#' ## Load packages
#+ message = FALSE
library(tidyverse)
library(sf)
library(patchwork)
library(bcmaps)
library(kableExtra)
library(glue)

#' ## Load Data
#'
#' Hi-res
pa_eco <- read_rds("data/CPCAD_Dec2020_BC_clean_no_ovlps_ecoregions.rds")
pa_bec <- read_rds("data/CPCAD_Dec2020_BC_clean_no_ovlps_beczones.rds")

#' Simplified
pa_eco_simp <- read_rds("out/CPCAD_Dec2020_eco_simp.rds")
pa_bec_simp <- st_read("out/CPCAD_Dec2020_bec_simp.geojson", crs = 3005)

#' Background
eco <- ecoregions(ask = FALSE) %>%
  rename_all(tolower)
bec <- st_read("data/bec_clipped_simp.geojson", crs = 3005)
eco_simp <- read_rds("out/eco_simp.rds")
bec_simp <- st_read("out/bec_simp.geojson", crs = 3005)


#' ## Compare figures - Eco Regions
cnts <- count(st_set_geometry(pa_eco, NULL), ecoregion_code, ecoregion_name) %>%
  arrange(desc(n))
cnts %>%
  kable() %>%
  kable_styling()

#+ fig.width = 12, fig.asp = 0.5
for(r in unique(pa_eco$ecoregion_code)) {

  temp <- filter(pa_eco, ecoregion_code == r)
  keep_shapes <- if_else(nrow(temp) <= 1000, TRUE, FALSE)
  keep <- case_when(nrow(temp) < 50 ~ 1,
                    nrow(temp) < 1000 ~ 0.1,
                    TRUE ~ 0.05)

  g <- ggplot() +
    theme_void() +
    scale_fill_manual(values = c("Yes" = "#004529", "No" = "#93c288"), guide = FALSE)

  g1 <- g +
    geom_sf(data = filter(eco, ecoregion_code == r), fill = "grey80", colour = NA) +
    geom_sf(data = temp, aes(fill = oecm), colour = NA) +
    labs(title = "Hi-res")

  g2 <- g +
    geom_sf(data = filter(eco_simp, ecoregion_code == r), fill = "grey80", colour = NA) +
    geom_sf(data = filter(pa_eco_simp, ecoregion_code == r), aes(fill = oecm), colour = NA) +
    labs(title = "Simplified")

  cap <- glue("Eco Region: {r}; keep_shapes = {keep_shapes}; keep = {keep}")
  print(g1 + g2 + plot_annotation(caption = cap))
}


#' ## Compare figures - BEC Zones
count(st_set_geometry(pa_bec, NULL), zone) %>%
  arrange(desc(n)) %>%
  kable() %>%
  kable_styling()

#+ fig.width = 12, fig.asp = 0.5
for(z in c("BG", "IMA", "CWH")) {
  g <- ggplot() +
    theme_void() +
    scale_fill_manual(values = c("Yes" = "#004529", "No" = "#93c288"), guide = FALSE)

  g1 <- g +
    geom_sf(data = filter(bec, zone == z), fill = "grey80", colour = NA) +
    geom_sf(data = filter(pa_bec, zone == z), aes(fill = oecm), colour = NA) +
    labs(title = "Hi-res")

  g2 <- g +
    geom_sf(data = filter(bec_simp, zone == z), fill = "grey80", colour = NA) +
    geom_sf(data = filter(pa_bec_simp, zone == z), aes(fill = oecm), colour = NA) +
    labs(title = "Simplified")

  print(g1 + g2 + plot_annotation(caption = glue("BEC Zone: {z}")))
}

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

library(targets)
library(tarchetypes)

source("R/functions.R")
tar_option_set(packages=c("dplyr", "tidyr", "readr", "purrr", "stringr", "ggplot2",
                          "lubridate", "glue", "assertr", "sf", "bcmaps", "bcdata",
                          "rmapshaper", "geojsonio", "ggiraph", "cowplot", "shiny",
                          "knitr", "rmarkdown", "kableExtra", "tibble"),
               imports=c("bcmaps", "bcdata"))

# load datasets ------------------------------------------------------------------------------------

load_data <- list(
  tar_target(wha_data, get_wha_data()),
  tar_target(ogma_data, get_ogma_data()),
  tar_target(pa_data, get_cpcad_bc_data(crs="data/wha.rds")),
  tar_target(ecoregions, load_ecoregions()),
  tar_target(bec, load_bec())
)

# clean data --------------------------------------------------------------

clean_data <- list(
  tar_target(pa_wha, fill_in_dates(data=wha_data, column = "approval_date",
                                   join= pa_data, landtype = "Wildlife Habitat Areas",
                                   output=pa_wha)),
  tar_target(pa_ogma, fill_in_dates(data=ogma_data, column = "legalization_frpa_date",
                                    join= pa_data, landtype = "Old Growth Management Areas (Mapped Legal)",
                                    output=pa_ogma)),
  tar_target(clean_dates_pa, clean_up_dates(pa_data, pa_wha, pa_ogma, clean_dates_pa)),
  tar_target(clean_pa, remove_overlaps(clean_dates_pa, clean_pa))
)

# intersect data ----------------------------------------------------------

intersect_data <- list(
  tar_target(clipped_bec, clip_bec_to_bc_boundary(bec)),
  tar_target(pa_eco, intersect_pa(ecoregions, clean_pa, pa_eco)),
  tar_target(pa_bec, intersect_pa(clipped_bec, clean_pa, pa_bec))
)


# simplify spatial data  --------------------------------------------------
simplify_data <- list(
  tar_target(map_eco, simplify_ecoregions(pa_eco)),
  tar_target(map_bec, simplify_beczones(pa_bec)),
  tar_target(map_eco_background, simplify_eco_background(ecoregions)),
  tar_target(map_bec_background, simplify_bec_background())
)

# analyze and prepare for visualization -----------------------------------
analyze_data <- list(
  tar_target(ecoregion_totals, find_ecoregion_size(ecoregions)),
  tar_target(pa_eco_sum, protected_area_by_eco(pa_eco, ecoregion_totals)),
  tar_target(pa_bec_sum, protected_area_by_bec(bec, pa_bec)),
  tar_target(total_prot_area, protected_area_totals(pa_eco, pa_eco_sum))
)

# supplemental bec zone plots ---------------------------------------------
plot_data <- list(
  tar_target(bec_plot_type, plot_by_bec_zone(pa_bec_sum)),
  tar_target(bec_plot_total, plot_bec_zone_totals(pa_bec_sum)),
  tar_target(bec_map_figure, bec_zone_map(map_bec))
)

# targets pipeline --------------------------------------------------------

list(
  load_data,
  clean_data,
  intersect_data,
  simplify_data,
  analyze_data,
  plot_data
  #...
)
#add list(,
#tar_targets() for each intermediate step of workflow)

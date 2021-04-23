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
  tar_target(load_ogma, get_ogma_data()),
  tar_target(load_wha, get_wha_data()),
  tar_target(load_pa, get_cpcad_bc_data(crs="data/wha.rds"))
)


intersect_data <- list(
  tar_target(pa_wha, fill_in_dates(data="data/wha.rds", column = "approval_date",
                                   join= "data/CPCAD_Dec2020_BC_fixed.rds", landtype = "Wildlife Habitat Areas",
                                   output=pa_wha)),
  tar_target(pa_ogma, fill_in_dates(data="data/ogma.rds", column = "legalization_frpa_date",
                                    join= "data/CPCAD_Dec2020_BC_fixed.rds", landtype = "Old Growth Management Areas (Mapped Legal)",
                                    output=pa_ogma))
)


# pipeline
list(
  load_data,
  intersect_data
  #...
)
#add list(,
#tar_targets() for each intermediate step of workflow)

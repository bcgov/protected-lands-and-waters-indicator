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
                          "knitr", "rmarkdown", "kableExtra"))

# load datasets ------------------------------------------------------------------------------------

load_data <- list(
  tar_target(get_cpcad_bc_data, get_wha_data, get_ogma_data)
)





# pipeline
list(
  load_data,
  #...
)
#add list(,
#tar_targets() for each intermediate step of workflow)

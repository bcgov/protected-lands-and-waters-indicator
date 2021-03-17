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

package_list <- c("dplyr", "tidyr", "readr", "purrr", "stringr", "ggplot2",
                  "lubridate", "glue", "assertr", "sf", "bcmaps", "bcdata",
                  "rmapshaper", "geojsonio", "ggiraph", "cowplot", "shiny",
                  "knitr", "rmarkdown", "kableExtra")
package_new <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(package_new)) install.packages(package_new)

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(ggplot2)
library(lubridate)
library(glue)
library(assertr)
library(sf)
library(bcmaps)
library(bcdata)
library(rmapshaper)
library(geojsonio)

if(!dir.exists("data")) dir.create("data")
if(!dir.exists("share")) dir.create("share")
if(!dir.exists("out")) dir.create("out")

# Create bc map for reset button
bc_button <- bc_bound() %>%
  st_geometry() %>%
  ms_simplify(0.02, explode = TRUE, keep_shapes = FALSE) %>%
  ggplot() +
  theme_void() +
  ggiraph::geom_sf_interactive(fill = "black", data_id = "reset")
write_rds(bc_button, "out/bc_button.rds")

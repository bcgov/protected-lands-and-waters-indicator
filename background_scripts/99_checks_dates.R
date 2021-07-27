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
#' > rmarkdown::render(input = "99_checks_dates.R", output_dir = "share")
#'
#' ## Preamble
#' This file checks that how many dates are fixed and how many are still missing
#'
#' ## Load packages
#+ message = FALSE
library(tidyverse)
library(sf)
library(patchwork)

pa <- read_rds("data/CPCAD_Dec2020_BC_clean_no_ovlps.rds") %>%
  st_set_geometry(NULL)

#' Are there any polygons with more than one date assigned?
pa %>%
  select(name_e, protdate, approval_date, legalization_frpa_date) %>%
  filter((!is.na(protdate) & !is.na(approval_date)) |
           (!is.na(protdate) & !is.na(legalization_frpa_date)) |
           (!is.na(approval_date) & !is.na(legalization_frpa_date))) %>%
  distinct() %>%
  nrow()

#' How many missing dates at the start?
pa %>%
  filter(is.na(protdate)) %>%
  group_by(name_e) %>%
  summarise(area=sum(area_all), n=n()) %>%
  arrange(desc(n))


#' How many missing dates after adding dates?
pa %>%
  filter(is.na(date)) %>%
  group_by(name_e) %>%
  summarise(area=sum(area_all), n=n()) %>%
  arrange(desc(n))

pa %>%
  filter(is.na(date)) %>%
  filter(name_e == "Old Growth Management Areas (Mapped Legal)") %>%
  arrange(desc(parent_id))

ogma_no_date <- pa %>%
  filter(is.na(date)) %>%
  filter(name_e == "Old Growth Management Areas (Mapped Legal)") %>%
  arrange(desc(parent_id))
geojson_write(ogma_no_date, file = "data/ogma_no_dates.geojson")

wha_no_date <- pa %>%
  filter(is.na(date)) %>%
  filter(name_e == "Wildlife Habitat Areas") %>%
  arrange(desc(parent_id))
geojson_write(wha_no_date, file = "data/wha_no_dates.geojson")

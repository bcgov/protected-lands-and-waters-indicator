# Copyright 2016 Province of British Columbia
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

which_min <- function(x) {
  if (all(is.na(x))) {
    fun <- first
  } else {
    fun <- which.min
  }
  fun(x)
}

## Given a vector of values, if the first n values are 0 or NA, make them NA up
## up to the first non-zero/non-NA value
fill_initial_na <- function(x) {
  if (is.na(x[1]) || x[1] == 0) {
    x[is.na(x)] <- 0
    runs <- rle(x)
    x[1:runs$lengths[1]] <- NA
  }
  x
}

iucn_cats <- function() ordered(c("Ia", "Ib", "II", "III", "IV", "V", "VI"))

factor_iucn_cats <- function(x) {
  factor(x, levels = iucn_cats(), ordered = TRUE)
}

get_gov_level <- function(x) {
  if (any(grepl("federal", x, ignore.case = TRUE))) { # let federal take precedence ??
    gov <- "Federal"
  } else if (any(grepl("sub-national", x, ignore.case = TRUE))) {
    gov <- "Provincial"
  } else {
    gov <- NA_character_
  }

  gov
}

bind_spdf <- function(x, y) {
  len_x <- length(x)
  len_y <- length(y)
  x <- spChFIDs(x, as.character(1:len_x))
  y <- spChFIDs(y, as.character((len_x + 1):(len_x + len_y)))
  rbind(x, y)
}

gg_fortify <- function(x) {
  if (!require("maptools")) stop("maptools is not installed")
  if (!requireNamespace("ggplot2")) stop("ggplot2 is not installed.")
  if (!requireNamespace("dplyr")) stop("dplyr is not installed.")
  x@data$ggid <- rownames(x@data)
  x_points <- ggplot2::fortify(x, region = "ggid")
  x_df <- dplyr::left_join(x_points, x@data, by = c("id" = "ggid"))
  x_df
}

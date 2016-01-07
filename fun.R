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

#' Get the desried attribute of overlapping polygons from a spdf that is the
#' the result of raster::union on a single SPDF
#'
#' @param unioned_sp the SPDF that is the result of running raster::union on \code{orig_sp}
#' @param orig_sp the original SPDF containing the desired attributes
#' @param col the column in \code{orig_sp} from which to retrieve attributes
#' @param fun function to determine the resulting single attribute from overlapping polygons
#' @param return_type The type of data expected to be returned. One of:
#'    "logical", "character", "numeric", "double", "integer", "complex".
#' @param ... other paramaters passed on to \code{fun}
#'
#' @return
#' @export
#'
#' @examples
get_unioned_attribute <- function(unioned_sp, orig_sp, col, fun, return_type, ...) {
  if (!return_type %in% c("logical", "character", "numeric", "double", "integer", "complex")) {
    stop("return_type is not a valid data type")
  }
  return_call <- call(return_type, 1)

  unioned_ids <- get_unioned_ids(unioned_sp)

  vapply(unioned_ids, function(x) {
    fun(orig_sp[[col]][x], ...)
  }, eval(return_call))
}

## Function to get the original polygon ids that make up each new polygon in the
## unioned product (the result of raster:union(SPDF, missing))
get_unioned_ids <- function(unioned_sp) {
  id_cols <- grep("^ID\\.", names(unioned_sp@data))
  unioned_sp_data <- as.matrix(unioned_sp@data[, id_cols])
  colnames(unioned_sp_data) <- gsub("ID\\.", "", colnames(unioned_sp_data))
  unioned_ids <- apply(unioned_sp_data, 1, function(i) {
    as.numeric(colnames(unioned_sp_data)[i > 0])
  })
  names(unioned_ids) <- rownames(unioned_sp_data)
  unioned_ids
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

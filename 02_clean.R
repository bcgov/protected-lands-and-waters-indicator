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

# Spatial packages
library(rgdal)
library(sp)
library(rgeos)
library(raster)
library(bcmaps) # install using devtools::install_github("bcgov/bcmaps")
library(rmapshaper) # install using devtools::install_github("ateucher/rmapshaper")

## The CARTS database is downloadable from the Canadian Council on
## Ecological Areas here: http://www.ccea.org/carts/
carts <- readOGR("data/CARTS_Update_31122015.gdb", "CARTS_Update_31122015_Without_Qc", stringsAsFactors = FALSE)

## Extract just BC
bc_carts_orig <- carts[carts$LOC_E == "British Columbia", ]
rm(carts)

## Transform CRS of bc_carts to BC Albers
bc_carts <- spTransform(bc_carts_orig, CRS(proj4string(ecoregions)))

#### Simplify for testing
## devtools::install_github("ateucher/rmapshaper")
# library(rmapshaper)
# bc_carts <- ms_simplify(bc_carts, 0.01, keep_shapes = TRUE)
# ecoregions <- ms_simplify(ecoregions, 0.01, keep_shapes = TRUE)
####

## Check validity of polygons of bc_carts, fix with gBuffer trick, and check again:
if (any(!gIsValid(bc_carts, byid = TRUE))) {
  bc_carts <- gBuffer(bc_carts, byid = TRUE, width = 0)
  any(!gIsValid(bc_carts, byid = TRUE))
}

# Get separate layers for terrestrial and marine protected areas
bc_carts_t <- bc_carts[bc_carts$BIOME == "T", ]
bc_carts_m <- bc_carts[bc_carts$BIOME == "M", ]

## Union shapes to deal with overlaps (loses attributes but makes a dataframe of the shapes that combine to make each polygon)
bc_carts_t_unioned <- raster::union(bc_carts_t) # This is incredibly slow.
bc_carts_m_unioned <- raster::union(bc_carts_m)

## Remove marine ecoregions
m_ecoregions <- c("GPB", "HCS", "IPS", "OPS", "SBC", "TPC")
ecoregions_t <- ecoregions[!ecoregions$CRGNCD %in% m_ecoregions, ]

## Remove terrestrial areas from marine ecoregions
ecoregions_m <- ms_erase(ecoregions[ecoregions$CRGNCD %in% m_ecoregions, ],
                         bc_bound_hres)


# BEC ---------------------------------------------------------------------

bec <- readOGR("data/BEC_POLY", "BEC_POLY_polygon", stringsAsFactors = FALSE)
bec <- disaggregate(bec)
bc_bound_hres <- disaggregate(bc_bound_hres)
bec_t <- raster::intersect(bec, bc_bound_hres)
bec_t <- aggregate(bec_t, by = "OBJECTID")
bec_t$area <- gArea(bec_t, byid = TRUE)

dir.create("tmp", showWarnings = FALSE)

save.image(file = "tmp/input_layers.rda")

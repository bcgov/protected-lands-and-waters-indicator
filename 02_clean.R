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

dir.create("tmp", showWarnings = FALSE)

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

save(list = ls(), file = "tmp/bc_carts_clean.rda")
rm(list = ls())

## Remove marine ecoregions
m_ecoregions <- c("GPB", "HCS", "IPS", "OPS", "SBC", "TPC")
ecoregions_t <- ecoregions[!ecoregions$CRGNCD %in% m_ecoregions, ]

## Remove terrestrial areas from marine ecoregions
ecoregions_m <- ms_erase(ecoregions[ecoregions$CRGNCD %in% m_ecoregions, ],
                         bc_bound_hres)

## Create simplified versions for visualization
ecoregions_t_simp <- ms_simplify(ecoregions_t, 0.01)
ecoregions_m_simp <- ms_simplify(ecoregions_m, 0.01)

save(list = ls(), file = "tmp/ecoregions_clean.rda")
rm(list = ls())

# BEC ---------------------------------------------------------------------

## Using rmapshaper - runs out of memory
# bec <- readOGR("data/BEC_POLY", "BEC_POLY_polygon", stringsAsFactors = FALSE)
# bec_t <- rmapshaper::ms_clip(bec, bc_bound_hres)

## Using mapshaper on the command line. Requires Node installed (https://nodejs.org),
## and install mapshaper with: 'npm install -g mapshaper'
geojsonio::geojson_write(bc_bound_hres, file = "data/bc_bound.geojson")
system("mapshaper data/BEC_POLY/BEC_POLY_polygon.shp -clip data/bc_bound.geojson -o data/bec_clip.shp")
unlink(paste0("data/", c("bc_bound.geojson", "bec_clip.*")))
bec_t <- readOGR("data", "bec_clip", stringsAsFactors = FALSE)
if (any(!gIsValid(bec_t, byid = TRUE))) {
  bec_t <- gBuffer(bec_t, byid = TRUE, width = 0)
  any(!gIsValid(bec_t, byid = TRUE))
}

bec_t$area <- gArea(bec_t, byid = TRUE) * 1e-4 # convert to hectares (makes field too wide for writing as shp)
unlink(paste0("data/", c("bec_t.*", "bec_t_simp.*")))
writeOGR(bec_t, "data", "bec_t", "ESRI Shapefile")
system("mapshaper data/bec_t.shp -explode -simplify 0.01 keep-shapes -o data/bec_t_simp.shp")
bec_t_simp <- readOGR("data", "bec_t_simp", stringsAsFactors = FALSE)
## Repair orphaned hole
bec_t_simp <- gBuffer(bec_t_simp, byid = TRUE, width = 0)
bec_t_simp <- raster::aggregate(bec_t_simp, by = "OBJECTID")

# bec_t_simp <- ms_simplify(bec_t, keep = 0.01, keep_shapes = TRUE)

save(list = ls(), file = "tmp/bec_clean.rda")

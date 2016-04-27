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

source("fun.R")

dir.create("tmp", showWarnings = FALSE)

## The CARTS database is downloadable from the Canadian Council on
## Ecological Areas here: http://www.ccea.org/carts/
carts <- readOGR("data/CARTS_Update_31122015.gdb", "CARTS_Update_31122015_WithoutQc", stringsAsFactors = FALSE)

## Read in the Conservation Lands database from: http://catalogue.data.gov.bc.ca/dataset/conservation-lands
cl_gdb <- "data/BCGW_conservation_lands_fgdb/WCL_CONSERVATION_LANDS_SP.gdb"
conservation_lands <- readOGR(cl_gdb, ogrListLayers(cl_gdb)[1], stringsAsFactors = FALSE)
rm(cl_gdb)

## Read in NGO conservation lands
fee_simple_ngo_lands <- readOGR("data", "BC_NGO_ConsDB_FeeSimple_31Dec2014_updated02Nov2015", stringsAsFactors = FALSE)

## Extract just BC from CARTS
bc_carts <- carts[carts$LOC_E %in% c("British Columbia", "Offshore Pacific Marine"), ]
rm(carts)

## Subset only the Administrated Lands - Acquisitions and Leases
bc_admin_lands <- conservation_lands[conservation_lands$CONSERVATION_LAND_TYPE == "Administered Lands" &
                                           (conservation_lands$TENURE_TYPE == "Acquisition" |
                                              conservation_lands$TENURE_TYPE == "Lease" |
                                              conservation_lands$TENURE_TYPE == "Transfer of Administration/Control"), ]

bc_admin_lands$PROTDATE <- max(bc_carts$PROTDATE, na.rm = TRUE)

## Transform CRS of all layers to BC Albers
bc_carts <- transform_albers(bc_carts)
bc_admin_lands <- transform_albers(bc_admin_lands)
fee_simple_ngo_lands <- transform_albers(fee_simple_ngo_lands)

#### Simplify for testing
## devtools::install_github("ateucher/rmapshaper")
# library(rmapshaper)
# bc_carts <- ms_simplify(bc_carts, 0.01, keep_shapes = TRUE)
# ecoregions <- ms_simplify(ecoregions, 0.01, keep_shapes = TRUE)
####

## Check validity of polygons of bc_carts, fix with gBuffer trick, and check again:
bc_carts <- fix_geometry(bc_carts)
bc_admin_lands <- fix_geometry(bc_admin_lands)
fee_simple_ngo_lands <- fix_geometry(fee_simple_ngo_lands)

bc_admin_lands_unioned <- single_sp_union(bc_admin_lands)
fee_simple_ngo_lands_unioned <- single_sp_union(fee_simple_ngo_lands)
bc_carts_unioned <- single_sp_union(bc_carts)

non_carts_unioned <- raster::union(bc_admin_lands_unioned, fee_simple_ngo_lands_unioned)

all_unioned <- raster::union(bc_carts_unioned, non_carts_unioned)

# # Get separate layers for terrestrial and marine protected areas
# bc_carts_t <- bc_carts[bc_carts$BIOME == "T", ]
# bc_carts_m <- bc_carts[bc_carts$BIOME == "M", ]
#
# ## Union shapes to deal with overlaps (loses attributes but makes a dataframe of the shapes that combine to make each polygon)
# bc_carts_t_unioned <- raster::union(bc_carts_t) # This is incredibly slow.
# bc_carts_m_unioned <- raster::union(bc_carts_m)

save(list = ls(), file = "tmp/prot_areas_clean.rda")
rm(list = ls())


# Process Ecoregions ------------------------------------------------------

## Marine ecoregions
m_ecoregions <- c("HCS", "IPS", "OPS", "SBC", "TPC")

## Extract the terrestrial and marine portions of GPB
gpb_terrestrial <- ms_clip(ecoregions[ecoregions$CRGNCD == "GPB",],
                           bc_bound_hres)
gpb_marine <- ms_erase(ecoregions[ecoregions$CRGNCD == "GPB",],
                       bc_bound_hres)
## Fix it up:
gpb_terrestrial <- fix_self_intersect(gpb_terrestrial)
gpb_marine <- fix_self_intersect(gpb_marine)

## Add terrestrial portion of GPB back to terrestrial ecoregions
ecoregions_t <- rbind(ecoregions[!ecoregions$CRGNCD %in% c("GPB", m_ecoregions), ],
                          gpb_terrestrial[, setdiff(names(gpb_terrestrial), "rmapshaperid")])

## Add marine portion of GPB back to marine ecoregions
ecoregions_m <- rbind(ecoregions[ecoregions$CRGNCD %in% m_ecoregions, ],
                      gpb_marine[, setdiff(names(gpb_terrestrial), "rmapshaperid")])

ecoregions_t$area <- gArea(ecoregions_t, byid = TRUE)
ecoregions_m$area <- gArea(ecoregions_m, byid = TRUE)

## Create simplified versions for visualization
ecoregions_t_simp <- ms_simplify(ecoregions_t, 0.01)
ecoregions_m_simp <- ms_simplify(ecoregions_m, 0.01)

rm(list = c("gpb_marine", "gpb_terrestrial"))
save(list = ls(), file = "tmp/ecoregions_clean.rda")
rm(list = ls())

# BEC ---------------------------------------------------------------------

## Using rmapshaper - runs out of memory
# bec <- readOGR("data/BEC_POLY", "BEC_POLY_polygon", stringsAsFactors = FALSE)
# bec_t <- rmapshaper::ms_clip(bec, bc_bound_hres)

## Using mapshaper on the command line. Requires Node installed (https://nodejs.org),
## and install mapshaper with: 'npm install -g mapshaper'
unlink(paste0("data/", c("bc_bound.geojson", "bec_clip*")))
geojsonio::geojson_write(bc_bound_hres, file = "data/bc_bound.geojson")
system("mapshaper data/BEC_POLY/BEC_POLY_polygon.shp -clip data/bc_bound.geojson -explode -o data/bec_clip.shp")

bec_t <- readOGR("data", "bec_clip", stringsAsFactors = FALSE)
if (any(!gIsValid(bec_t, byid = TRUE))) {
  bec_t <- gBuffer(bec_t, byid = TRUE, width = 0)
  # bec_t <- createSPComment(bec_t)
  any(!gIsValid(bec_t, byid = TRUE))
}

bec_t$poly_id <- row.names(bec_t)

bec_t$area <- gArea(bec_t, byid = TRUE) * 1e-4 # convert to hectares (makes field too wide for writing as shp)
unlink("data/bec_t*")
writeOGR(bec_t, "data", "bec_t", "ESRI Shapefile")
system("mapshaper data/bec_t.shp -simplify 0.01 keep-shapes -o data/bec_t_simp.shp")
bec_t_simp <- readOGR("data", "bec_t_simp", stringsAsFactors = FALSE)
## Repair orphaned holes
bec_t_simp <- gBuffer(bec_t_simp, byid = TRUE, width = 0)

# bec_t_simp <- ms_simplify(bec_t, keep = 0.01, keep_shapes = TRUE)

save(list = ls(), file = "tmp/bec_clean.rda")

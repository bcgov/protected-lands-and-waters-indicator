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

fee_simple_ngo_lands$prot_date <- as.numeric(substr(fee_simple_ngo_lands$SecDate1, 1, 4))

## Extract just BC from CARTS
bc_carts <- carts[carts$LOC_E %in% c("British Columbia", "Offshore Pacific Marine"), ]
rm(carts)

## Subset only the Administrated Lands - Acquisitions and Leases
bc_admin_lands <- conservation_lands[conservation_lands$CONSERVATION_LAND_TYPE == "Administered Lands" &
                                           (conservation_lands$TENURE_TYPE == "Acquisition" |
                                              conservation_lands$TENURE_TYPE == "Lease" |
                                              conservation_lands$TENURE_TYPE == "Transfer of Administration/Control"), ]
rm(conservation_lands)

## Transform CRS of all layers to BC Albers
bc_carts <- transform_bc_albers(bc_carts)
bc_admin_lands <- transform_bc_albers(bc_admin_lands)
fee_simple_ngo_lands <- transform_bc_albers(fee_simple_ngo_lands)

#### Simplify for testing
## devtools::install_github("ateucher/rmapshaper")
# library(rmapshaper)
# bc_carts <- ms_simplify(bc_carts, 0.01, keep_shapes = TRUE)
# ecoregions <- ms_simplify(ecoregions, 0.01, keep_shapes = TRUE)
####

## Check validity of polygons of bc_carts, fix with gBuffer trick, and check again:
bc_carts <- fix_self_intersect(bc_carts)
bc_admin_lands <- fix_self_intersect(bc_admin_lands)
fee_simple_ngo_lands <- fix_self_intersect(fee_simple_ngo_lands)

## Convert IUCN category to an ordered factor
bc_carts$IUCN_CAT <- factor_iucn_cats(bc_carts$IUCN_CAT)

## Union CARTS
bc_carts_agg <- raster::aggregate(bc_carts, by = "PROTDATE")
bc_carts_agg <- fix_self_intersect(bc_carts_agg)
bc_carts_agg_unioned <- self_union(bc_carts_agg)
## Get the earliest year of protection for polygon segments that overlap
bc_carts_agg_unioned$prot_date <- sapply(bc_carts_agg_unioned$union_df, min, na.rm = TRUE)
bc_carts_agg_unioned <- raster::aggregate(bc_carts_agg_unioned, by = "prot_date")

## Union and get attributes of Fee Simple lands
fee_simple_ngo_lands_agg <- raster::aggregate(fee_simple_ngo_lands, by = "prot_date")
fee_simple_ngo_lands_unioned <- self_union(fee_simple_ngo_lands_agg)
fee_simple_ngo_lands_unioned$prot_date <- sapply(fee_simple_ngo_lands_unioned$union_df, min, na.rm = TRUE)
fee_simple_ngo_lands_unioned$prot_date[is.infinite(fee_simple_ngo_lands_unioned$prot_date)] <- NA
fee_simple_ngo_lands_unioned$designation_type <- "NGO Conservation Areas"
fee_simple_ngo_lands_unioned$designation <- "Fee Simple"
fee_simple_ngo_lands_unioned$prot_area <- gArea(fee_simple_ngo_lands_unioned, byid = TRUE)
fee_simple_ngo_lands_agg_unioned <- raster::aggregate(fee_simple_ngo_lands_unioned, by = "prot_date")

## Union and get attributes of BC Administered lands
bc_admin_lands_agg <- raster::aggregate(bc_admin_lands, by = "TENURE_TYPE")
bc_admin_lands_unioned <- self_union(bc_admin_lands_agg)
bc_admin_lands_unioned$prot_date <- max(c(bc_carts_agg_unioned$prot_date,
                                          fee_simple_ngo_lands_unioned$prot_date), na.rm = TRUE)
bc_admin_lands_unioned$designation_type <- "BC Administered Conservation Lands"
bc_admin_lands_unioned$designation <- sapply(bc_admin_lands_unioned$union_df, min, na.rm = TRUE)
bc_admin_lands_unioned$prot_area <- gArea(bc_admin_lands_unioned, byid = TRUE)
bc_admin_lands_agg_unioned <- raster::aggregate(bc_admin_lands_unioned, by = "prot_date")

## Union the first two layers
admin_fee_simple_unioned <- raster::union(bc_admin_lands_agg_unioned, fee_simple_ngo_lands_agg_unioned)
admin_fee_simple_unioned$prot_date <- pmin(admin_fee_simple_unioned$prot_date.1,
                                           admin_fee_simple_unioned$prot_date.2, na.rm = TRUE)
admin_fee_simple_unioned <- raster::aggregate(admin_fee_simple_unioned, by = "prot_date")

## Finally add the CARTS data for BC
prot_areas_unioned <- raster::union(bc_carts_agg_unioned, admin_fee_simple_unioned)
prot_areas_unioned$prot_date <- pmin(prot_areas_unioned$prot_date.1,
                                     prot_areas_unioned$prot_date.2, na.rm = TRUE)
prot_areas_unioned$prot_date[is.infinite(prot_areas_unioned$prot_date)] <- max(c(bc_carts_agg_unioned$prot_date,
                                                                                 fee_simple_ngo_lands_unioned$prot_date), na.rm = TRUE)
prot_areas_agg <- raster::aggregate(prot_areas_unioned, by = "prot_date")

saveRDS(fee_simple_ngo_lands_unioned, "tmp/fee_simple_ngo_lands_unioned.rds")
saveRDS(bc_admin_lands_unioned, "tmp/bc_admin_lands_unioned.rds")
saveRDS(bc_carts_agg_unioned, "tmp/bc_carts_agg_unioned.rds")
saveRDS(admin_fee_simple_unioned, "tmp/admin_fee_simple_unioned.rds")
saveRDS(prot_areas_unioned, "tmp/prot_areas_unioned.rds")

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

unlink("data/bec_t*")
writeOGR(bec_t, "data", "bec_t", "ESRI Shapefile")
system("mapshaper data/bec_t.shp -simplify 0.01 keep-shapes -o data/bec_t_simp.shp")
bec_t_simp <- readOGR("data", "bec_t_simp", stringsAsFactors = FALSE)
## Repair orphaned holes
bec_t_simp <- gBuffer(bec_t_simp, byid = TRUE, width = 0)

## Put area back in m2
bec_t$area <- gArea(bec_t, byid = TRUE)
bec_t_simp$area <- bec_t$area

# bec_t_simp <- ms_simplify(bec_t, keep = 0.01, keep_shapes = TRUE)

save("bec_t", "bec_t_simp", file = "tmp/bec_clean.rda")

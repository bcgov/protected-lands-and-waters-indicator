library(sp)
library(raster)
library(rgeos)
library(dplyr)
library(readr)

options(scipen = 5)

load("tmp/prot_areas_clean.rda")
load("tmp/ecoregions_clean.rda")
load("tmp/bec_clean.rda")
load("tmp/analyzed.rda")
ngo_summary <- read_csv("data/ngo_fee_simple_reg_int_summary.csv", col_types = "cdcdci")






bc_carts_des <- raster::aggregate(bc_carts, by = "TYPE_E")
fee_simple_des <- raster::aggregate(fee_simple_ngo_lands, by = "SecType1")




# Individual land designations by BEC -------------------------------------

fee_simple_bec <- raster::intersect(bec_t, fee_simple_des)
fee_simple_bec$prot_area <- rgeos::gArea(fee_simple_bec, byid = TRUE)

bc_admin_lands_bec <- raster::intersect(bec_t, bc_admin_lands_agg)
bc_admin_lands_bec$prot_area <- rgeos::gArea(bc_admin_lands_bec, byid = TRUE)

bc_carts_bec <- raster::intersect(bec_t, bc_carts_des)
bc_carts_bec$prot_area <- rgeos::gArea(bc_carts_bec, byid = TRUE)

# Get total size of terrestrial area of each zone
bec_t_summary <- bec_t@data %>%
  group_by(ZONE_NAME) %>%
  summarize(total_zone_area_ha = sum(area) * 1e-4)

reg_int_bec_summary <- reg_int_bec %>%
  left_join(bec_t_summary, by = "ZONE_NAME") %>%
  mutate(category = "Private Conservation Lands",
         designation = "Registerable Interests",
         prot_area_ha = round(prot_area * 1e-4, 3),
         percent_protected = round((prot_area_ha / total_zone_area_ha * 100), 4)) %>%
  dplyr::select(-prot_area_overlaps_removed, -prot_area)

# Summarize fee simple
fee_simple_bec_summary <- fee_simple_bec@data %>%
  group_by(ZONE_NAME) %>%
  summarize(prot_area_ha = round(sum(prot_area) * 1e-4, 3)) %>%
  left_join(bec_t_summary, by = "ZONE_NAME") %>%
  mutate(category = "Private Conservation Lands",
         designation = "Fee Simple",
         percent_protected = round((prot_area_ha / total_zone_area_ha * 100), 4))

# Summarize admin areas
admin_lands_bec_summary <- bc_admin_lands_bec@data %>%
  group_by(ZONE_NAME, designation = TENURE_TYPE) %>%
  summarize(prot_area_ha = round(sum(prot_area) * 1e-4, 3)) %>%
  left_join(bec_t_summary, by = "ZONE_NAME") %>%
  mutate(category = "BC Administered Lands",
         percent_protected = round((prot_area_ha / total_zone_area_ha * 100), 4))

# Summarize carts data
bc_carts_bec_summary <- bc_carts_bec@data %>%
  group_by(ZONE_NAME, designation = TYPE_E) %>%
  summarize(prot_area_ha = round(sum(prot_area) * 1e-4, 3)) %>%
  left_join(bec_t_summary, by = "ZONE_NAME") %>%
  mutate(category = "Provincial and Federal Protected Lands",
         percent_protected = round((prot_area_ha / total_zone_area_ha * 100), 4))

designations_bec <- bind_rows(reg_int_bec_summary, fee_simple_bec_summary,
                              admin_lands_bec_summary, bc_carts_bec_summary) %>%
  complete(nesting(ZONE_NAME, total_zone_area_ha),
           nesting(category, designation),
           fill = list(prot_area_ha = 0, percent_protected = 0))

## Output csv files
## Prep summary for interactive web viz
cum_summary_t$ecoregion <- tools::toTitleCase(tolower(cum_summary_t$ecoregion))
cum_summary_t_viz <- cum_summary_t[cum_summary_t$tot_protected > 0, ]
write_csv(cum_summary_t_viz, path = "out/ecoregion_cons_lands_trends.csv")

write_csv(designations_bec, "out/land_designations_bec_zone.csv")
write_csv(private_lands_summary, "out/private_lands_summary_bec_zone.csv")

library(dplyr)
library(readr)
library(sp)
library(raster)
library(rgeos)

options(scipen = 5)

load("C:/_dev/protected_areas_analysis/tmp/bec_clean.rda")
load("C:/_dev/protected_areas_analysis/tmp/prot_areas_clean_new.rda")
reg_int_bec <- read_csv("data/reg_interests_bec_summary.csv")

# bec_zones <- raster::aggregate(bec_t, by = "ZONE_NAME")
# bec_zones$area <- rgeos::gArea(bec_zones, byid = TRUE)

bc_carts_des <- raster::aggregate(bc_carts, by = "TYPE_E")

fee_simple_des <- raster::aggregate(fee_simple_ngo_lands, by = "SecType1")

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
                              admin_lands_bec_summary, bc_carts_bec_summary)


designations_bec <- complete(designations_bec,
         nesting(ZONE_NAME, total_zone_area_ha),
         nesting(category, designation),
         fill = list(prot_area_ha = 0, percent_protected = 0))

## Just the private lands:
private_lands_summary <- filter(designations_bec,
                                category == "Private Conservation Lands")

private_lands_summary <- private_lands_summary %>%
  group_by(ZONE_NAME, total_zone_area_ha, category) %>%
  summarize(designation = "All private conservation lands",
            prot_area_ha = sum(prot_area_ha)) %>%
  mutate(percent_protected = round((prot_area_ha / total_zone_area_ha * 100), 4)) %>%
  bind_rows(private_lands_summary)

write_csv(designations_bec, "out/land_designations_bec_zone.csv")
write_csv(private_lands_summary, "out/private_lands_summary_bec_zone.csv")

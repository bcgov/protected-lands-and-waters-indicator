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

## General and plotting packages
library(readr) # write_csv
library(dplyr) # data muanipulation and summaries
library(ggplot2) # Plottiong
library(envreportutils) # for order_df and theme_soe ###
library(RColorBrewer) # Colour for plotting
library(ggthemes) # theme_map

## Spatial packages
library(sp)
library(geojsonio) # writing out geojson file of ecoregions
library(rmapshaper) # ms_dissolve
library(maptools) ###
library(bcmaps) ###
library(rgeos) ###

load("tmp/prot_areas_clean.rda")
load("tmp/ecoregions_clean.rda")
load("tmp/bec_clean.rda")
load("tmp/analyzed.rda")

source("fun.R")

# Terrestrial -------------------------------------------------------------

## Prep data frame for visualizations
cum_summary_t_eco <- order_df(cum_summary_t_eco, "ecoregion", "cum_percent_protected",
                              max, na.rm = TRUE, desc = TRUE)
cum_summary_t_eco$decade <- floor(cum_summary_t_eco$prot_date / 10) * 10

###############################################################################
## Facet line-chart by ecoregion of cumulative percent protected over time

# Make a data frame of labels for current % protected
current_eco_t <- cum_summary_t_eco[cum_summary_t_eco$prot_date == max(cum_summary_t_eco$prot_date), ]

(ecoregion_t_facet_plot <- ggplot(cum_summary_t_eco,
                               aes(x = prot_date, y = cum_percent_protected)) +
  geom_path(colour = "forestgreen") +
  facet_wrap(~ecoregion, labeller = label_wrap_gen(width = 20), ncol = 6) +
  scale_x_continuous(expand = c(0,0), breaks = function(x) round(seq(min(x),max(x), length.out = 5))) +
  scale_y_continuous(breaks = seq(0,100, length.out = 3)) +
  labs(x = "Year", y = "Cumulative Percent of Ecoregion Protected") +
  theme_minimal() +
  theme(panel.margin.x = unit(1.5, "lines"),
        axis.text = element_text(size = 8)) +
  geom_text(data = current_eco_t, x = 2002, y = 80,
            aes(label = paste0(round(cum_percent_protected, 1), "%")),
            size = 2.5))
  # theme_soe_facet() +
  # theme(panel.margin = unit(1, "mm"))

# plot(ecoregion_t_facet_plot)

###############################################################################
## Bar chart of current protection by ecoregion
# (summary_eco_t_plot <- ggplot(current_eco_t, aes(x = ecoregion, y = cum_percent_protected, colour = is_bc)) +
#   scale_colour_manual(guide = "none", values = c(NA, "royalblue3")) +
#   geom_bar(stat = "identity", fill = "#008000", size = 1) +
#   coord_flip() +
#   scale_y_continuous(breaks = seq(0, 100, by = 20), expand = c(0, 1.2)) +
#   labs(x = "Ecoregion", y = "Percent Ecoregion Protected") +
#   theme_soe() +
#   theme(axis.text.y = element_text(colour = ifelse(current_eco_t$is_bc, "royalblue3", "black")),
#         axis.line = element_blank(), panel.grid.major.y = element_blank()))

# plot(summary_eco_t_plot)

###############################################################################
## Make a facetted map of protection level by decade
carts_eco_t_by_decade <- cum_summary_t_eco %>%
  group_by(ecoregion_code, ecoregion, decade) %>%
  summarise(percent_protected = max(cum_percent_protected))

ecoregions_t_gg <- fortify(ecoregions_t_simp, region = "CRGNCD")
ecoregions_t_gg <- left_join(ecoregions_t_gg, carts_eco_t_by_decade, by = c("id" = "ecoregion_code"))

(decade_t_facet_map <- ggplot(ecoregions_t_gg, aes(x = long, y = lat, group = group, fill = percent_protected)) +
  facet_wrap(~decade) +
  geom_polygon(colour = "grey80") +
  scale_fill_continuous(low = "white", high = "#008000") +
  coord_equal() +
  theme_map())
# plot(decade_t_facet_map)

###############################################################################
## Map of current level of protection by ecoregion
(current_t_map <- ecoregions_t_gg %>%
  filter(decade == 2010) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = percent_protected)) +
  geom_polygon(colour = "grey80") +
  scale_fill_continuous(low = "white", high = "#008000") +
  coord_equal() +
  theme_map())
# plot(current_t_map)


# Marine ------------------------------------------------------------------


## Prep data frame for visualizations
cum_summary_m_eco <- order_df(cum_summary_m_eco, "ecoregion", "cum_percent_protected", max, na.rm = TRUE, desc = TRUE)
cum_summary_m_eco$decade <- floor(cum_summary_m_eco$prot_date / 10) * 10

###############################################################################
## Facet line-chart by ecoregion of cumulative percent protected over time

# Make a data frame of labels for current % protected
current_eco_m <- cum_summary_m_eco[cum_summary_m_eco$prot_date == max(cum_summary_m_eco$prot_date), ]

(ecoregion_m_facet_plot <- ggplot(cum_summary_m_eco,
                               aes(x = prot_date, y = cum_percent_protected)) +
  geom_path(colour = "#253494") +
  facet_wrap(~ecoregion, labeller = label_wrap_gen(width = 20), ncol = 6) +
  scale_x_continuous(expand = c(0,0), breaks = function(x) round(seq(min(x),max(x), length.out = 5))) +
  # scale_y_continuous(breaks = seq(0,100, length.out = 5)) +
  labs(x = "Year", y = "Cumulative Percent of Ecoregion Protected") +
  theme_minimal() +
  theme(panel.margin.x = unit(1.5, "lines"),
        axis.text = element_text(size = 8)) +
  geom_text(data = current_eco_m, x = 1980, y = 80,
            aes(label = paste(round(cum_percent_protected, 1), "%")),
            size = 3))
  # theme_soe_facet() +
  # theme(panel.margin = unit(1, "mm"))

# plot(ecoregion_m_facet_plot)

###############################################################################
## Bar chart of current protection by ecoregion
(summary_eco_m_plot <- ggplot(current_eco_m,
                             aes(x = ecoregion, y = cum_percent_protected, fill = cum_percent_protected)) +
  scale_fill_distiller(limits = c(0, max(current_eco_m$cum_percent_protected, na.rm = TRUE)),
                       palette = "YlGnBu", direction = 1, guide = "none") +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = sprintf("%.1f%s", cum_percent_protected, "%")),
            nudge_y = c(rep(-0.7, 5), 0.7), colour = c(rep("white", 5), "black"), size = 5) +
  #scale_y_continuous(breaks = seq(0, 100, by = 20), expand = c(0, 1.2)) +
  labs(x = "Marine Ecoregion", y = "Percent Protected") +
  theme_soe() +
  theme(axis.line = element_blank(), panel.grid.major.y = element_blank(),
        plot.margin = unit(c(2,0,2,0), "lines")))

# plot(summary_eco_m_plot)

###############################################################################
## Make a facetted map of protection level by decade
carts_eco_m_by_decade <- cum_summary_m_eco %>%
  group_by(ecoregion_code, ecoregion, decade) %>%
  summarise(percent_protected = max(cum_percent_protected))

ecoregions_m_gg <- fortify(ecoregions_m_simp, region = "CRGNCD")
ecoregions_m_gg_decade <- left_join(ecoregions_m_gg, carts_eco_m_by_decade, by = c("id" = "ecoregion_code"))

(decade_m_facet_map <- ggplot(ecoregions_m_gg_decade, aes(x = long, y = lat, group = group, fill = percent_protected)) +
  facet_wrap(~decade) +
  geom_polygon(data = ecoregions_m_gg_decade[!ecoregions_m_gg_decade$hole, ],
               aes(fill = percent_protected), colour = "grey70") +
  geom_polygon(data = ecoregions_m_gg_decade[ecoregions_m_gg_decade$hole, ], fill = "white",
               colour = "grey70") +
  scale_fill_distiller(limits = c(0, max(ecoregions_m_gg_decade$percent_protected, na.rm = TRUE)),
                       palette = "YlGnBu", direction = 1, na.value = brewer.pal(6, "YlGnBu")[1]) +
  coord_equal() +
  labs(fill = "Percent of Marine\nEcoregion Protected\n") +
  theme_map() +
  theme(legend.key = element_rect(colour = "grey70", size = 2), legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.position = c(0.7,0.1)))
# plot(decade_m_facet_map)

###############################################################################
## Map of current level of protection by ecoregion
eco_m_gg_current <- left_join(ecoregions_m_gg, current_eco_m, by = c("id" = "ecoregion_code"))

(current_m_map <- ggplot(eco_m_gg_current, aes(x = long, y = lat, group = group)) +
  geom_polygon(data = eco_m_gg_current[!eco_m_gg_current$hole, ],
               aes(fill = cum_percent_protected), colour = "grey70") +
  geom_polygon(data = eco_m_gg_current[eco_m_gg_current$hole, ], fill = "white",
               colour = "grey70") +
  scale_fill_distiller(limits = c(0, max(eco_m_gg_current$cum_percent_protected, na.rm = TRUE)),
                       palette = "YlGnBu", direction = 1, na.value = brewer.pal(6, "YlGnBu")[1]) +
  coord_equal() +
  labs(fill = "Percent of Marine\nEcoregion Protected\n") +
  theme_map() +
  theme(legend.key = element_rect(colour = "grey70", size = 2), legend.direction = "horizontal",
        legend.title = element_text(size = 12), legend.text = element_text(size = 11),
        legend.key.height = unit(1, "cm"), legend.key.width = unit(0.8, "cm"),
        plot.margin = unit(c(0,0,0,0), "lines")))
# plot(current_m_map)

endeavour <- coordinates(bc_carts[bc_carts$ZONE_ID == "700020100", ])

(annotated_m_map <- current_m_map +
  geom_point(aes(x = endeavour[1], y = endeavour[2]), inherit.aes = FALSE,
             colour = "#253494", size = 2) +
  annotate("text", x = endeavour[1], y = endeavour[2] - 40000, hjust = 0.2,
           label = "Endeavour Hydrothermal Vents\nMarine Protected Area"))

# BEC ---------------------------------------------------------------------

# aggregate prot_areas_bec by poly_id
# prot_areas_bec_agg <- raster::aggregate(prot_areas_bec, by = "poly_id",
#                                    sums = list(list(sum, "prot_area")))
#
# bec_t_prot_simp <- merge(bec_t_simp, prot_areas_bec_agg, by = "poly_id")
# bec_t_prot_simp$poly_id <- as.character(bec_t_prot_simp$poly_id)
# bec_t_prot_simp$percent_protected <- bec_t_prot_simp$prot_area / bec_t_prot_simp$area * 100
#
# bec_t_gg <- fortify(bec_t_prot_simp, region = "poly_id")
#
# bec_t_gg <- left_join(bec_t_gg, bec_t_prot_simp@data, by = c("id" = "poly_id"))
# bec_t_gg$percent_protected[is.na(bec_t_gg$percent_protected)] <- 0
#
# (bec_prot_map <- ggplot(bec_t_gg, aes(x = long, y = lat, group = group, fill = percent_protected)) +
#   geom_polygon() +
#   scale_fill_distiller(palette = "YlGn", direction = 1) +
#   guides(fill = guide_colourbar(title = "Percent of\nEcosystem\nProtected",
#                                 title.position = "left", label.position = "right")) +
#   coord_fixed() +
#   theme_map() +
#   theme(legend.title = element_text(size = 12, vjust = 1),
#         legend.text = element_text(size = 11), legend.position = c(0,0.03),
#         legend.background = element_rect(fill = NA),
#         legend.key.width = unit(1, "cm"), legend.key.height = unit(1, "cm"),
#         # panel.background = element_rect(fill = "grey90", colour = NA),
#         plot.margin = margin(0,0,0,0)))

#plot(bec_prot_map)

bec_zone_gg <- fortify(bec_zone_simp, region = "ZONE")

(bec_zone_map <- ggplot(bec_zone_gg, aes(x = long, y = lat, group = group, fill = id)) +
  geom_polygon() +
  scale_fill_manual(values = bgc_colours(), guide = "none") +
  coord_fixed() +
  ggtitle("Biogeoclimatic Zones of B.C.") +
  theme_map() +
  theme(plot.margin = margin(2,0,1,0, "lines"),
        plot.title = element_text(hjust = 0.3, size = 13,
                                  margin = margin(0,0,0,0, "pt"))))

# Temp fix for UTF8 chars in Zone Summary
prot_areas_bec_summary$ZONE_NAME <- gsub(" \x97 ", "--", prot_areas_bec_summary$ZONE_NAME)

zone_summary <- prot_areas_bec_summary  %>%
  order_df("ZONE_NAME", "percent_protected", fun = max)

(zone_barplot <- ggplot(zone_summary, aes(x = ZONE_NAME, y = percent_protected,
                                         fill = ZONE)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = bgc_colours(), guide = "none") +
  coord_flip() +
  # ggtitle("Percent of Biogeoclimatic Zones\nDesignated Within\nParks & Protected Areas") +
  labs(x = "Biogeoclimatic Zone\n", y = "Percent Protected") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0),
        panel.grid.major.x = element_line(colour = "grey85"),
        panel.grid.minor.x = element_line(colour = "grey90"),
        panel.grid.major.y = element_blank(),
        plot.margin = unit(c(2,0,1,0), "lines")))

#plot(zone_barplot)

## Plot protected areas


bc_bound_hres <- fix_geo_problems(bc_bound_hres)
bc_fortified <- fortify(bc_bound_hres, region = "PRUID")

(gg_bc <- ggplot(bc_fortified, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = NA, colour = "grey70", size = 0.5) +
  theme_map() +
  coord_fixed())

## Make a simplified spdf of terrestrial and marine protected areas
prot_areas_t <- rmapshaper::ms_dissolve(prot_areas_eco_t)
prot_areas_t$BIOME <- "Terrestrial"
prot_areas_m <- rmapshaper::ms_dissolve(prot_areas_eco_m)
prot_areas_m$BIOME <- "Marine"
prot_areas_map <- bind_spdf(prot_areas_t, prot_areas_m)
prot_areas_map <- fix_geo_problems(prot_areas_map)

gg_prot <- gg_fortify(prot_areas_map)

(prot_map <- gg_bc +
  geom_polygon(data = gg_prot, aes(x = long, y = lat, group = group, fill = BIOME)) +
  scale_fill_manual(name = "Biome",
                    values = c("Terrestrial" = "#006837", "Marine" = "#253494")) +
  coord_fixed() +
  theme_map() +
  theme(legend.title = element_text(size = 12, vjust = 1),
        legend.text = element_text(size = 11), legend.position = c(0.05,0.05),
        legend.background = element_rect(fill = NA),
        legend.key.width = unit(1, "cm"), legend.key.height = unit(1, "cm")))


# Output charts ----------------------------------------

png("out/prot_map.png", width = 600, height = 550, units = "px", type = "cairo-png")
plot(prot_map)
dev.off()

## Multiplot of marine map and bar chart
png(filename = "out/marine_chart.png", width = 900, height = 550, units = "px", type = "cairo-png")
multiplot(annotated_m_map, summary_eco_m_plot, cols = 2, widths = c(3,2))
dev.off()

## BGC plots
png("out/bgc_multiplot.png", width = 930, height = 430, units = "px")
multiplot(zone_barplot, bec_zone_map, cols = 2)
dev.off()

# png("out/bgc_finescale_map.png", width = 600, height = 550, units = "px") #, bg = bec_prot_map$theme$panel.background$fill)
# plot(bec_prot_map)
# dev.off()


## Output terrestrial ecoregions as geojson for the visualization:
ecoregions_t_out <- ecoregions_t_simp[, "CRGNCD"]
names(ecoregions_t_out) <- "ECOREGION_CODE"
file.remove("out/ecoregions.geojson")
spTransform(ecoregions_t_out, CRS("+init=epsg:4326")) %>%
  geojson_write(file = "out/ecoregions.geojson", precision = 5)

# Function to list objects of a certain class in the global environment
ls_class <- function(cls) {
  ls(.GlobalEnv)[sapply(ls(.GlobalEnv),function(x) cls %in% class(get(x)))]
}

save(list = ls_class("ggplot"), file = "tmp/out_gg.rda")
save(list = ls_class("SpatialPolygonsDataFrame"), file = "tmp/out_spatial.rda")
save(list = ls_class("data.frame"), file = "tmp/out_data.rda")

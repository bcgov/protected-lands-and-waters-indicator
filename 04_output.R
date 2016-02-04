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

library(dplyr)
library(ggplot2)
library(envreportutils) # for order_df and theme_soe
library(geojsonio)
library(rmapshaper)
library(readr)
library(sp)
library(ggthemes)
library(maptools)
library(RColorBrewer)

load("tmp/analyzed2016-02-03.rda")


# Terrestrial -------------------------------------------------------------

## Prep data frame for visualizations
cum_summary_t$ecoregion <- tools::toTitleCase(tolower(cum_summary_t$ecoregion))
cum_summary_t <- order_df(cum_summary_t, "ecoregion", "cum_percent_protected", max, na.rm = TRUE, desc = TRUE)
cum_summary_t$is_bc <- ifelse(cum_summary_t$ecoregion == "British Columbia", TRUE, FALSE)
cum_summary_t$decade <- floor(cum_summary_t$prot_date / 10) * 10

###############################################################################
## Facet line-chart by ecoregion of cumulative percent protected over time

# Make a data frame of labels for current % protected
current_eco_t <- cum_summary_t[cum_summary_t$prot_date == max(cum_summary_t$prot_date), ]

ecoregion_t_facet_plot <- ggplot(cum_summary_t,
                               aes(x = prot_date, y = cum_percent_protected)) +
  geom_path(colour = "forestgreen") +
  facet_wrap(~ecoregion, labeller = label_wrap_gen(width = 20), ncol = 6) +
  scale_x_continuous(expand = c(0,0), breaks = function(x) round(seq(min(x),max(x), length.out = 5))) +
  scale_y_continuous(breaks = seq(0,100, length.out = 5)) +
  labs(x = "Year", y = "Cumulative percent of ecoregion protected") +
  theme_minimal() +
  theme(panel.margin.x = unit(1.5, "lines"),
        axis.text = element_text(size = 8)) +
  geom_text(data = current_eco_t, x = 1980, y = 80,
            aes(label = paste(round(cum_percent_protected, 1), "%")),
            size = 3)
  # theme_soe_facet() +
  # theme(panel.margin = unit(1, "mm"))

plot(ecoregion_t_facet_plot)

###############################################################################
## Bar chart of current protection by ecoregion
summary_eco_t_plot <- ggplot(current_eco_t, aes(x = ecoregion, y = cum_percent_protected, colour = is_bc)) +
  scale_colour_manual(guide = "none", values = c(NA, "royalblue3")) +
  geom_bar(stat = "identity", fill = "#008000", size = 1) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 100, by = 20), expand = c(0, 1.2)) +
  labs(x = "Ecoregion", y = "Percent ecoregion protected") +
  theme_soe() +
  theme(axis.text.y = element_text(colour = ifelse(current_eco_t$is_bc, "royalblue3", "black")),
        axis.line = element_blank(), panel.grid.major.y = element_blank())

plot(summary_eco_t_plot)

###############################################################################
## Make a facetted map of protection level by decade
carts_eco_t_by_decade <- cum_summary_t %>%
  group_by(ecoregion_code, ecoregion, decade) %>%
  summarise(percent_protected = max(cum_percent_protected))

ecoregions_t_gg <- fortify(ecoregions_t_simp, region = "CRGNCD")
ecoregions_t_gg <- left_join(ecoregions_t_gg, carts_eco_t_by_decade, by = c("id" = "ecoregion_code"))

decade_t_facet_map <- ggplot(ecoregions_t_gg, aes(x = long, y = lat, group = group, fill = percent_protected)) +
  facet_wrap(~decade) +
  geom_polygon(colour = "grey80") +
  scale_fill_continuous(low = "white", high = "#008000") +
  coord_equal() +
  theme_map()
plot(decade_t_facet_map)

###############################################################################
## Map of urrent level of protection by ecoregion
current_t_map <- ecoregions_t_gg %>%
  filter(decade == 2010) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = percent_protected)) +
  geom_polygon(colour = "grey80") +
  scale_fill_continuous(low = "white", high = "#008000") +
  coord_equal() +
  theme_map()
plot(current_t_map)


# Marine ------------------------------------------------------------------


## Prep data frame for visualizations
cum_summary_m$ecoregion <- tools::toTitleCase(tolower(cum_summary_m$ecoregion))
cum_summary_m <- order_df(cum_summary_m, "ecoregion", "cum_percent_protected", max, na.rm = TRUE, desc = TRUE)
cum_summary_m$is_bc <- ifelse(cum_summary_m$ecoregion == "British Columbia", TRUE, FALSE)
cum_summary_m$decade <- floor(cum_summary_m$prot_date / 10) * 10

###############################################################################
## Facet line-chart by ecoregion of cumulative percent protected over time

# Make a data frame of labels for current % protected
current_eco_m <- cum_summary_m[cum_summary_m$prot_date == max(cum_summary_m$prot_date), ]

ecoregion_m_facet_plot <- ggplot(cum_summary_m,
                               aes(x = prot_date, y = cum_percent_protected)) +
  geom_path(colour = "forestgreen") +
  facet_wrap(~ecoregion, labeller = label_wrap_gen(width = 20), ncol = 6) +
  scale_x_continuous(expand = c(0,0), breaks = function(x) round(seq(min(x),max(x), length.out = 5))) +
  scale_y_continuous(breaks = seq(0,100, length.out = 5)) +
  labs(x = "Year", y = "Cumulative percent of ecoregion protected") +
  theme_minimal() +
  theme(panel.margin.x = unit(1.5, "lines"),
        axis.text = element_text(size = 8)) +
  geom_text(data = current_eco_m, x = 1980, y = 80,
            aes(label = paste(round(cum_percent_protected, 1), "%")),
            size = 3)
  # theme_soe_facet() +
  # theme(panel.margin = unit(1, "mm"))

plot(ecoregion_m_facet_plot)

###############################################################################
## Bar chart of current protection by ecoregion
summary_eco_m_plot <- ggplot(current_eco_m[!current_eco_m$is_bc, ],
                             aes(x = ecoregion, y = cum_percent_protected, fill = cum_percent_protected)) +
  scale_fill_distiller(limits = c(0, max(current_eco_m$cum_percent_protected, na.rm = TRUE)),
                       palette = "YlGnBu", direction = 1, guide = "none") +
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_y_continuous(breaks = seq(0, 100, by = 20), expand = c(0, 1.2)) +
  labs(x = "Marine Ecoregion", y = "Percent Protected") +
  theme_soe() +
  theme(axis.line = element_blank(), panel.grid.major.y = element_blank(),
        plot.margin = unit(c(2,0,2,0), "lines"))

plot(summary_eco_m_plot)

###############################################################################
## Make a facetted map of protection level by decade
carts_eco_m_by_decade <- cum_summary_m %>%
  group_by(ecoregion_code, ecoregion, decade) %>%
  summarise(percent_protected = max(cum_percent_protected))

ecoregions_m_gg <- fortify(ecoregions_m_simp, region = "CRGNCD")
ecoregions_m_gg_decade <- left_join(ecoregions_m_gg, carts_eco_m_by_decade, by = c("id" = "ecoregion_code"))

decade_m_facet_map <- ggplot(ecoregions_m_gg_decade, aes(x = long, y = lat, group = group, fill = percent_protected)) +
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
        legend.title = element_text(size = 11), legend.position = c(0.7,0.1))
plot(decade_m_facet_map)

###############################################################################
## Map of current level of protection by ecoregion
eco_m_gg_current <- left_join(ecoregions_m_gg, current_eco_m, by = c("id" = "ecoregion_code"))

current_m_map <- ggplot(eco_m_gg_current, aes(x = long, y = lat, group = group)) +
  geom_polygon(data = eco_m_gg_current[!eco_m_gg_current$hole, ],
               aes(fill = cum_percent_protected), colour = "grey70") +
  geom_polygon(data = eco_m_gg_current[eco_m_gg_current$hole, ], fill = "white",
               colour = "grey70") +
  scale_fill_distiller(limits = c(0, max(eco_m_gg_current$cum_percent_protected, na.rm = TRUE)),
                       palette = "YlGnBu", direction = 1, na.value = brewer.pal(6, "YlGnBu")[1]) +
  coord_equal() +
  labs(fill = "Percent of marine\necoregion protected\n") +
  theme_map() +
  theme(legend.key = element_rect(colour = "grey70", size = 2), legend.direction = "horizontal",
        legend.title = element_text(size = 12), legend.text = element_text(size = 11),
        legend.key.height = unit(1, "cm"), legend.key.width = unit(0.8, "cm"),
        plot.margin = unit(c(0,0,0,0), "lines"))
plot(current_m_map)

## Multiplot of marine map and bar chart
png(filename = "out/marine_chart.png", width = 900, height = 550, units = "px")
multiplot(current_m_map, summary_eco_m_plot, cols = 2, widths = c(3,2))
dev.off()

## Output csv files
cum_summary_t_viz <- cum_summary_t[cum_summary_t$tot_protected > 0, ]
write_csv(cum_summary_t_viz, path = "out/ecoregion_cons_lands_trends.csv")
write_csv(bc_designation_summary, path = "out/bc_carts_designation_summary.csv")
write_csv(bc_iucn_summary, path = "out/bc_carts_iucn_summary.csv")
write_csv(bc_designation_iucn_summary, path = "out/bc_carts_designation_iucn_summary.csv")

## Output terrestrial ecoregions as geojson for the visualization:
ecoregions_t_out <- ecoregions_t_simp[, "CRGNCD"]
names(ecoregions_t_out) <- "ECOREGION_CODE"
file.remove("out/ecoregions.geojson")
spTransform(ecoregions_t_out, CRS("+init=epsg:4326")) %>%
  geojson_write(file = "out/ecoregions.geojson", precision = 5)

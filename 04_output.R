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

load("tmp/analyzed.rda")

cum_summary_t$ecoregion <- tools::toTitleCase(tolower(cum_summary_t$ecoregion))
cum_summary_t <- order_df(cum_summary_t, "ecoregion", "cum_percent_protected", max, na.rm = TRUE, desc = TRUE)
cum_summary_t$is_bc <- ifelse(cum_summary_t$ecoregion == "British Columbia", TRUE, FALSE)

label_df <- cum_summary_t[cum_summary_t$prot_date == max(cum_summary_t$prot_date), ]

ecoregion_facet_plot <- ggplot(cum_summary_t,
                               aes(x = prot_date, y = cum_percent_protected)) +
  geom_path(colour = "forestgreen") +
  facet_wrap(~ecoregion, labeller = label_wrap_gen(width = 20), ncol = 6) +
  scale_x_continuous(expand = c(0,0), breaks = function(x) round(seq(min(x),max(x), length.out = 5))) +
  scale_y_continuous(breaks = seq(0,100, length.out = 5)) +
  labs(x = "Year", y = "Cumulative percent of ecoregion protected") +
  theme_minimal() +
  theme(panel.margin.x = unit(1.5, "lines"),
        axis.text = element_text(size = 8)) +
  geom_text(data = label_df, x = 1980, y = 80,
            aes(label = paste(round(cum_percent_protected, 1), "%")),
            size = 3)
  # theme_soe_facet() +
  # theme(panel.margin = unit(1, "mm"))

plot(ecoregion_facet_plot)

carts_eco_t_current <- cum_summary_t %>% group_by(ecoregion, is_bc) %>%
  summarize(total_ha_prot = round(max(cum_area_protected) / 1e4),
            percent_protected = round(max(cum_percent_protected), 1)) %>%
  arrange(ecoregion) %>%
  ungroup()

summary_eco_t_plot <- ggplot(carts_eco_t_current, aes(x = ecoregion, y = percent_protected, fill = is_bc)) +
  scale_fill_manual(guide = "none", values = c("forestgreen", "royalblue3")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_soe() +
  theme(axis.text.y = element_text(colour = ifelse(carts_eco_t_current$is_bc, "royalblue3", "black")))



## Too much variation in size for this to be useful
# ggplot(cum_summary_t, aes(x = prot_date, y = cum_area_protected)) +
#   geom_path() +
#   facet_wrap(~ecoregion)

cum_summary_t_viz <- cum_summary_t[cum_summary_t$tot_protected > 0, ]

write_csv(cum_summary_t_viz, path = "out/ecoregion_cons_lands_trends.csv")
write_csv(bc_designation_summary_t, path = "out/bc_carts_designation_summary.csv")
write_csv(bc_iucn_summary_t, path = "out/bc_carts_iucn_summary.csv")

## Output terrestrial ecoregions as geojson for the visualization:
ecoregions_t_out <- ecoregions_t[, "CRGNCD"]
names(ecoregions_t_out) <- "ECOREGION_CODE"
file.remove("out/ecoregions.geojson")
spTransform(ecoregions_t_out, CRS("+init=epsg:4326")) %>%
  ms_simplify() %>%
  geojson_write(file = "out/ecoregions.geojson", precision = 5)

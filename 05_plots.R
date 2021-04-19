# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

# Load packages etc.
source("00_setup.R")

bec <- st_read("out/bec_simp.geojson", crs = 3005)
bec_area <- read_rds("out/bec_area.rds")

bar1 <- ggplot(data = bec_area,
       aes(x = p_area, y = zone_name, fill = zone, alpha = park_type)) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(0.7, 0.3)) +
  geom_bar(width = 0.9, stat = "identity") +
  labs(x = "Percent Area Protected", y = "Biogeoclimatic Zone") +
  scale_fill_manual(values = bec_colours(), guide = FALSE) +
  scale_alpha_manual(name = "Type", values = c("OECM" = 0.5, "PA" = 1)) +
  scale_x_continuous(expand = c(0,0)) +
  guides(alpha = guide_legend(override.aes = list(fill = "black")))

bar2 <- ggplot(data = select(bec_area, zone_name, zone, p_zone) %>% distinct(),
       aes(x = p_zone, y = zone_name, fill = zone)) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank()) +
  geom_bar(width = 0.9, stat = "identity") +
  labs(x = "Percent Area Protected", y = "Biogeoclimatic Zone") +
  scale_fill_manual(values = bec_colours(), guide = FALSE) +
  scale_x_continuous(expand = c(0,0))

map <- ggplot() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.25, size = 25)) +
  geom_sf(data = bec, aes(fill = zone), colour = NA)+
  scale_fill_manual(values = bec_colours(), guide = FALSE) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Biogeoclimatic Zones of B.C.")

ggsave("out/bec_map.png", map, width = 11, height = 10, dpi = 300)
ggsave("out/bec_bar1.png", bar1, width = 6, height = 6, dpi = 300)
ggsave("out/bec_bar2.png", bar2, width = 6, height = 6, dpi = 300)

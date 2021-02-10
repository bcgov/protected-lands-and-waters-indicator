#' Copyright 2021 Province of British Columbia
#'
#' Licensed under the Apache License, Version 2.0 (the "License");
#' you may not use this file except in compliance with the License.
#' You may obtain a copy of the License at
#'
#' http://www.apache.org/licenses/LICENSE-2.0
#'
#' Unless required by applicable law or agreed to in writing, software
#' distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
#' WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
#' License for the specific language governing permissions and limitations under
#' the License.
#'
#' > Render this file with:
#' >
#' > rmarkdown::render(input = "99_checks.R", output_dir = "share")
#'
#' ## Preamble
#' This file checks that the `st_difference()` actually correctly removed overlapping
#' areas in `02_clean.R`. It also establishes that there are some overlaps, which
#' when removed leave little wiggly bits left over.
#'
#' These could be
#'
#' a) ignored
#' b) fixed by using something like st_snap before st_difference
#' c) removed manually
#'
#' ## Load packages
#+ message = FALSE
library(tidyverse)
library(sf)
library(patchwork)

#' ## Load protected areas
pa1 <- read_rds("data/CPCAD_Dec2019_BC_clean.rds")
pa2 <- read_rds("data/CPCAD_Dec2019_BC_clean_no_ovlps.rds") %>%
  mutate(area2 = as.numeric(st_area(Shape)))

#' pa1 = original clean, pa2 with overlaps removed

pa2_df <- st_set_geometry(pa2, NULL) %>%
  group_by(zone_id, name_e) %>%
  summarize(area1 = unique(area1), area2 = sum(area2))

#' Look for features where a lot of overlap was removed
filter(pa2_df, abs(area1 - area2) > 5000000)

#+ echo = FALSE
# Myra - Bellevue Park ------------------------------------------------------
#' ## Myra - Bellevue Park
bbx <- filter(pa2, str_detect(name_e, "Myra")) %>%
  st_buffer(dist = 1000) %>%
  st_bbox()

sub_pa1 <- st_crop(pa1, bbx)
sub_pa2 <- sub_pa1 %>%
  select(zone_id, name_e, oecm, iucn_cat, protdate) %>%
  arrange(oecm, iucn_cat, protdate) %>%
  st_difference()

sub_pa2

#+ fig.asp = 0.5
g1 <- ggplot(data = sub_pa1, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Orig")
g2 <- ggplot(data = sub_pa2, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Overlaps removed")
g1 + g2 + plot_layout(guides = "collect")

#+ fig.asp = 1.2
(g1 + facet_wrap(~name_e)) / (g2 + facet_wrap(~name_e)) +
  plot_layout(guides = "collect")

#' Good, no problems, Myra-Vellevue Protected Area includes river, but
#' Myra - Bellevue Park is the rest


#+ echo = FALSE
# Cape Scott Park -----------------------------------------------------------
#' ## Cape Scott Park

bbx <- filter(pa2, zone_id == 592000250) %>%
  st_buffer(dist = 1000) %>%
  st_bbox()

sub_pa1 <- st_crop(pa1, bbx)
sub_pa2 <- sub_pa1 %>%
  select(zone_id, name_e, oecm, iucn_cat, protdate) %>%
  arrange(oecm, iucn_cat, protdate) %>%
  st_difference()

#+ fig.asp = 0.4, fig.width = 8
g1 <- ggplot(data = sub_pa1, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Orig")
g2 <- ggplot(data = sub_pa2, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "After overlaps removed")
g1 + g2 + plot_layout(guides = "collect")

#+ fig.asp = 0.5, fig.width = 8
g1 + facet_wrap(~name_e) +
  g2 + facet_wrap(~name_e) +
  plot_layout(guides = "collect")

#' Check out wiggly bits left over
g1 <- ggplot(data = filter(sub_pa1, zone_id == 730017000), aes(fill = name_e)) +
  geom_sf(alpha = 0.5) +
  coord_sf(xlim = c(-2195800, -2193595), ylim = c(1718783, 1723805)) +
  facet_wrap(~name_e) +
  labs(title = "Orig")
g2 <- ggplot(data = filter(sub_pa2, zone_id == 730017000), aes(fill = name_e)) +
  geom_sf(alpha = 0.5) +
  coord_sf(xlim = c(-2195800, -2193595), ylim = c(1718783, 1723805)) +
  facet_wrap(~name_e)+
  labs(title = "After overlaps removed")

g1 + g2 + plot_layout(guides = "collect")

sub_pa2 %>%
  filter(zone_id == 730017000) %>%
  st_crop(c(xmin =-2195800, xmax = -2193595,
            ymin = 1718783, ymax = 1723805)) %>%
  st_area() %>%
  sum()

#' Left over bits add 2330 m^2 or 0.233 hectares to Scott islands Marine National Wildlife Area

filter(pa2, str_detect(name_e, "Scott Islands")) %>%
  pull(area2) %>%
  sum()

#' Minuscule error (if this is the total amount)


#+ echo = FALSE
# Columbia National Wildlife Area --------------------------------------------
#' ## Columbia National Wildlife Area
bbx <- filter(pa2, zone_id == 730003200) %>%
  st_buffer(dist = 1000) %>%
  st_bbox()

sub_pa1 <- st_crop(pa1, bbx)
sub_pa2 <- st_crop(pa2, bbx)

sub_pa2 %>% select(zone_id, name_e, oecm, iucn_cat, protdate)

g1 <- ggplot(data = sub_pa1, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Orig")
g2 <- ggplot(data = sub_pa2, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Overlaps removed")
g1 + g2 + plot_layout(guides = "collect")


bbx2 <- c(bbx[1], bbx[2] + 40000, bbx[3]-10000, bbx[4])
sub_pa1 <- st_crop(pa1, bbx2)
sub_pa2 <- st_crop(pa2, bbx2)

g1 <- ggplot(data = sub_pa1, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Orig")
g2 <- ggplot(data = sub_pa2, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Overlaps removed")

g11 <- ggplot(data = sub_pa1, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Orig") +
  facet_wrap(~name_e, nrow = 1)
g22 <- ggplot(data = sub_pa2, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Overlaps removed") +
  facet_wrap(~name_e, nrow = 1)

#+ fig.asp = 0.5, fig.width = 10
(g1 + g11) / (g2 + g22) + plot_layout(guides = "collect")


filter(sub_pa2, name_e == "Columbia National Wildlife Area") %>%
  pull(area2) %>%
  sum()

#' 83229.88 m^2 or 8.322 hectares left over...

filter(pa2, name_e == "Columbia National Wildlife Area") %>%
  pull(area2) %>%
  sum()

#' 4620729 m^2 or 462.0729 hectares remaining total
#'
#' Therefore 1.8% error in this case...


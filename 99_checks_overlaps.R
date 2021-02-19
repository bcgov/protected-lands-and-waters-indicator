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
#' Finally it shows that there is confusion regarding actual and official areas.
#' And that there seem to be overlapping, yet distinct management areas...
#'
#' ## Load packages
#+ message = FALSE
library(tidyverse)
library(sf)
library(patchwork)
library(kableExtra)

#' ## Load protected areas
pa2 <- read_rds("data/CPCAD_Dec2020_BC_clean_no_ovlps.rds") %>%
  mutate(area_single2 = as.numeric(st_area(Shape)))
pa1 <- read_rds("data/CPCAD_Dec2020_BC_clean.rds") %>%
  st_transform(crs = st_crs(pa2))
#' pa1 = original clean, pa2 with overlaps removed

pa2_df <- st_set_geometry(pa2, NULL) %>%
  group_by(zone_id, name_e, oecm) %>%
  summarize(o_area = unique(o_area),
            area_all = unique(area_all),
            area_all2 = sum(area_single2),
            area_diff = abs(area_all - area_all2))



#' Look for features where a lot of overlap was removed
filter(pa2_df, area_diff > 2000000) %>%
  arrange(desc(area_diff)) %>%
  kable() %>%
  kable_styling()

#+ echo = FALSE
# Hecate Strait and Queen Charlotte Sound   ---------------------------------
#' ## Hecate Strait and Queen Charlotte Sound
sub_pa1 <- filter(pa1, str_detect(name_e, "Hecate"))
sub_pa2 <- filter(pa2, str_detect(name_e, "Hecate"))

select(sub_pa2, parent_id, zone_id, name_e, oecm, iucn_cat, protdate) %>%
  st_set_geometry(NULL) %>%
  distinct() %>%
  kable() %>%
  kable_styling()

#+ fig.asp = 1
g1 <- ggplot(data = sub_pa1, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Orig")
g2 <- ggplot(data = sub_pa2, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Overlaps removed")
g1 + g2 + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#+ fig.asp = 0.5, fig.width = 12
(g1 + facet_wrap(~name_e)) / (g2 + facet_wrap(~name_e)) +
  plot_layout(guides = "collect")

#' Good, areas overlapping removed (something to report?)


#+ echo = FALSE
# Scott Islands -----------------------------------------------------------
#' ## Scott Islands

bbx <- filter(pa2, zone_id == 730017000) %>%
  st_buffer(dist = 1000) %>%
  st_bbox()

sub_pa1 <- st_crop(pa1, bbx)
sub_pa2 <- st_crop(pa2, bbx)

#+ fig.asp = 0.4, fig.width = 15
g1 <- ggplot(data = sub_pa1, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Orig")
g2 <- ggplot(data = sub_pa2, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "After overlaps removed")
g1 + g2 + plot_layout(guides = "collect")

#' Cape Scott Park takes priority because more important IUCN Category (II vs. VI)

select(sub_pa2, parent_id, zone_id, name_e, oecm, iucn_cat, protdate) %>%
  st_set_geometry(NULL) %>%
  filter(name_e %in% c("Cape Scott Park",
                       "Scott Islands Marine National Wildlife Area")) %>%
  distinct() %>%
  kable() %>%
  kable_styling()

#' Small islands parks take priority because Ia > VI
select(sub_pa2, parent_id, zone_id, name_e, oecm, iucn_cat, protdate) %>%
  st_set_geometry(NULL) %>%
  filter(name_e %in% c("Lanz And Cox Islands",
                       "Anne Vallee (Triangle Island) Ecological Reserve",
                       "Beresford Island Ecological Reserve",
                       "Scott Islands Marine National Wildlife Area")) %>%
  distinct() %>%
  kable() %>%
  kable_styling()


#+ fig.asp = 0.5, fig.width = 20
g1 + facet_wrap(~name_e) +
  g2 + facet_wrap(~name_e) +
  plot_layout(guides = "collect")

#' Check out wiggly bits left over
bbx <- c(xmin = 828472.4, xmax = 831002.2, ymin = 640000, ymax = 644300)
g1 <- ggplot(data = filter(sub_pa1, zone_id == 730017000), aes(fill = name_e)) +
  geom_sf(alpha = 0.5) +
  coord_sf(xlim = bbx[1:2], ylim = bbx[3:4]) +
  facet_wrap(~name_e) +
  labs(title = "Orig")
g2 <- ggplot(data = filter(sub_pa2, zone_id == 730017000), aes(fill = name_e)) +
  geom_sf(alpha = 0.5) +
  coord_sf(xlim = bbx[1:2], ylim = bbx[3:4]) +
  facet_wrap(~name_e)+
  labs(title = "After overlaps removed")

#+ fig.asp = 0.5, fig.width = 12
g1 + g2 + plot_layout(guides = "collect")

pa2_df %>%
  filter(zone_id == 730017000) %>%
  kable() %>%
  kable_styling()

#' Interestingly, the official area matches best the area BEFORE we removed
#' overlaps...


#+ echo = FALSE
# Mount Maxwell - Phase II --------------------------------------------
#' ## Mount Maxwell - Phase II
bbx <- filter(pa2, zone_id == 591104501) %>%
  st_buffer(dist = 1000) %>%
  st_bbox()

sub_pa1 <- st_crop(pa1, bbx)
sub_pa2 <- st_crop(pa2, bbx)

select(sub_pa2, zone_id, name_e, oecm, iucn_cat, protdate, delisdate) %>%
  kable() %>%
  kable_styling()

#+ fig.asp = 0.5, fig.width = 14
g1 <- ggplot(data = sub_pa1, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Orig")
g2 <- ggplot(data = sub_pa2, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Overlaps removed")
g1 + g2 + plot_layout(guides = "collect")

g11 <- ggplot(data = sub_pa1, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Orig") +
  facet_wrap(~name_e, nrow = 1)
g22 <- ggplot(data = sub_pa2, aes(fill = name_e)) +
  geom_sf(alpha = 0.5) + labs(title = "Overlaps removed") +
  facet_wrap(~name_e, nrow = 1)

#+ fig.asp = 0.5, fig.width = 14
g11 / g22


filter(sub_pa2, str_detect(name_e, "Maxwell")) %>%
  select(parent_id, name_e, zone_id, iucn_cat, oecm, o_area, type_e, protdate) %>%
  kable() %>%
  kable_styling()

#' A bit confusing, looks like phase II is privately owned, but overlaps extensively
#' with the Ecological Reserve which is public (BC Gov)...
#'
#'

#+ echo = FALSE
# Official area ---------------------------------------------------------------
#' # Official area
#' Compare new area to Official Area (o_area)
#'
#' - `o_area` is the Official reported area
#' - `area_all` is the calculated area *prior* to fixing overlaps
#' - `area_all2` is the calculated area *after* fixing overlaps
#' - `area_diff` is the absolute area change after fixing overlaps
#' - `o_diff1` is the difference between official area and area prior to fixing overlaps
#' - `o_diff2` is the difference between official area and area after fixing overlaps
#'
#' There are `r nrow(pa2_df)` distinct areas. Below are listed the ones wherein
#' the Official Area deviates from the calculated area after overlaps have been fixed
#' by more than 5 hectares.
#'
#' The remaining problems don't really seem to be related to overlaps...
#'
compare <- pa2_df %>%
  mutate(area_all = area_all / 10000,
         area_all2 = area_all2 / 10000,
         area_diff = area_diff / 10000,
         o_diff1 = o_area - area_all,
         o_diff2 = o_area - area_all2)

compare %>%
  group_by(oecm) %>%
  summarize(o_area = sum(o_area),
            area_all = sum(area_all),
            area_all2 = sum(area_all2))



compare %>%
  filter(abs(o_diff2) > 5) %>%
  DT::datatable() %>%
  DT::formatRound(columns = 3:5, digits = 0) %>%
  DT::formatRound(columns = 6:8, digits = 2)

#' Only in 3 cases did removing overlaps really improve the match between
#' official area and shape area...
#'
compare %>%
  filter(abs(o_diff1) > 5, abs(o_diff2) < 5)%>%
  DT::datatable() %>%
  DT::formatRound(columns = 3:5, digits = 0) %>%
  DT::formatRound(columns = 6:8, digits = 2)


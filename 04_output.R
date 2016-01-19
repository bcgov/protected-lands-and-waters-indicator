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

library(ggplot2)
library(envreportutils) # for order_df and theme_soe

load("tmp/analyzed.rda")

cum_summary$ecoregion <- tools::toTitleCase(tolower(cum_summary$ecoregion))
cum_summary <- order_df(cum_summary, "ecoregion", "cum_percent_protected", max, na.rm = TRUE, desc = TRUE)

label_df <- cum_summary[cum_summary$prot_date == max(cum_summary$prot_date), ]

ecoregion_facet_plot <- ggplot(cum_summary,
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

## To much variation in size for this to be useful
# ggplot(cum_summary, aes(x = prot_date, y = cum_area_protected)) +
#   geom_path() +
#   facet_wrap(~ecoregion)

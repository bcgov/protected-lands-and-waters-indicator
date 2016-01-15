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
cum_summary$ecoregion <- tools::toTitleCase(cum_summary$ecoregion)
cum_summary <- order_df(cum_summary, "ecoregion", "cum_percent_protected", max, na.rm = TRUE, desc = TRUE)

ggplot(cum_summary_plot, aes(x = prot_date, y = cum_percent_protected)) +
  geom_path() + geom_point() +
  facet_wrap(~ecoregion, labeller = label_wrap_gen())

## To much variation in size for this to be useful
# ggplot(cum_summary, aes(x = prot_date, y = cum_area_protected)) +
#   geom_path() +
#   facet_wrap(~ecoregion)

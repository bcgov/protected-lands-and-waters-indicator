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

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  titlePanel("Protected Lands and Waters Indicator"),

  fixedRow(
    align = "center",
    div(style = glue("width: {app_width}px"),
        column(width = 12, girafeOutput("top", height = glue("{top_height}px")))
    )),
  fixedRow(
    align = "center",
    div(style = glue("width: {app_width}px"),
        column(width = 12, girafeOutput("bottom", height = glue("{bottom_height}px")))
    ))
))


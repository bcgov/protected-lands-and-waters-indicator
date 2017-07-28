<!-- README.md is generated from README.Rmd. Please edit that file -->
<a rel="Delivery" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="In production, but maybe in Alpha or Beta. Intended to persist and be supported." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/delivery.svg" title="In production, but maybe in Alpha or Beta. Intended to persist and be supported." /></a>

------------------------------------------------------------------------

Analysis of trends and current amounts of protected lands and waters in B.C.
----------------------------------------------------------------------------

A set of R scripts to complete the analysis behind the Environmental Indicator [Protected Lands & Waters in B.C.](http://www.env.gov.bc.ca/soe/indicators/land/protected-lands-and-waters.html), published by [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B) in June 2016.

### Data

The analysis uses data from several sources; they should be downloaded and stored in the `data` folder:

-   [Canadian Council on Ecological Areas (CCEA) shapefile database of protected areas in Canada](http://www.ccea.org/download-carts-data/)
-   [B.C. Government Conservation Lands](https://catalogue.data.gov.bc.ca/dataset/68327529-c0d5-4fcb-b84e-f8d98a7f8612) (Licence: [Open Government Licence - British Columbia](http://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61))
-   [B.C. NGO Conservation Areas Database — Fee Simple Areas](http://ltabc.ca/resources/2012-02-05-22-20-02)
-   [Ecoregions - Ecoregion Ecosystem Classification of British Columbia](https://catalogue.data.gov.bc.ca/dataset/d00389e0-66da-4895-bd56-39a0dd64aa78) (Licence: [Open Government Licence - British Columbia](http://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61))
-   [Biogeoclimatic Ecosystem Classification (BEC) Map](https://catalogue.data.gov.bc.ca/dataset/f358a53b-ffde-4830-a325-a5a03ff672c3) (Licence: [Open Government Licence - British Columbia](http://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61))

### Code

There are four core scripts that are required for the analysis; they need to be run in order:

-   `01_load.R` - unzips the data files in the data folder
-   `02_clean.R` - cleans and prepares the spatial data for analysis, especially resolving overlaps among types of protected area designations
-   `03_analysis.R` - performs the spatial intersections and creates data summaries
-   `04_plot.R` - creates plots for communicating the results

The file `fun.R` contains a few custom functions required for the analysis.

#### Required R packages:

Spatial packages:

    sp
    rgdal
    rgeos
    raster
    maptools
    geojsonio
    bcmaps
    rmapshaper

General data manipulation packages:

    dplyr
    tidyr
    readr

Plotting packages:

    ggplot2
    RColorBrewer
    ggthemes
    envreportutils

Most packages used in the analysis can be installed from CRAN using `install.packages()`, but there are a few you will need to install using devtools:

``` r
install.packages("devtools") # If you don't already have it installed

library(devtools)
install_github("bcgov/envreportutils")
install_github("bcgov/bcmaps")
```

#### Other required software

You will also need the [mapshaper](https://github.com/mbloch/mapshaper) node package, and for this you will require Node.js to be installed. You can install it from [here](https://nodejs.org/en/), then on the command line type:

    npm install -g mapshaper

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/bc_population_indicator/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

    Copyright 2016 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

This repository is maintained by [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B). Click [here](https://github.com/bcgov/EnvReportBC-RepoList) for a complete list of our repositories on GitHub.

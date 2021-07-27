<!-- README.md is generated from README.Rmd. Please edit that file -->

[![img](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

# Analysis of trends and current amounts of protected lands and waters in B.C.

A set of R scripts to complete the analysis behind the Environmental
Indicator [Conserved Lands & Waters in
B.C.](http://www.env.gov.bc.ca/soe/indicators/land/protected-lands-and-waters.html),
published by [Environmental Reporting
BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B)
in 2021.

**Note**: Moving forward this analysis and indicator will be updated
using the analysis and approach found
[here](https://github.com/bcgov/land-designations-indicator).

### Data

The analysis uses data from several sources. The data does not need to
be pre-downloaded to run the code.

-   [Canadian Protected and Conserved Areas Database
    (CPCAD)](https://www.canada.ca/en/environment-climate-change/services/national-wildlife-areas/protected-conserved-areas-database.html)
-   [Ecoregions - Ecoregion Ecosystem Classification of British
    Columbia](https://catalogue.data.gov.bc.ca/dataset/d00389e0-66da-4895-bd56-39a0dd64aa78)
    (Licence: [Open Government Licence - British
    Columbia](http://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61))
-   [Biogeoclimatic Ecosystem Classification (BEC)
    Map](https://catalogue.data.gov.bc.ca/dataset/f358a53b-ffde-4830-a325-a5a03ff672c3)
    (Licence: [Open Government Licence - British
    Columbia](http://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61))

### Targets Workflow

This project leverages the `targets` package, a pipeline toolkit for
data science projects in R. You can install `targets` from CRAN:

``` r
#install.packages("targets")
```

\#Usage Run `targets::tar_make()` to run project. This will run all of
the analysis - no individual scripts are required.

### Required R packages

The packages used in this analysis are catalogued in `packages.R`. The
packages will be loaded automatically with `tar_make()` but some may
need to be installed prior to initiating the workflow.

## Conserved Areas Shiny app

One component of the conserved areas indicator is a shiny app that
details the percentage of conserved area by ecoregion. The shiny app can
be opened separately after the targets workflow has been run completely
via the following code:

``` r
#install.packages("shiny")
#library("shiny")
#shiny::runApp('app')
```

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/protected-lands-and-waters-indicator/issues).

## How to Contribute

If you would like to contribute, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## License

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

This repository is maintained by [Environmental Reporting
BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B).
Click [here](https://github.com/bcgov/EnvReportBC) for a complete list
of our repositories on GitHub.

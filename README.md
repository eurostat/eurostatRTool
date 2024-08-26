
# eurostatRTool

The eurostatRTool is a Eurostat R package which provides a flexible
visualisation and interpretation environment for economic indicators and
their short-term analysis.

eurostatRTool allows users to create customisable dashboards, as html
files, for displaying user-defined indicators in multiple ways (time
series plots, vertical bar plots for comparisons across groups, trend
and cycle plots, horizontal bar plots, maps and tables) both for data
inspection and interpretation.

The users can easily customise the general layout, update the data used
for building the indicators in the dashboard, as well as specify which
displaying modes and interpretative texts are shown.

## Installation

Please note that the package has been only tested in R version 4.3 thus
if you are using later versions of R we cannot ensure its right
functioning.

### For Windows users: install Rtools

For installing R packages from source on Windows, users also need to
install Rtools, which provides the necessary tools for building R
packages.

1.  Download: go to the [Rtools download
    page](https://cran.r-project.org/bin/windows/Rtools/) and download
    the appropriate version of Rtools.

2.  Install: run the downloaded installer and follow the installation
    instructions. Make sure to select the option to modify the system
    PATH during installation.

### Install devtools package in R

Open R or RStudio and install the `devtools` package from CRAN:

``` r
install.packages("devtools")
```

### Install R package from source

Now, you can use `devtools::install_github()` or `devtools::install()`
to install R packages directly from GitHub or other sources.
Specifically, the eurostatRTool package can be installed from GitHub by
running:

``` r
devtools::install_github("eurostat/eurostatRTool")
```

This should directly install the package from Github, without any other
steps. 

The following packages are required: `configr`,  `dplyr`,  `DT`, `flexdashboard`, `htmltools`, `leaflet`, `lubridate`, `magick`,
`magrittr`, `mapview`,  `metathis`, `plotly`, `rmarkdown`, `shiny`, `shinydashboard`, `sp`, `tidyr`, `xlsx`.

Setting up an renv environment for this project is recommended but not
mandatory. The setup of a [renv environment](https://rstudio.github.io/renv) provide a robust and reliable way to
manage dependencies, ensuring that the R project you will setup to use
this R package is isolated. By creating a specific renv environment for
this R package, you will be able to set its dependencies in the same
version used during the building and testing of the R package, to avoid
errors due to package updating. The environment will be isolate from
your other projects, allowing you to change the version of the
dependencies in other projects without affecting the project you setup
for using this R package and viceversa.

## Getting started

The eurostatRTool package requires some learning to be
used properly. Below we provide a basic example which shows you how to generate a
general dashboard in a given output folder:

``` r
library(eurostatRTool)
eurostatRTool::generate_dashboard("path/to/output/folder")
```

<img src="man/figures/README-dashboard-example.png" width="100%" style="display: block; margin: auto;" />

## Further help and documentation

For general help with the eurostatRTool, you should look into the [Wiki page for this project](https://github.com/eurostat/eurostatRTool/wiki) 
or the package’s documentation, which is available by checking the individual
function documentation (`?function_name`) and also via the vignettes. 

If you have any further questions, please contact us at [ESTAT-EUROINDICATORS@ec.europa.eu](mailto:ESTAT-EUROINDICATORS@ec.europa.eu).

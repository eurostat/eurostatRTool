
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
steps. You may be asked to update packages. This might not be strictly
necessary, so you can try skipping this step. The following packages are required (with versions used to develop eurostatRTool):
``` r
install.packages("configr", version= "0.3.5")
install.packages("dplyr", version= "1.1.4")
install.packages("DT", version= "0.32")
install.packages("flexdashboard", version= "0.6.2")
install.packages("htmltools", version= "0.5.8")
install.packages("leaflet", version= "2.2.1")
install.packages("lubridate", version= "1.9.3")
install.packages("magick", version= "2.8.3")
install.packages("magrittr", version= "2.0.3")
install.packages("mapview", version= "2.11.2")
install.packages("metathis", version= "1.1.4")
install.packages("plotly", version= "4.10.3")
install.packages("rmarkdown", version= "2.25")
install.packages("shiny", version= "1.8.0")
install.packages("shinydashboard", version= "0.7.2")
install.packages("sp", version= "2.1-3")
install.packages("tidyr", version= "1.3.0")
install.packages("xlsx", version= "0.6.5")
```
The setup of a renv environment provide a robust and reliable way to
manage dependencies, ensuring that the R project you will setup to use
this R package is isolated. By creating a specific renv environment for
this R package, you will be able to set its dependencies in the same
version used during the building and testing of the R package, to avoid
errors due to package updating. The environment will be isolate from
your other projects, allowing you to change the version of the
dependencies in other projects without affecting the project you setup
for using this R package and viceversa.

Setting up an renv environment for this project is recommended but not
mandatory. You can learn how to do it reading the corresponding vignette
`Setup-a-renv-environment-for-managing-the-package-dependencies`.
## Getting started

The eurostatRTool package requires a little reading and learning to be
used properly. But once you have understood the way it works, you will
find out that it is a very easy and flexible tool for visualising and
sharing your data and their interpretation.

Below we provide a basic example which shows you how to generate a
general dashboard in a given output folder:

``` r
library(eurostatRTool)
eurostatRTool::generate_dashboard("path/to/output/folder")
```

<img src="man/figures/README-dashboard-example.png" width="100%" style="display: block; margin: auto;" />

## Further help and documentation

For general help with the eurostatRTool you should look into the
package’s documentation, which is available by checking the individual
function documentation (`?function_name`) and also via the vignettes.

Following the guidelines contained in the vignettes you will learn how
to build and maintain a customised dashboard for your organisation.

These are the available vignettes for the eurostatRTool package:

- Demonstration dashboards showing the functionalities of the tool
  (`Demonstration-dashboards-showing-the-functionalities-of-the-tool`)
- General workflow for building and maintaining a customised dashboard
  for your organisation
  (`General-workflow-for-building-and-maintaining-your-dashboard`)
- Setup an renv environment for managing the package dependencies
  (`Setup-a-renv-environment-for-managing-the-package-dependencies`)

We recommend you to start from the
`Demonstration-dashboards-showing-the-functionalities-of-the-tool`
vignette, which allows you to generate two general sample dashboards,
and then follow with the
`General-workflow-for-building-and-maintaining-your-dashboard` vignette,
where you will find guidance to build and maintain your own customised
dashboard.

By running the code below, you will be redirected to a page in your
browser displaying the list of vignettes for the eurostatRtool package.
From the list, you can choose the vignette you would like to read. We
recommend using the `HTML` version of the vignettes, which is the more
readable format.

``` r
utils::browseVignettes("eurostatRTool")
```
If you have any further questions, please contact us at [ESTAT-EUROINDICATORS@ec.europa.eu](mailto:ESTAT-EUROINDICATORS@ec.europa.eu).

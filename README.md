
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Load dependencies

Load and install all necessary packages

``` r
proj_name <- "hockey"
pkgs <- c(
  "devtools",
  "tidyverse",
  "RMariaDB",
  "DBI",
  "readr",
  "pander",
  "na.tools",
  "ggimage",
  "devtools",
  "teamcolors",
  "glue",
  "dplyr",
  "RCurl",
  "tictoc",
  "animation",
  "gt",
  "DT",
  "ggthemes",
  "bbplot",
  "ggtext",
  "ggforce",
  "ggridges",
  "ggrepel",
  "ggbeeswarm",
  "extrafont",
  "RCurl",
  "xml2",
  "rvest",
  "jsonlite",
  "foreach",
  "lubridate",
  "snakecase",
  "nhlapi"
)
installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
if (any("bbplot" %in%
  rownames(installed.packages()) == FALSE)) {
  library(devtools)
  devtools::install_github('bbc/bbplot')
}
invisible(lapply(pkgs, library, character.only = TRUE))
#> Loading required package: usethis
#> ── Attaching packages ───────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
#> ✓ tibble  3.0.3     ✓ dplyr   1.0.0
#> ✓ tidyr   1.1.0     ✓ stringr 1.4.0
#> ✓ readr   1.3.1     ✓ forcats 0.5.0
#> ── Conflicts ──────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
#> 
#> Attaching package: 'glue'
#> The following object is masked from 'package:dplyr':
#> 
#>     collapse
#> 
#> Attaching package: 'RCurl'
#> The following object is masked from 'package:tidyr':
#> 
#>     complete
#> Registering fonts with R
#> 
#> Attaching package: 'rvest'
#> The following object is masked from 'package:gt':
#> 
#>     html
#> The following object is masked from 'package:purrr':
#> 
#>     pluck
#> The following object is masked from 'package:readr':
#> 
#>     guess_encoding
#> 
#> Attaching package: 'jsonlite'
#> The following object is masked from 'package:purrr':
#> 
#>     flatten
#> 
#> Attaching package: 'foreach'
#> The following objects are masked from 'package:purrr':
#> 
#>     accumulate, when
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
rm(pkgs, installed_packages)
```

## Initialize Working Directory

Set up correct file paths

``` r
source("../initR/init.R")
fx.setdir(proj_name)
#> [1] "Goose (iMac) is ready for some hockey"
```

## Create standard objects

Variables to connect to

``` r
f.con <- "../initR/con.R"

# source(f.con)
# dbListTables(con)
# dbDisconnect(con)

userYear <- substr(Sys.Date(), 1, 4)
userDate <- Sys.Date()

today <- format(Sys.Date(), "%Y-%d-%m")
source("EH_scrape_functions.R")
```

How many games should we scrape at once?

``` r
u.scrape_interval <- 250
```

## Create standard objects

``` r
# source("playground/addToTable.R")
```

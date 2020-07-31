# Packages & Init Setup ---------------------------------------------------
proj_name <- "hockey"

# devtools::install_github('bbc/bbplot')

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
invisible(lapply(pkgs, library, character.only = TRUE))
rm(pkgs, installed_packages)

# Initialize Working Directory --------------------------------------------

source("../initR/init.R")
fx.setdir(proj_name)

# Create standard objects -------------------------------------------------
f.con <- "../initR/con.R"

source(f.con)
dbListTables(con)
dbDisconnect(con)

userYear <- substr(Sys.Date(), 1, 4)
userDate <- Sys.Date()

today <- format(Sys.Date(), "%Y-%d-%m")
source("EH_scrape_functions.R")
u.scrape_interval <- 250
source("playground/addToTable.R")

 ## Packages ##

####################

pkgs <- c("devtools", "tidyverse", "RMariaDB", "DBI", "readr",
          "pander", "na.tools", "ggimage",
          "devtools", "teamcolors", "glue",
          "animate", "dplyr", "tictoc",
          "animation", "gt", "DT",
          "ggthemes", "bbplot", "ggtext",
          "ggforce", "ggridges", "ggrepel",
          "ggbeeswarm", "extrafont", "RCurl",
          "xml2", "rvest", "jsonlite",
          "foreach", "lubridate", "snakecase")

##install.packages(c("devtools", "tidyverse", "readr", "pander", "na.tools", "ggimage", "devtools", "teamcolors", "glue", "animate", "dplyr", "tictoc", "animation"))
##download https://downloads.mariadb.org/connector-c/

installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}

invisible(lapply(pkgs, library, character.only = TRUE))

####################

## Initialize Working Directory

####################

##reset
setwd("~/")
gid <- paste(getwd())
gid
device <- ""

if (gid == "/Volumes/HDD/Users/colinwelsh") {
  ##Maverick - MBP
  setwd("~/Documents/dev/hockey")
  device <- "Maverick (MBP)"
} else if (gid == "/Users/ColinWelsh") {
  ##Goose - iMac
  setwd("~/Documents/dev/hockey")
  device <- "Goose (iMac)"
  ##add Goose
} else if (gid == "/home/rstudio-user") {
  setwd("/cloud/project")
  device <- "RStudio Cloud"
}
print(paste(device, "is ready for some hockey", sep = " "))
rm(gid, device)
####################

## Create Items

userYear <- 2019
userDate <- Sys.Date()
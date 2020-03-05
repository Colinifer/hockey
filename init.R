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

library(devtools)
library(tidyverse)
library(RMariaDB)
library(DBI)
library(readr)
library(pander)
library(dplyr)
library(na.tools)
library(ggimage)
library(teamcolors) # NFL team colors and logos
library(plyr)
library(readr)
library(glue)
##library(animate)
library(animation)
library(tictoc)
library(gt) # beautiful tables
library(DT) # beautiful interactive tables
library(ggthemes) # custom pre-built themes
library(bbplot) # more themes
library(ggtext) # custom text color
library(ggforce) # better annotations
library(ggridges) # many distributions at once
library(ggrepel) # better labels
library(ggbeeswarm) # beeswarm plots
library(extrafont) # for extra fonts
library(RCurl)
library(xml2)
library(rvest)
library(jsonlite)
library(foreach)
library(lubridate)
library(snakecase)



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
} 
print(paste(device, "is ready for some hockey", sep = " "))
rm(gid, device)


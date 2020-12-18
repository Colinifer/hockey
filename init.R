# Packages & Init Setup ---------------------------------------------------
proj_name <- 'hockey'
# setwd('~/Documents/dev/hockey')

# devtools::install_github('bbc/bbplot')
# devtools::install_github('war-on-ice/nhlplot')

pkgs <- c(
  'devtools',
  'tidyverse',
  'RMariaDB',
  'DBI',
  'readr',
  'pander',
  'na.tools',
  'devtools',
  'teamcolors',
  'glue',
  'dplyr',
  'arrow',
  'RCurl',
  'tictoc',
  'animation',
  'gt',
  'DT',
  'ggimage',
  'ggpubr',
  'ggthemes',
  'bbplot',
  'ggtext',
  'ggforce',
  'ggridges',
  'ggrepel',
  'ggbeeswarm',
  'extrafont',
  'RCurl',
  'xml2',
  'rvest',
  'jsonlite',
  'foreach',
  'lubridate',
  'snakecase',
  'nhlapi',
  'nhlplot',
  'initR'
)
installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
if (any('bbplot' %in%
        rownames(installed.packages()) == FALSE)) {
  library(devtools)
  devtools::install_github('bbc/bbplot')
}
invisible(lapply(pkgs, library, character.only = TRUE))
rm(pkgs, installed_packages)

# Initialize Working Directory --------------------------------------------

fx.setdir(proj_name)

# Create standard objects -------------------------------------------------
if ((
  Sys.Date() %>% lubridate::wday() > 1 & # If day is greater than Sunday
  Sys.Date() %>% lubridate::wday() < 6 & # and day is less than Saturday
  Sys.time() %>% format("%H") %>% as.integer() >= 17 & # and greater than 5PM
  Sys.time() %>% format("%H") %>% as.integer() <= 23 # and less than 12AM
) == TRUE) {
  source("../initR/con.R")
  dbListTables(con)
  dbDisconnect(con)
}

year <- substr(Sys.Date(), 1, 4)
date <- Sys.Date()

today <- format(Sys.Date(), '%Y-%d-%m')
source('EH_scrape_functions.R')
source('functions/add_to_table.R')

f.scrape <- paste0('data/', list.files(path = 'data/', pattern = 'pbp_scrape'))
# f.scrape[3] %>% lapply(fx.add_to_table)

# If scrape isn't caught up
# u.scrape_interval <- 250
# source('playground/addToTable.R')


# http://www.nhl.com/scores/htmlreports/20192020/RO030113.HTM # Roster
# http://www.nhl.com/scores/htmlreports/20192020/PL030113.HTM # Play by Play
# http://www.nhl.com/scores/htmlreports/20192020/GS020113.HTM # Game Summary
# http://www.nhl.com/scores/htmlreports/20192020/TV030113.HTM # Visitor TOI Shift Report
# http://www.nhl.com/scores/htmlreports/20192020/TH030113.HTM # Home TOI Shift Report
# http://www.nhl.com/scores/htmlreports/20192020/SS030113.HTM # Shot Summary
# http://www.nhl.com/scores/htmlreports/20192020/ES020967.HTM # Event Summary

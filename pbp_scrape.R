library(RCurl); library(xml2); library(rvest); library(jsonlite); library(foreach)
library(lubridate)
library(tidyverse)

source("EH_scrape_functions.R")
fpbp_scrape <- paste("data/pbp/", userYear, "pbp.csv", sep = "")

## Scrape games
pbp_scrape <- sc.scrape_pbp(games = as.character(seq(2019020001, 2019020100, by = 1)))

pbp_scrape %>% write.csv(file = fpbp_scrape)


####################

 ## Combine CSV ##

####################

playerSeason <- list.files(paste("data/players/", userYear, "/", sep = ""),
                           pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
playerSeason
write.csv(playerSeason, file = paste("data/season_total/", userYear, "players.csv", sep = ""), row.names=FALSE)

####################
library(RCurl); library(xml2); library(rvest); library(jsonlite); library(foreach)
library(lubridate)
library(tidyverse)

source("EH_scrape_functions.R")
fpbp_scrape <- paste("data/pbp/", userYear, "pbp.csv", sep = "")

## Scrape games
pbp_scrape <- sc.scrape_pbp(games = as.character(seq(2019020001, 2019020100, by = 1)))

pbp_scrape %>% write.csv(file = fpbp_scrape)
library(RCurl); library(xml2); library(rvest); library(jsonlite); library(foreach)
library(lubridate)
library(tidyverse)

source("EH_scrape_functions.R")

## Scrape games
pbp_scrape <- sc.scrape_pbp(games = as.character(seq(2019020001, 2019021032, by = 1)))


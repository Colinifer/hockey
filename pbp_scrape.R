library(RCurl); library(xml2); library(rvest); library(jsonlite); library(foreach)
library(lubridate)
library(tidyverse)

source("EH_scrape_functions.R")
game_ids <- as.character(seq(2019020001, 2019020100, by = 1))
x <- 1

## Scrape games
pbp_scrape <- sc.scrape_pbp(games = game_ids[x])




##### wrap into function 
game_info_df_new <-     pbp_scrape$game_info_df       ## game information data
pbp_base_new <-         pbp_scrape$pbp_base           ## main play-by-play data
pbp_extras_new <-       pbp_scrape$pbp_extras         ## extra play-by-play data
player_shifts_new <-    pbp_scrape$player_shifts      ## full player shifts data
player_periods_new <-   pbp_scrape$player_periods     ## player TOI sums per period
roster_df_new <-        pbp_scrape$roster_df          ## roster data
scratches_df_new <-     pbp_scrape$scratches_df       ## scratches data
event_summary_df_new <- pbp_scrape$events_summary_df  ## event summary data
scrape_report <-        pbp_scrape$report             ## scrape report



####################

 ## Combine CSV ##

####################

game_ids <- as.character(seq(2019020001, 2019020100, by = 1))
x <- 1

sc.scrape_pbp(games = game_ids[x])

pbp_season <- list.files(paste("data/pbp/", userYear, "/", sep = ""),
                         pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
playerSeason
write.csv(playerSeason, file = paste("data/pbp/", userYear, "pbp.csv", sep = ""), row.names=FALSE)

####################
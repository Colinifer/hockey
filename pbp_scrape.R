library(RCurl); library(xml2); library(rvest); library(jsonlite); library(foreach)
library(lubridate)
library(tidyverse)

source("EH_scrape_functions.R")
game_ids <- as.character(seq(2019020001, 2019020100, by = 1))
x <- 1

schedule <- sc.scrape_schedule(start_date = paste(userYear, "-10-01", sep = ""), end_date = paste(userYear + 1, "-07-01", sep = ""), print_sched = TRUE)

write.csv(schedule, file = fschedule, row.names = FALSE)
schedule <- read.csv(file = fschedule)
tibble(schedule)

today <- schedule %>% 
  filter(game_date == paste(Sys.Date()))
tibble(today)

yesterday <- schedule %>% 
  filter(game_date == paste(Sys.Date()-1))
tibble(yesterday)


game_ids <- as.character(seq(2019020001, yesterday$game_id[nrow(yesterday)], by = 1))

## Scrape games

x = 201902100

for (x in game_ids) {
  print(x)
  pbp_scrape <- sc.scrape_pbp(games = x)
  
  fgame_info_df <- paste("data/", userYear, "game_info_df", ".csv", sep = "")
  fpbp_base <- paste("data/", userYear, "pbp_base", ".csv", sep = "")
  fpbp_extras <- paste("data/", userYear, "pbp_extras", ".csv", sep = "")
  fplayer_shifts <- paste("data/", userYear, "player_shifts", ".csv", sep = "")
  fplayer_periods <- paste("data/", userYear, "player_periods", ".csv", sep = "")
  froster_df <- paste("data/", userYear, "roster_df", ".csv", sep = "")
  fscratches_df <- paste("data/", userYear, "scratches_df", ".csv", sep = "")
  fevents_summary_df <- paste("data/", userYear, "events_summary_df", ".csv", sep = "")
  freport <- paste("data/", userYear, "report", ".csv", sep = "")
  
  game_info_df <-           read.csv(fgame_info_df, row.names = FALSE)
  pbp_base <-               read.csv(fpbp_base, row.names = FALSE)
  pbp_extras <-             read.csv(fpbp_extras, row.names = FALSE)
  player_shifts <-          read.csv(fplayer_shifts, row.names = FALSE)
  player_periods <-         read.csv(fplayer_periods, row.names = FALSE)
  roster_df <-              read.csv(froster_df, row.names = FALSE)
  scratches_df <-           read.csv(fscratches_df, row.names = FALSE)
  events_summary_df <-      read.csv(fevents_summary_df, row.names = FALSE)
  report <-                 read.csv(freport, row.names = FALSE)
  
  game_info_df_new <-       pbp_scrape$game_info_df       ## game information data
  pbp_base_new <-           pbp_scrape$pbp_base           ## main play-by-play data
  pbp_extras_new <-         pbp_scrape$pbp_extras         ## extra play-by-play data
  player_shifts_new <-      pbp_scrape$player_shifts      ## full player shifts data
  player_periods_new <-     pbp_scrape$player_periods     ## player TOI sums per period
  roster_df_new <-          pbp_scrape$roster_df          ## roster data
  scratches_df_new <-       pbp_scrape$scratches_df       ## scratches data
  events_summary_df_new <-  pbp_scrape$events_summary_df  ## event summary data
  report_new <-             pbp_scrape$report             ## scrape report
  
  game_info_df <- rbind(game_info_df, game_info_df_new) %>% 
    unique()
  pbp_base <- rbind(pbp_base, pbp_base_new) %>% 
    unique()
  pbp_extras <- rbind(pbp_extras, pbp_extras_new) %>% 
    unique()
  player_shifts <- rbind(player_shifts, player_shifts_new) %>% 
    unique()
  player_periods <- rbind(player_periods, player_periods_new) %>% 
    unique()
  roster_df <- rbind(roster_df, roster_df_new) %>% 
    unique()
  scratches_df <- rbind(scratches_df, roster_df_new) %>% 
    unique()
  events_summary_df <- rbind(events_summary_df, events_summary_df_new) %>% 
    unique()
  report <- rbind(report, scrape_report_new) %>% 
    unique()
  
  write.csv(game_info_df, fgame_info_df, row.names = FALSE)
  write.csv(pbp_base, fpbp_base, row.names = FALSE)
  write.csv(pbp_extras, fpbp_extras, row.names = FALSE)
  write.csv(player_shifts, fplayer_shifts, row.names = FALSE)
  write.csv(player_periods, fplayer_periods, row.names = FALSE)
  write.csv(roster_df, froster_df, row.names = FALSE)
  write.csv(scratches_df, fscratches_df, row.names = FALSE)
  write.csv(events_summary_df_new, fevents_summary_df, row.names = FALSE)
  write.csv(report, freport, row.names = FALSE)
}


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
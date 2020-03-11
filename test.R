###############
## Packages
###############

pkgs <- c("RCurl", "xml2", "rvest", 
          "jsonlite", "foreach", "lubridate",
          "tidyverse")
installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
invisible(lapply(pkgs, library, character.only = TRUE))

source("EH_scrape_functions.R")

###############
## Variables
###############
userYear <- 2019
fschedule <- paste("schedule/", userYear, "schedule.csv", sep = "")
schedule <- sc.scrape_schedule(start_date = paste(userYear, "-10-01", sep = ""), 
                               end_date = paste(userYear + 1, "-07-01", sep = ""), 
                               print_sched = TRUE)


write.csv(schedule, file = fschedule, row.names = FALSE)
schedule <- read.csv(file = fschedule)
tibble(schedule)

today <- schedule %>% 
  filter(game_date == paste(Sys.Date()))
tibble(today)

yesterday <- schedule %>% 
  filter(game_date == paste(Sys.Date()-1))
tibble(yesterday)

## list of games
yesterday_game_ids <- as.character(seq(yesterday$game_id[1], yesterday$game_id[nrow(yesterday)], by = 1))
today_game_ids <- schedule %>% filter(EST_date == Sys.Date() & EST_time_convert < format(Sys.time(), "%F %X"))
today_game_ids <- today_game_ids$game_id

dead_games <- as.character(c(2019020442, 2019020665, 2019020876, 2019020919))

scrape_items <- c("game_info_df_new", "pbp_base_new", "pbp_extras_new", 
                  "player_shifts_new", "player_periods_new", "roster_df_new",
                  "scratches_df_new", "events_summary_df_new", "report_new")

###############
## Functions
###############

scrape_games_today <- function(x) {
  pbp_scrape_today <- sc.scrape_pbp(games = x)
  
  game_info_df_new <-       pbp_scrape_today$game_info_df       ## game information data
  pbp_base_new <-           pbp_scrape_today$pbp_base           ## main play-by-play data
  pbp_extras_new <-         pbp_scrape_today$pbp_extras         ## extra play-by-play data
  player_shifts_new <-      pbp_scrape_today$player_shifts      ## full player shifts data
  player_periods_new <-     pbp_scrape_today$player_periods     ## player TOI sums per period
  roster_df_new <-          pbp_scrape_today$roster_df          ## roster data
  scratches_df_new <-       pbp_scrape_today$scratches_df       ## scratches data
  events_summary_df_new <-  pbp_scrape_today$events_summary_df  ## event summary data
  report_new <-             pbp_scrape_today$report             ## scrape report
  
  game_info_df_new <- game_info_df_new %>% 
    mutate(game_id = as.numeric(game_id), season = as.numeric(season))
  pbp_base_new <- pbp_base_new %>% 
    mutate(game_id = as.numeric(game_id), 
           season = as.numeric(season), 
           home_on_7 = as.logical(home_on_7), 
           away_on_7 = as.logical(away_on_7))
  pbp_extras_new <- pbp_extras_new %>% 
    mutate(game_id = as.numeric(game_id))
  player_shifts_new <- player_shifts_new %>% 
    mutate(game_id = as.numeric(game_id), season = as.numeric(season))
  player_periods_new <- player_periods_new %>% 
    mutate(game_id = as.numeric(game_id), season = as.numeric(season))
  roster_df_new <- roster_df_new %>% 
    mutate(game_id = as.numeric(game_id), season = as.numeric(season))
  scratches_df_new <- scratches_df_new %>% 
    mutate(game_id = as.numeric(game_id), season = as.numeric(season))
  events_summary_df_new <- events_summary_df_new %>% 
    mutate(game_id = as.numeric(game_id), season = as.numeric(season))
  report_new <- report_new %>% 
    mutate(game_id = as.numeric(game_id))
}

save_scrapes <- function() {
  fgame_info_df <- paste("data/", userYear, "game_info_df", ".csv", sep = "")
  fpbp_base <- paste("data/", userYear, "pbp_base", ".csv", sep = "")
  fpbp_extras <- paste("data/", userYear, "pbp_extras", ".csv", sep = "")
  fplayer_shifts <- paste("data/", userYear, "player_shifts", ".csv", sep = "")
  fplayer_periods <- paste("data/", userYear, "player_periods", ".csv", sep = "")
  froster_df <- paste("data/", userYear, "roster_df", ".csv", sep = "")
  fscratches_df <- paste("data/", userYear, "scratches_df", ".csv", sep = "")
  fevents_summary_df <- paste("data/", userYear, "events_summary_df", ".csv", sep = "")
  freport <- paste("data/", userYear, "report", ".csv", sep = "")
  
  game_info_df <-           read.csv(fgame_info_df)
  pbp_base <-               read.csv(fpbp_base)
  pbp_extras <-             read.csv(fpbp_extras)
  player_shifts <-          read.csv(fplayer_shifts)
  player_periods <-         read.csv(fplayer_periods)
  roster_df <-              read.csv(froster_df)
  scratches_df <-           read.csv(fscratches_df)
  events_summary_df <-      read.csv(fevents_summary_df)
  report <-                 read.csv(freport)
  
  ##  game_info_df <-       pbp_scrape$game_info_df       ## game information data
  ##  pbp_base <-           pbp_scrape$pbp_base           ## main play-by-play data
  ##  pbp_extras <-         pbp_scrape$pbp_extras         ## extra play-by-play data
  ##  player_shifts <-      pbp_scrape$player_shifts      ## full player shifts data
  ##  player_periods <-     pbp_scrape$player_periods     ## player TOI sums per period
  ##  roster_df <-          pbp_scrape$roster_df          ## roster data
  ##  scratches_df <-       pbp_scrape$scratches_df       ## scratches data
  ##  events_summary_df <-  pbp_scrape$events_summary_df  ## event summary data
  ##  report <-             pbp_scrape$report             ## scrape report
  
  game_info_df <- dplyr::union(game_info_df, game_info_df_new)
  pbp_base <- dplyr::union(pbp_base, pbp_base_new)
  pbp_extras <- dplyr::union(pbp_extras, pbp_extras_new)
  player_shifts <- dplyr::union(player_shifts, player_shifts_new)
  player_periods <- dplyr::union(player_periods, player_periods_new)
  roster_df <- dplyr::union(roster_df, roster_df_new)
  scratches_df <- dplyr::union(scratches_df, scratches_df_new)
  events_summary_df <- dplyr::union(events_summary_df, events_summary_df_new)
  report <- dplyr::union(report, report_new)
  
  write.csv(game_info_df, fgame_info_df, row.names = FALSE)
  write.csv(pbp_base, fpbp_base, row.names = FALSE)
  write.csv(pbp_extras, fpbp_extras, row.names = FALSE)
  write.csv(player_shifts, fplayer_shifts, row.names = FALSE)
  write.csv(player_periods, fplayer_periods, row.names = FALSE)
  write.csv(roster_df, froster_df, row.names = FALSE)
  write.csv(scratches_df, fscratches_df, row.names = FALSE)
  write.csv(events_summary_df, fevents_summary_df, row.names = FALSE)
  write.csv(report, freport, row.names = FALSE)
}

###############
## Today's games
###############

##scrape_games_today(today_game_ids)
##pbp_scrape_today
##View(pbp_base_new)
##save_scrapes(today_game_ids)


###############
## First batch
###############

scrape.1 <- as.character(seq(2019020238, 2019020300))
scrape.1 <- scrape.1[!scrape.1%in%dead_games]
scrape_games_today(scrape.1)
scrape.1
View(pbp_base_new)
save_scrapes()

###############
## Second batch
###############

scrape.2 <- as.character(seq(2019020301, 2019020500))

scrape_games_today(scrape.2)
scrape.2
save_scrapes()

###############
## Third batch
###############

scrape.3 <- as.character(seq(2019020501, 2019020750))

scrape_games_today(scrape.3)
scrape.3
save_scrapes()

###############
## Fourth batch
###############

scrape.4 <- as.character(seq(2019020751, 2019021073))

scrape_games_today(scrape.4)
scrape.4
save_scrapes()
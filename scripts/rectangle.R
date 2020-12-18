library(tidyverse)
library(data.table)
library(httr)
library(repurrrsive)
library(listviewer)
nhl_api <- "https://statsapi.web.nhl.com/api/v1"

game <- "game"

id <- 2019030062

boxscore <- "boxscore"

game_boxscore <- glue::glue(nhl_api, "/", game, "/", id, "/", boxscore) %>% fromJSON(flatten = T)

response_json <- content(GET(glue::glue(nhl_api, "/", game, "/", id, "/", boxscore)), as = "parsed", type = "application/json")
str(game_boxscore)

df.game_boxscore_home <- game_boxscore[[2]]$home %>% 
  map_if(is.data.frame, list) %>% 
  as_tibble()

df.game_boxscore_away <- game_boxscore[[2]]$away %>% 
  map_if(is.data.frame, list) %>% 
  as_tibble() %>%
  unnest(players)

game_boxscore <- nhl_games_boxscore(id)


sc.scrape_rosters_API <- function(games_data, cores) {
  
  ## Register cores
  registerDoMC(cores = cores)
  
  ## Scrape player info by game
  players <- foreach(i = 1:nrow(games_data), .combine = bind_rows) %dopar% { 
    
    ## Scrape specific game
    game_box <- jsonlite::fromJSON(
      paste0(
        "https://statsapi.web.nhl.com/api/v1/game/", 
        2019030063, 
        "/boxscore"
      )
    )
    
    
    ## Comine home and away players in specific game
    player_info <- bind_rows(
      ## Away skaters
      foreach(j = 1:length(game_box$teams$away$players), .combine = bind_rows) %dopar% { 
        as.data.frame(game_box$teams$away$players[[j]]$person)
      } %>% 
        mutate(
          team = games_data$away_team[i], 
          opponent = games_data$home_team[i], 
          is_home = 0
        ) %>% 
        arrange(fullName), 
      ## Home skaters
      foreach(j = 1:length(game_box$teams$home$players), .combine = bind_rows) %dopar% { 
        as.data.frame(game_box$teams$home$players[[j]]$person)
      } %>% 
        mutate(
          team = games_data$home_team[i], 
          opponent = games_data$away_team[i], 
          is_home = 1
        ) %>% 
        arrange(fullName)
    ) %>% 
      ## Add game information
      mutate(
        game_id = games_data$game_id[i], 
        game_date = games_data$game_date[i], 
        season = as.character(games_data$season[i]), 
        session = as.character(games_data$session[i])
      )
    
  } %>% 
    ## Modify returned data
    mutate(
      id = as.character(id), 
      birthDate = as.Date(birthDate), 
      currentAge = round(as.numeric((as.Date(paste0(substr(season, 1, 4), "-9-15"), "%Y-%m-%d") - birthDate) / 365.25), 2), 
      position = case_when(
        primaryPosition.type == "Forward" ~ "F", 
        primaryPosition.type == "Defenseman" ~ "D", 
        primaryPosition.type == "Goalie" ~ "G"
      ),
      position_type = case_when(
        primaryPosition.abbreviation == "LW" ~ "L", 
        primaryPosition.abbreviation == "C" ~ "C", 
        primaryPosition.abbreviation == "RW" ~ "R", 
        primaryPosition.abbreviation == "D" ~ "D", 
        primaryPosition.abbreviation == "G" ~ "G"
      )
    ) %>% 
    select(
      api_id = id, fullName, position, position_type, birthDate, game_id, game_date, season, session, team, opponent, is_home, 
      everything()
    ) %>% 
    select(-c(primaryPosition.code, primaryPosition.name, primaryPosition.type, primaryPosition.abbreviation)) %>% 
    rename(birthday = birthDate) %>% 
    data.frame()
  
  ## Return data frame
  return(players)
  
}
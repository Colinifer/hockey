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

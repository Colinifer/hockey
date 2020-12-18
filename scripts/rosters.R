roster_data_API <- bind_rows(
  
  ## Home players
  foreach(i = 1:length(game_boxscore$teams$home$players), .combine = bind_rows) %do% {
    
    data.frame(
      player_ID = na_if_null(game_boxscore$teams$home$players[[i]]$person$id), 
      fullName =  na_if_null(game_boxscore$teams$home$players[[i]]$person$fullName), 
      Team =      game_info_data$home_team, 
      number =    na_if_null(game_boxscore$teams$home$players[[i]]$jerseyNumber), 
      stringsAsFactors = FALSE
    )
    
  } %>% 
    data.frame(row.names = NULL), 
  
  ## Away players
  foreach(i = 1:length(game_boxscore$teams$away$players), .combine = bind_rows) %do% {
    
    data.frame(
      player_ID = na_if_null(game_boxscore$teams$away$players[[i]]$person$id), 
      fullName =  na_if_null(game_boxscore$teams$away$players[[i]]$person$fullName), 
      Team =      game_info_data$away_team, 
      number =    na_if_null(game_boxscore$teams$away$players[[i]]$jerseyNumber), 
      stringsAsFactors = FALSE
    )
    
  } %>% 
    data.frame(row.names = NULL)
) %>% 
  mutate(player_team_num = paste0(Team, number))


nhl_api <- "https://statsapi.web.nhl.com/api/v1"

game <- "game"

id <- 2019030062

boxscore <- "boxscore"

game_boxscore <- glue::glue(nhl_api, "/", game, "/", id, "/", boxscore) %>% fromJSON(flatten = T)

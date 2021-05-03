# CSV to RDS
csv_to_rds <- function(x){
  x %>% 
    read_csv() %>% 
    saveRDS(glue('{gsub(".csv", "", x)}.rds'))
}


# Scrape Moneypuck
fx.scrape_moneypuck <- function(x) {
  
  schedule_df <- get_nhl_schedule(x) %>% 
    mutate(game_id = game_id %>% as.integer())
    invisible()
  
  season_full <- schedule_df %>% 
    pull(season) %>% 
    first()
  
  # Game/PBP data
  print('Scraping mp games')
  games_existing_ids <- mp_games_ds %>% 
    filter(season == season_full) %>% 
    pull(game_id) %>% 
    unique()
  
  scrape_ids <- schedule_df %>% 
    filter(!(game_id %in% games_existing_ids) & 
             game_status == 'Final') %>% 
    pull(game_id)
  
  map(scrape_ids, function(x.gameid) {
    print(glue('{x.gameid}'))
    x.year <- x
    mp_season_id <- glue('{x.year}{x.year+1}')
    
    mp_base <- 'http://moneypuck.com/moneypuck/gameData'
    mp_csv <- read_csv(url(glue('{mp_base}/{mp_season_id}/{x.gameid}.csv'))) %>% 
      mutate(game_id = game_id %>% as.integer(), 
             season = mp_season_id) %>% 
      select(game_id, 
             season,
             everything())
    
    mp_csv %>% 
      saveRDS(glue('data/moneypuck_games/{mp_season_id}/{x.gameid}.rds'))
    
    mp_csv %>% 
      bind_rows(
        mp_games_ds %>% 
          filter(season == mp_season_id) %>% 
          collect()
      ) %>% 
      write_parquet(glue('data/moneypuck/games/{mp_season_id}/mp_games_{mp_season_id}.parquet'))
    
    gc()
  })
  
  # Player data
  print('Scraping mp players')
  players_existing_ids <- mp_players_ds %>% 
    filter(season == season_full) %>% 
    pull(game_id) %>% 
    unique()
  
  scrape_ids <- schedule_df %>% 
    filter(!(game_id %in% players_existing_ids) & 
             game_status == 'Final') %>% 
    pull(game_id)
  
  map(scrape_ids, function(x.gameid) {
    print(glue('{x.gameid}'))
    x.year <- x
    mp_season_id <- glue('{x.year}{x.year+1}')
    
    mp_base <- 'http://moneypuck.com/moneypuck/playerData/games'
    mp_csv <- read_csv(url(glue('{mp_base}/{mp_season_id}/{x.gameid}.csv'))) %>% 
      janitor::clean_names() %>%
      mutate(game_id = game_id %>% as.integer(),
             season = mp_season_id
             ) %>% 
      select(game_id, 
             season,
             everything())
    
    mp_csv %>% 
      saveRDS(glue('data/moneypuck_players/{mp_season_id}/{x.gameid}.rds'))
    
    mp_csv %>% 
      bind_rows(
        mp_players_ds %>% 
          filter(season == mp_season_id) %>% 
          collect()
      ) %>% 
      write_parquet(glue('data/moneypuck/players/{mp_season_id}/mp_players_{mp_season_id}.parquet'))
    
    gc()
  })
  
  runif(1, 
        min=2, 
        max=3
        ) %>% 
    Sys.sleep()
}


# Scrape Natural Stat Trick
fx.scrape_nst <- function(x) {
  
  schedule_df <- get_nhl_schedule(x) %>% 
    mutate(game_id = game_id %>% as.integer())
  invisible()
  
  season_full <- schedule_df %>% 
    pull(season) %>% 
    first()
  
  # Game/PBP data
  print('Scraping mp games')
  existing_ids <- nst_ds %>% 
    filter(season == season_full) %>% 
    pull(game_id) %>% 
    unique()
  
  scrape_ids <- schedule_df %>% 
    filter(!(game_id %in% existing_ids) & 
             game_status == 'Final') %>% 
    pull(game_id)
  
  map(scrape_ids, function(x.gameid) {
    # x.gameid <- 2020020543
    con <- initR::fx.db_con()
    print(x.gameid)
    nst_season_id <- glue('{x.gameid %>% substr(1,4) %>% as.integer()}{x.gameid %>% substr(1,4) %>% as.integer()+1}')
    nst_game_id <- substr(x.gameid,5,10) %>% as.integer()
    nst_base <- glue('https://naturalstattrick.com/')
    nst_url <- glue('game.php?season={nst_season_id}&game={nst_game_id}')
    
    # Get NST game page
    print(glue('Downloading {x.gameid} from NST'))
    nst_page <- read_html(glue('{nst_base}{nst_url}'))
    
    x.nst_away_team <- tbl(con, 'schedule') %>%
      filter(game_id == x.gameid) %>% 
      collect() %>% 
      mutate(nst_away_team = case_when(away_team == 'T.B' ~ 'TB',
                                       away_team == 'N.J' ~ 'NJ',
                                       away_team == 'S.J' ~ 'SJ',
                                       away_team == 'L.A' ~ 'LA',
                                       TRUE ~ away_team)) %>%
      pull(nst_away_team)
    
    x.nst_home_team <- schedule_df %>%
      filter(game_id == x.gameid) %>%
      mutate(nst_home_team = case_when(home_team == 'T.B' ~ 'TB',
                                       home_team == 'N.J' ~ 'NJ',
                                       home_team == 'S.J' ~ 'SJ',
                                       home_team == 'L.A' ~ 'LA',
                                       TRUE ~ home_team)) %>%
      pull(nst_home_team)
    
    print(glue('Download Complete!\n{x.gameid}: {x.nst_away_team} vs {x.nst_home_team}'))
    
    # Create table ids from NST game page
    x.nst_table_ids <- sapply(
      c(x.nst_home_team,
        x.nst_away_team),
      function(x){
        paste0(
          '#tb',
          x,
          c(
            'stall',
            'stgall',
            'stev',
            'st5v5',
            'stpk'
          )
        )
      }
    )
    
    # Show all tables on page
    # nst_tables <- nst_page %>% html_elements("table")
    # nst_page %>% 
    #   html_element("#tbtsall") %>% 
    #   html_table()
    
    # Scrape and bind all tables
    print(glue('Combining tables for {x.gameid}'))
    nst_scrape <- map_df(
      x.nst_table_ids %>%
        # x.nst_table_ids[c(1,6)] %>%
        # nth(1) %>%
        identity(),
      function(x) {
        # print(glue('{x}'))
        nst_scrape <- nst_page %>% 
          html_element(x) %>% 
          html_table(fill = NA, header = T) %>% 
          janitor::clean_names() %>%
          as_tibble() %>% 
          mutate(
            position = ifelse('position' %in% colnames(.), position %>% as.character(), NA),
            # goals = ifelse('goals' %in% colnames(.), goals %>% as.integer(), NA),
            # shots = ifelse('shots' %in% colnames(.), shots %>% as.integer(), NA),
            sh_percent = ifelse('sh_percent' %in% colnames(.), sh_percent %>% as.character(), NA),
            faceoffs_percent = ifelse('faceoffs_percent' %in% colnames(.), faceoffs_percent  %>% as.character(), NA),
            avg_goal_distance = ifelse('avg_goal_distance' %in% colnames(.), avg_goal_distance %>% as.double(), NA),
            avg_shot_distance = ifelse('avg_shot_distance' %in% colnames(.), avg_shot_distance %>% as.double(), NA),
            sv_percent = ifelse('sv_percent' %in% colnames(.), sv_percent %>% as.double(), NA),
            sv_percent = ifelse(sv_percent == '-', NA, sv_percent),
            hdsv_percent = ifelse('hdsv_percent' %in% colnames(.), hdsv_percent %>% as.double(), NA),
            hdsv_percent = ifelse(hdsv_percent == '-', NA, hdsv_percent),
            mdsv_percent = ifelse('mdsv_percent' %in% colnames(.), mdsv_percent %>% as.double(), NA),
            mdsv_percent = ifelse(mdsv_percent == '-', NA, mdsv_percent),
            ldsv_percent = ifelse('ldsv_percent' %in% colnames(.), ldsv_percent %>% as.double(), NA),
            ldsv_percent = ifelse(ldsv_percent == '-', NA, ldsv_percent),
            game_id = x.gameid,
            table = x,
            team = ifelse(grepl(x.nst_home_team, x) == T, x.nst_home_team, x.nst_away_team),
            team = case_when(team == 'TB' ~ 'T.B',
                             team == 'NJ' ~ 'N.J',
                             team == 'SJ' ~ 'S.J',
                             team == 'LA' ~ 'L.A',
                             TRUE ~ team),
            full_name = player %>% toupper(),
            # sub searches left-to-right instead of the whole string
            # let's remove the first space/tab-space from string to 
            player = sub('\\s', '.', player, ) %>% toupper(),
            NULL
          )
      }
    ) %>% 
      as_tibble() %>% 
      mutate(
        sh_percent = ifelse('sh_percent' %in% colnames(.) | sh_percent != '-', sh_percent %>% as.double(), NA),
        faceoffs_percent = ifelse(!is.na(faceoffs_won), faceoffs_won / (faceoffs_won + faceoffs_lost), NA),
        NULL
      ) %>% 
      select(
        -position,
        NULL
      ) %>% 
      left_join(
        roster_df %>% 
          select(
            player,
            team,
            player_id,
            position,
            position_type,
            NULL
          ),
        by = c('player', 'team')
      ) %>% 
      select(
        game_id,
        table,
        team,
        full_name,
        player,
        player_id,
        position,
        position_type,
        everything(),
        NULL
      ) %>% 
      # select(player) %>%
      identity()
    
    # Save game tables to RDS
    print(glue('Saving {x.gameid} to RDS'))
    nst_scrape %>% 
      saveRDS(glue('data/nst_games/{nst_season_id}/{x.gameid}.rds'))
    
    nst_scrape %>% 
      bind_rows(
        nst_ds %>% 
          collect()
      ) %>% 
      write_parquet(glue('data/nst/{nst_season_id}/nst_{nst_season_id}.parquet'))
    
    # Stall scrape timer
    sleep_time <- runif(1, min=2, max=3)
    print(glue('Sleeping {sleep_time}s before next request'))
    
    sleep_time %>% 
      Sys.sleep()
  })
 #
}


# moneypuck_pbp_df <- map_df(dir(
#   path = glue('data/moneypuck/{current_season}{current_season+1}/'),
#   full.names = T
#   ),
#   read_csv)
# 
# moneypuck_pbp_df <- map_df(dir(
#   path = glue('data/moneypuck/{current_season}{current_season+1}/'),
#   full.names = T
# ),
# csv_to_rds)

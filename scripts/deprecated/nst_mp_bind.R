# MP Games ----------------------------------------------------------------

mp_games_data <- do.call(bind_rows,
                    lapply(list.files(
                      path = glue('data/moneypuck_games/{current_full_season}'),
                      pattern = '.rds',
                      full.names = F
                    ),
                    function(x){
                      df <- readRDS(glue('data/moneypuck_games/{current_full_season}/{x}')) %>% 
                        janitor::clean_names() %>% 
                        # mutate(game_id = gsub("^.*\\.","", x))
                        mutate(season = current_full_season,
                               game_id = gsub("\\..*","", x) %>% as.integer()) %>% 
                        select(
                          season,
                          game_id,
                          everything(),
                          NULL
                        )
                      return(df)
                    }))

# mp_games_data %>%
#   write_parquet(glue('data/moneypuck/games/{current_full_season}/mp_games_{current_full_season}.parquet'))

con <- fx.db_con()
mp_games_data %>% 
  DBI::dbWriteTable(fx.db_con(), 'moneypuck_games', ., overwrite = TRUE, row.names = FALSE)
dbDisconnect(con)

# MP Players --------------------------------------------------------------

mp_players_data <- do.call(bind_rows,
                          lapply(list.files(
                            path = glue('data/moneypuck_players/{current_full_season}'),
                            pattern = '.rds',
                            full.names = F
                          ),
                          function(x){
                            df <- readRDS(glue('data/moneypuck_players/{current_full_season}/{x}')) %>% 
                              janitor::clean_names() %>% 
                              # mutate(game_id = gsub("^.*\\.","", x))
                              mutate(season = current_full_season,
                                     game_id = gsub("\\..*","", x) %>% as.integer()) %>% 
                              select(
                                season,
                                game_id,
                                everything(),
                                NULL
                              ) %>% 
                            return(df)
                          }))

# mp_players_data %>%
#   write_parquet(glue('data/moneypuck/players/{current_full_season}/mp_players_{current_full_season}.parquet'))

con <- fx.db_con()
mp_players_data %>% 
  DBI::dbWriteTable(con, 'moneypuck_players', ., overwrite = TRUE, row.names = FALSE)
dbDisconnect(con)

# NST ---------------------------------------------------------------------

nst_data <- do.call(bind_rows,
lapply(list.files(
  path = glue('data/nst_games/{current_full_season}'),
  pattern = '.rds',
  full.names = F
),
function(x){
  df <- readRDS(glue('data/nst_games/{current_full_season}/{x}')) %>% 
    # mutate(game_id = gsub("^.*\\.","", x))
    mutate(season = current_full_season,
           game_id = gsub("\\..*","", x) %>% as.integer()) %>% 
    select(game_id,
           season, 
           everything()
           )
  # saveRDS(glue('data/nst_games/{current_full_season}/cleaned/{x}'))
  return(df)
  }
)
)

# nst_data %>%
#   write_parquet(glue('data/nst/{current_full_season}/nst_{current_full_season}.parquet'))

con <- fx.db_con()
nst_data %>%
  DBI::dbWriteTable(con, 'nst', ., overwrite = TRUE, row.names = FALSE)
dbDisconnect(con)

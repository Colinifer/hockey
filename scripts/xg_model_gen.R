library(xgboost)

# X = 100max -100min
# Y = 42max -42min

con <- fx.db_con()
pbp <- tbl(con, 'pbp_base') %>% 
  filter(season %in% c('20202021')) %>% 
  collect()
dbDisconnect(con)

# Prep pbp mutations for xG model
pbp_mutate <- pbp %>% 
  group_by(game_id) %>% 
  arrange(season, game_id, event_index) %>% 
  mutate(
    prev_event = dplyr::lag(event_type),
    seconds_since_last_event = game_seconds - dplyr::lag(game_seconds)
    ) %>% 
  arrange(season, game_id, event_type, event_index) %>% 
  mutate(
    # seconds_since_last_shot = ifelse(
    #   event_type == 'SHOT', 
    #   game_seconds - dplyr::lag(game_seconds, n = 1, default = NA), 
    #   NA
    # ),
    seconds_since_last_shot = case_when(
      event_type %in% st.corsi_events ~ game_seconds - dplyr::lag(game_seconds, default = NA)
    )
  ) %>% 
  arrange(season, game_id, event_index) %>% head(40) %>% view()
  mutate(
    # https://thewincolumn.ca/2021/01/15/r-tutorial-creating-an-nhl-rink-using-the-tidyverse/
    event_distance = ifelse(event_type %in% st.corsi_events,
                            sqrt((((89) - abs(coords_x)
                            ) ^ 2) + ((
                              abs(coords_y)
                            ) ^ 2)),
                            NA),
    is_rebound = ifelse(
      prev_event %in% st.corsi_events &
        seconds_since_last_shot <= 3,
      TRUE,
      FALSE
    ),
    shot_type = ifelse(
      event_type %in% st.corsi_events,
      gsub(".*\\, (.*)\\,.*", "\\1", event_description) %>% toupper(),
      NA
    ),
    # rebound_angle_from_last_shot = ifelse(
    #   is_rebound == TRUE,
    #   ,
    #   NA
    #   ),
    shot_angle = sin(abs(coords_y) / event_distance),
    penalty_taken_player = ifelse(event_type == 'PENL', event_player_1, NA),
    penalty_drawn_player = ifelse(event_type == 'PENL', event_player_2, NA),
    # faceoff_win_player = ifelse(event_type == 'FAC', event_player_1, NA),
    # faceoff_lose_player = ifelse(event_type == 'FAC', event_player_2, NA),
    NULL
  ) %>% 
  # select(event_type, is_rebound, event_distance, event_description) %>%
  # filter(event_type %in% st.corsi_events) %>% 
  identity()

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
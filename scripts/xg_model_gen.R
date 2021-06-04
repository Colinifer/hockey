library(xgboost)

# X = 100max -100min
# Y = 

pbp_base_ds %>% 
  filter(season %in% c('20202021')) %>% 
  collect() %>% 
  as_tibble() %>% 
  mutate(prev_event = lag(event_type),
         seconds_since_last_event = game_seconds - lag(game_seconds),
         # https://thewincolumn.ca/2021/01/15/r-tutorial-creating-an-nhl-rink-using-the-tidyverse/
         event_distance = ifelse(
           event_type %in% st.corsi_events, 
           sqrt((((89) - abs(coords_x))^2)+((abs(coords_y))^2)),
           NA),
         is_rebound = ifelse(
           prev_event %in% st.corsi_events &
             seconds_since_last_event <= 3,
           TRUE,
           FALSE
         ),
         shot_type = ifelse(
           event_type %in% st.corsi_events,
           # TKTK,
           # TKTK
         )
         ) %>% 
  select(event_type, is_rebound, event_distance, event_description) %>%
  identity() %>% 
  filter(event_type %in% st.corsi_events)

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
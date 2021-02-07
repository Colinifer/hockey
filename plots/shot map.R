source(url('https://github.com/mtthwastn/statswithmatt/raw/master/hockey-with-r/hockey-rink.R'))
source(url('https://github.com/mtthwastn/statswithmatt/raw/master/hockey-with-r/gg-rink.R'))

pbp_base_ds %>%
  filter(season == 20202021 &
           game_id == '2020020174' &
           event_type == 'SHOT') %>%
  select(event_team,
         game_period,
         coords_x,
         coords_y) %>%
  collect() %>%
  mutate(
    coords_x = if_else(game_period == 2,
                       -coords_x,
                       coords_x),
    coords_y = if_else(game_period == 2,
                       -coords_y,
                       coords_y)
  ) %>%
  as_tibble() %>% 
  ggplot(aes(x = coords_x, y = coords_y)) +
  gg_rink(side = "right", specs = "nhl") +
  gg_rink(side = "left", specs = "nhl") +
  # geom_point(aes(x = coords_x, y = coords_y)) + 
  geom_point(aes(color = event_team),
             position = "jitter") +
  labs(title = "shot chart: 2010 NHL playoffs",
       subtitle = "NHL rink",
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(-30, 30, by = 5)) +
  scale_y_continuous(breaks = seq(-15, 15, by = 3))


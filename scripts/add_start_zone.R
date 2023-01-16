all1819 <- pbp_df |> 
  fix_strength_states() |> 
  arrange(game_id, event_idx) |> 
  group_by(game_id) |> 
  mutate(
    home_zone = case_when(x_fixed <  -20 ~ 'Def',
                          x_fixed >= -20 & x_fixed <= 20 ~ 'Neu',
                          x_fixed > 20 ~ 'Off',
                          TRUE ~ as.character(NA)),
    away_zone = case_when(x_fixed <  -20 ~ 'Off',
                          x_fixed >= -20 & x_fixed <= 20 ~ 'Neu',
                          x_fixed > 20 ~ 'Def',
                          TRUE ~ as.character(NA)),
    home_start_zone = case_when(event == 'Faceoff' ~ home_zone, 
                                secondary_type == 'On the fly' ~ 'OTF',
                                TRUE ~ as.character(NA)),
    away_start_zone = case_when(event == 'Faceoff' ~ away_zone,
                                secondary_type == 'On the fly' ~ 'OTF',
                                TRUE ~ as.character(NA)),
    home_start_zone = case_when(is.na(home_start_zone) & event != 'Change' ~ lag(home_start_zone),
                                is.na(home_start_zone) & event == 'Change' & lead(event, 2) == 'Faceoff' ~ lead(home_start_zone, 2),
                                is.na(home_start_zone) & event == 'Change' & lead(event, 1) == 'Faceoff' ~ lead(home_start_zone, 1),
                                TRUE ~ home_start_zone),
    away_start_zone = case_when(is.na(away_start_zone) & event != 'Change' ~ lag(away_start_zone),
                                is.na(away_start_zone) & event == 'Change' & lead(event, 2) == 'Faceoff' ~ lead(away_start_zone, 2),
                                is.na(away_start_zone) & event == 'Change' & lead(event, 1) == 'Faceoff' ~ lead(away_start_zone, 1),
                                TRUE ~ away_start_zone),
    # home_start_zone = case_when(is.na(home_start_zone) ~ lag(home_start_zone),
    #                             TRUE ~ home_start_zone),
    # away_start_zone = case_when(is.na(away_start_zone) ~ lag(away_start_zone),
    #                             TRUE ~ away_start_zone)
    duration = period_seconds - lag(period_seconds),
    is_corsi_event = case_when(event %in% c('Shot', 'Goal', 'Missed Shot', 'Blocked Shot') ~ 1,
                               TRUE ~ 0),
    game_score_state = paste(home_score, away_score, sep = "v")
    ) |> 
  fill(home_start_zone) |> 
  fill(away_start_zone) |> 
  select(home_start_zone, away_start_zone, duration, period_seconds, everything()) |> 
  filter(is_corsi_event == 1 & strength_state == '5v5')


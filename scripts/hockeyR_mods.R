

# Player stats ------------------------------------------------------------

calculate_player_stats_mod <- function(pbp = pbp_df) {
  # Requires hockeyR dataframe
  
  pbp <- clean_strength_states(pbp)
    
  
  pbp_season <- pbp |> 
    pull(season) |> 
    unique()
  
  pbp_game_ids <- pbp |> 
    pull(game_id) |> 
    unique()
  
  rosters <- nhlapi::nhl_teams_rosters() |> 
    unnest(roster.roster) |> 
    as_tibble() |> 
    janitor::clean_names() |>
    select(
      team_id = id,
      team_abbreviation = abbreviation,
      team_name,
      location_name,
      short_name,
      franchise_id,
      division_id,
      division_name,
      conference_id,
      conference_name,
      active,
      jersey_number,
      person_id,
      person_full_name,
      position_code,
      position_name,
      position_type,
      position_abbreviation
    )
  
  
  # Event Types -------------------------------------------------------------
  
  pbp.main_events <-
    c(
      'Goal',
      'Shot',
      'Missed Shot',
      'Blocked Shot',
      'Hit',
      'Giveaway',
      'Takeaway',
      'Faceoff',
      'Penalty'
    )
  
  pbp.shot_events <-     c('Shot',  'Goal')
  pbp.fenwick_events <-  c('Shot', 'Goal', 'Missed Shot')
  pbp.corsi_events <-
    c('Shot', 'Goal', 'Missed Shot', 'Blocked Shot')
  pbp.strength_states <-
    c(
      '3v3',
      '5v5',
      '4v4',
      '5v4',
      '4v5',
      '5v3',
      '3v5',
      '4v3',
      '3v4',
      '5v6',
      '6v5',
      '4v6',
      '6v4',
      '3v6',
      '6v3'
    )
  pbp.even_strength <-   c('5v5', '4v4', '3v3')
  pbp.pp_strength <-     c('5v4', '4v5', '5v3', '3v5', '4v3', '3v4')
  pbp.empty_net <-       c('5v6', '6v5', '4v6', '6v4', '3v6', '6v3')
  
  
  # Clean Player Names ------------------------------------------------------
  
  fixed_player_names = c(
    "Alex.Barré-Boulet",
    "Andrei.Svechnikov",
    "Axel.Jonsson-Fjallby",
    "Benoit-Olivier.Groulx",
    "Calvin.de Haan",
    "Drew.O'Connor",
    "Jacob.Bernard-Docker",
    "James.van Riemsdyk",
    "Jaret.Anderson-Dolan",
    "Jean-Gabriel.Pageau",
    "Joel.Eriksson Ek",
    "K'Andre.Miller",
    "Liam.O'Brien",
    "Logan.O'Connor",
    "Marc-Andre.Fleury",
    "Marc-Edouard.Vlasic",
    "Michael.Del Zotto",
    "Nicolas.Aube-Kubel",
    "Oliver.Ekman-Larsson",
    "Pierre-Edouard.Bellemare",
    "Pierre-Luc.Dubois",
    "Pierre-Olivier.Joseph",
    "Ryan.Nugent-Hopkins",
    "Ryan.O'Reilly",
    "Tim.Stützle",
    "Trevor.van Riemsdyk",
    "Zach.Aston-Reese",
    NULL
  )
  
  # Prepare data ------------------------------------------------------------


  # All events data ---------------------------------------------------------
  
  # Count all event_player_1 events
  main_events_data <- pbp |>
    dplyr::filter(event %in% pbp.main_events &
                    period_type != 'SHOOTOUT') |>
    dplyr::select(
      season,
      game_id,
      event_idx,
      event,
      player_name = event_player_1_name,
      player_id = event_player_1_id,
      player_type = event_player_1_type,
      event_team_abbr,
      season_type,
      strength_state
    ) |>
    dplyr::group_by(season,
                    # game_id,
                    event,
                    player_id,
                    player_name,
                    season_type,
                    # strength_state
                    ) |>
    dplyr::count() |>
    dplyr::mutate(
      event = dplyr::case_when(
        event == 'Goal' ~ 'Goals',
        event == 'Shot' ~ 'Shots',
        event == 'Blocked Shot' ~ 'Shots Blocked',
        event == 'Missed Shot' ~ 'Missed Shots',
        event == 'Faceoff' ~ 'Faceoffs Won',
        event == 'Hit' ~ 'Hits',
        event == 'Penalty' ~ 'Penalties Taken',
        event == 'Takeaway' ~ 'Takeaways',
        event == 'Giveaway' ~ 'Giveaways,',
        TRUE ~ event
      ),
    ) |>
    ungroup() |> 
    tidyr::spread(key = event, value = n, fill = 0) |> 
    janitor::clean_names() |>
    mutate(
      shots = shots + goals,
      shot_attempts = goals + shots + missed_shots + shots_blocked
    )
  
  shots_data <- pbp |>
    dplyr::filter(event %in% pbp.corsi_events &
                    period_type != 'SHOOTOUT') |>
    dplyr::select(
      season,
      game_id,
      event_idx,
      event,
      secondary_type,
      player_name = event_player_1_name,
      player_id = event_player_1_id,
      player_type = event_player_1_type,
      event_team_abbr,
      season_type,
      strength_state
    ) |> 
    filter(!is.na(secondary_type)) |> 
    dplyr::group_by(season,
                    # game_id,
                    event,
                    secondary_type,
                    player_id,
                    player_name,
                    season_type,
                    # strength_state
    ) |>
    dplyr::count() |>
    ungroup() |> 
    mutate(
      event_secondary = paste(event, secondary_type)
    ) |> 
    select(season, event_secondary, player_id, player_name, season_type, n) |> 
    tidyr::spread(key = event_secondary, value = n, fill = 0) |> 
    janitor::clean_names() |> 
    mutate(
      shot_backhand = goal_backhand + shot_backhand,
      shot_deflected = goal_deflected + shot_deflected,
      shot_slap_shot = goal_slap_shot + shot_slap_shot,
      shot_snap_shot = goal_snap_shot + shot_snap_shot,
      shot_tip_in = goal_tip_in + shot_tip_in,
      shot_wrap_around = goal_wrap_around + shot_wrap_around,
      shot_wrist_shot = goal_wrist_shot + shot_wrist_shot
    )
  
  # Count all event_player_2 events
  secondary_events_data <- pbp |>
    dplyr::filter(
      event %in% pbp.main_events &
        !is.na(event_player_2_id) &
        !(
          event %in% c(pbp.shot_events, 'Takeaway', 'Giveaway', 'Missed Shot')
        ) &
        period_type != 'SHOOTOUT'
    ) |>
    dplyr::select(
      season,
      game_id,
      event_idx,
      event,
      player_name = event_player_2_name,
      player_id = event_player_2_id,
      player_type = event_player_2_type,
      event_team_abbr,
      season_type,
      strength_state
    ) |>
    dplyr::group_by(season,
                    # game_id,
                    event,
                    player_id,
                    player_name,
                    season_type,
                    # strength_state
                    ) |>
    dplyr::count() |>
    dplyr::mutate(
      event = dplyr::case_when(
        event == 'Blocked Shot' ~ 'Blocked Shots',
        event == 'Faceoff' ~ 'Faceoffs Lost',
        event == 'Hit' ~ 'Hits Taken',
        event == 'Penalty' ~ 'Penalties Drawn',
        TRUE ~ event
      )
    ) |>
    ungroup() |> 
    tidyr::spread(key = event, value = n, fill = 0) |>
    janitor::clean_names()
  
  all_events_data <- main_events_data |>
    dplyr::full_join(secondary_events_data,
                     by = c('season',
                            'game_id',
                            'player_id',
                            'player_name',
                            'season_type',
                            'strength_state')
                     ) |> 
    dplyr::full_join(shots_data,
                     by = c('season',
                            'game_id',
                            'player_id',
                            'player_name',
                            'season_type',
                            'strength_state')
                     )
  
  all_events_data[is.na(all_events_data)] <- 0
  
  # Get data seasons
  data_seasons <- pbp %>% pull(season) %>% unique()
  
  
  # On-Ice Stats ------------------------------------------------------------
  
  on_ice_data <- pbp |>
    # filter(game_id == 2021030412) |> 
    filter(event %in% pbp.main_events & 
             period_type != 'SHOOTOUT') |>
    select(
      season,
      game_id,
      season_type,
      event_idx,
      event,
      event_team_abbr,
      home_abbreviation,
      away_abbreviation,
      contains('_on_'),
      strength_code,
      strength_state
    ) |>
    pivot_longer(
      cols = contains('_on_'),
      # names_to = NA,
      values_to = 'on_ice_player_name',
      values_drop_na = TRUE
    ) |>
    mutate(
      player_team_abbr = ifelse(
        grepl('home', name) == TRUE,
        home_abbreviation,
        away_abbreviation
      ),
      cumulative_corsi = case_when(
        player_team_abbr == event_team_abbr & 
          event %in% pbp.corsi_events &
          event != 'Blocked Shot' ~ 1,
        player_team_abbr != event_team_abbr &
          event %in% pbp.corsi_events &
          event == 'Blocked Shot' ~ 1,
        player_team_abbr != event_team_abbr &
          event %in% pbp.corsi_events &
          event != 'Blocked Shot' ~ -1,
        player_team_abbr == event_team_abbr &
          event %in% pbp.corsi_events &
          event == 'Blocked Shot' ~ -1
      ),
      cumulative_fenwick = case_when(
        player_team_abbr == event_team_abbr & 
          event %in% pbp.fenwick_events ~ 1,
        player_team_abbr != event_team_abbr &
          event %in% pbp.corsi_events ~ -1,
      ),
      is_goal_event = case_when(event == 'Goal' ~ 1,
                                TRUE ~ 0),
      is_corsi_event = case_when(event %in% pbp.corsi_events ~ 1,
                                 TRUE ~ 0),
      is_fenwick_event = case_when(event %in% pbp.fenwick_events ~ 1,
                                 TRUE ~ 0),
      is_hit_event = case_when(event == 'Hit' ~ 1,
                               TRUE ~ 0),
      is_giveaway_event = case_when(event == 'Giveaway' ~ 1,
                               TRUE ~ 0),
      is_takeaway_event = case_when(event == 'Takeaway' ~ 1,
                               TRUE ~ 0),
      corsi_for = case_when(
        player_team_abbr == event_team_abbr & 
          event %in% pbp.corsi_events &
          event != 'Blocked Shot' ~ 1,
        player_team_abbr != event_team_abbr & 
          event %in% pbp.corsi_events &
          event == 'Blocked Shot' ~ 1,
        TRUE ~ 0
      ),
      corsi_against = case_when(
        player_team_abbr == event_team_abbr & 
          event %in% pbp.corsi_events &
          event == 'Blocked Shot' ~ 1,
        player_team_abbr != event_team_abbr & 
          event %in% pbp.corsi_events &
          event != 'Blocked Shot' ~ 1,
        TRUE ~ 0
      ),
      fenwick_for = case_when(
        player_team_abbr == event_team_abbr & 
          event %in% pbp.fenwick_events ~ 1,
        TRUE ~ 0
      ),
      fenwick_against = case_when(
        player_team_abbr != event_team_abbr & 
          event %in% pbp.fenwick_events ~ 1,
        TRUE ~ 0
      ),
      shots_for = case_when(player_team_abbr == event_team_abbr & 
                              event == 'Shot' ~ 1,
                            TRUE ~ 0),
      shots_against = case_when(player_team_abbr != event_team_abbr & 
                              event == 'Shot' ~ 1,
                            TRUE ~ 0),
      goals_for = case_when(player_team_abbr == event_team_abbr & 
                              event == 'Goal' ~ 1,
                            TRUE ~ 0),
      goals_against = case_when(player_team_abbr != event_team_abbr & 
                                 event == 'Goal' ~ 1,
                               TRUE ~ 0),
      hits_for = case_when(player_team_abbr == event_team_abbr & 
                                  event == 'Hit' ~ 1,
                                TRUE ~ 0),
      hits_against = case_when(player_team_abbr != event_team_abbr & 
                                      event == 'Hit' ~ 1,
                                    TRUE ~ 0),
      giveaways_for = case_when(player_team_abbr == event_team_abbr & 
                                  event == 'Giveaway' ~ 1,
                                TRUE ~ 0),
      giveaways_against = case_when(player_team_abbr != event_team_abbr & 
                                  event == 'Giveaway' ~ 1,
                                TRUE ~ 0),
      takeaways_for = case_when(player_team_abbr == event_team_abbr & 
                                  event == 'Takeaway' ~ 1,
                                TRUE ~ 0),
      takeaways_against = case_when(player_team_abbr != event_team_abbr & 
                                      event == 'Takeaway' ~ 1,
                                    TRUE ~ 0),
      penalties_for = case_when(player_team_abbr == event_team_abbr & 
                                  event == 'Penalty' ~ 1,
                                TRUE ~ 0),
      penalties_against = case_when(player_team_abbr != event_team_abbr & 
                                      event == 'Penalty' ~ 1,
                                    TRUE ~ 0),
      strength_state = case_when(
        player_team_abbr != event_team_abbr &
          player_team_abbr != home_abbreviation & 
          !(strength_state %in% pbp.even_strength) & 
          corsi_against == 1 ~ paste0(substr(strength_state, 3, 3), 
                                      'v', 
                                      substr(strength_state, 1, 1)),
        player_team_abbr != event_team_abbr &
          player_team_abbr == home_abbreviation & 
          !(strength_state %in% pbp.even_strength) & 
          corsi_against == -1 ~ paste0(substr(strength_state, 3, 3), 
                                      'v', 
                                      substr(strength_state, 1, 1)),
        player_team_abbr != event_team_abbr &
          player_team_abbr != home_abbreviation & 
          !(strength_state %in% pbp.even_strength) & 
          corsi_against == -1 ~ paste0(substr(strength_state, 1, 1), 
                                      'v', 
                                      substr(strength_state, 3, 3)),
        player_team_abbr != event_team_abbr &
          player_team_abbr == home_abbreviation & 
          !(strength_state %in% pbp.even_strength) & 
          corsi_against == 1 ~ paste0(substr(strength_state, 1, 1), 
                                       'v', 
                                       substr(strength_state, 3, 3)),
        TRUE ~ strength_state
      ),
      strength = case_when(
        substr(strength_state, 1, 1) == substr(strength_state, 3, 3) ~ 'EV',
        substr(strength_state, 1, 1) > substr(strength_state, 3, 3) ~ 'PP',
        substr(strength_state, 1, 1) < substr(strength_state, 3, 3) ~ 'SH',
        substr(strength_state, 1, 1) > 6 | substr(strength_state, 3, 3) > 5 ~ 'EN'
      )
    ) |> 
    group_by(season,
             game_id,
             on_ice_player_name,
             player_team_abbr,
             season_type,
             strength_code,
             strength_state,
             NULL
             ) |> 
    summarise(
      total_corsi_events = sum(is_corsi_event, na.rm = T),
      cumulative_corsi = sum(cumulative_corsi, na.rm = T),
      corsi_for = sum(corsi_for, na.rm = T),
      corsi_against = sum(corsi_against, na.rm = T),
      total_fenwick_events = sum(is_fenwick_event, na.rm = T),
      cumulative_fenwick = sum(cumulative_corsi, na.rm = T),
      fenwick_for = sum(fenwick_for, na.rm = T),
      fenwick_against = sum(fenwick_against, na.rm = T),
      total_goal_events = sum(is_goal_event, na.rm = T),
      shots_for = sum(shots_for, na.rm = T), 
      shots_against = sum(shots_against, na.rm = T),
      goals_for = sum(goals_for, na.rm = T), 
      goals_against = sum(goals_against, na.rm = T),
      hits_for = sum(hits_for, na.rm = T), 
      hits_against = sum(hits_against, na.rm = T),
      giveaways_for = sum(giveaways_for, na.rm = T),
      giveaways_against = sum(giveaways_against, na.rm = T),
      takeaways_for = sum(takeaways_for, na.rm = T),
      takeaways_against = sum(takeaways_against, na.rm = T),
      penalties_for = sum(penalties_for, na.rm = T),
      penalties_against = sum(penalties_against, na.rm = T)
    ) |>
    mutate(
      corsi_pct = corsi_for / total_corsi_events,
      fenwick_pct = fenwick_for / total_fenwick_events,
      shooting_pct_for = goals_for / corsi_for,
      shooting_pct_against = goals_against / corsi_against,
      pdo = (goals_for / (shots_for + goals_for)) + (goals_against / (shots_against + goals_against))
    ) |> 
    ungroup()
  
  
  on_ice_data |>
    anti_join(all_events_data,
              by = c('season', 'game_id', 'player_name')) |>
    select(player_name) |>
    unique()
  
  all_events_data |>
    anti_join(on_ice_data,
              by = c('season', 'game_id', 'player_name')) |>
    select(player_name) |>
    unique()
  
  all_events_data |> 
    anti_join(on_ice_data,
              by = c('season', 'game_id', 'player_name')) |> 
    ungroup() |> 
    select(player_id, player_name) |> 
    filter(!(player_name %in% fixed_player_names)) |> 
    unique() |> 
    left_join(
      rosters |> 
        select(team_abbr = team_abbreviation, 
               jersey_number, 
               player_id = person_id, 
               contains('position')
        ),
      by = c('player_id')
    ) |> filter(position_code != 'G')
  
  # Time On Ice stats -----------------------------------------------------
  
  shifts_start_data <- pbp |>
    filter(event == 'Change' & !is.na(num_on)) |>
    select(
      season,
      game_id,
      event_idx,
      event_team,
      # period,
      # period_time,
      # period_seconds,
      game_seconds_start = game_seconds,
      num_on,
      players_on,
      # num_off,
      # players_off,
      event,
      secondary_type,
      strength_code,
      strength_state,
      # game_seconds_remaining,
      NULL
    ) |>
    arrange(game_id, event_idx) |> 
    mutate(
      secondary_type = case_when(
        game_seconds_start %% 1200 == 0 ~ 'Line Change',
        TRUE ~ secondary_type
      ),
      strength_state = case_when(
        game_seconds_start %% 1200 == 0 &
          substr(strength_state, 3, 3) == '0' ~ paste0(substr(lead(strength_state), 3, 3),
                                                       substr(lead(strength_state), 2, 2),
                                                       substr(lead(strength_state), 1, 1)),
        TRUE ~ strength_state
      ),
      strength_code = case_when(
        is.na(strength_code) & 
          as.numeric(substr(strength_state, 1, 1)) > as.numeric(substr(strength_state, 3, 3)) ~ 'PP',
        is.na(strength_code) & 
          as.numeric(substr(strength_state, 1, 1)) < as.numeric(substr(strength_state, 3, 3)) ~ 'SH',
        is.na(strength_code) & 
          as.numeric(substr(strength_state, 1, 1)) == as.numeric(substr(strength_state, 3, 3)) ~ 'EV',
        TRUE ~ strength_code
      )
    ) |> 
    separate(
      players_on,
      sep = ', ',
      into = c(
        'player_on_1',
        'player_on_2',
        'player_on_3',
        'player_on_4',
        'player_on_5',
        'player_on_6',
        'player_on_7',
        'player_on_8',
        'player_on_9',
        'player_on_10',
        'player_on_11',
        'player_on_12'
      )
    ) |>
    suppressWarnings() |>
    pivot_longer(
      cols = c(
        'player_on_1',
        'player_on_2',
        'player_on_3',
        'player_on_4',
        'player_on_5',
        'player_on_6',
        'player_on_7',
        'player_on_8',
        'player_on_9',
        'player_on_10',
        'player_on_11',
        'player_on_12'
      ),
      names_to = 'drop_column',
      values_to = 'players_on',
      values_drop_na = TRUE
    ) |>
    select(season,
           game_id,
           event_team,
           player_name = players_on,
           secondary_type,
           strength_code,
           strength_state,
           game_seconds_start) |>
    unique() |>
    group_by(game_id, event_team, player_name) |>
    mutate(shift_idx = row_number()) |> 
    left_join(
      rosters |> 
        mutate(
          event_team = paste(location_name, team_name)
        ) |> 
        select(
          player_name = person_full_name,
          player_id = person_id,
          player_team_abbr = team_abbreviation,
          event_team
        ),
      by = c('player_name', 'event_team')
    )
  
  shifts_end_data <- pbp |>
    filter(event == 'Change' & !is.na(num_off)) |>
    select(
      season,
      game_id,
      event_idx,
      event_team,
      period,
      # period_time,
      # period_seconds,
      game_seconds_finish = game_seconds,
      # num_on,
      # players_on,
      num_off,
      players_off,
      event,
      secondary_type,
      strength_code,
      strength_state,
      # game_seconds_remaining,
      NULL
    ) |>
    arrange(game_id, event_idx) |> 
    mutate(
      strength_state = case_when(
        game_seconds_finish %% 1200 == 0 &
          substr(strength_state, 3, 3) == '0' ~ paste0(substr(lag(strength_state), 3, 3), 
                                                       substr(strength_state, 2, 2), 
                                                       num_off - 1),
        game_seconds_finish %% 1200 == 0 &
          substr(strength_state, 1, 1) == '0' ~ paste0(num_off - 1, 
                                                       substr(strength_state, 2, 3)),
        TRUE ~ strength_state
      ),
      strength_code = case_when(
        is.na(strength_code) & 
          as.numeric(substr(strength_state, 1, 1)) > as.numeric(substr(strength_state, 3, 3)) ~ 'PP',
        is.na(strength_code) & 
          as.numeric(substr(strength_state, 1, 1)) < as.numeric(substr(strength_state, 3, 3)) ~ 'SH',
        is.na(strength_code) & 
          as.numeric(substr(strength_state, 1, 1)) == as.numeric(substr(strength_state, 3, 3)) ~ 'EV',
        TRUE ~ strength_code
      )
    ) |> 
    separate(
      players_off,
      sep = ', ',
      into = c(
        'player_off_1',
        'player_off_2',
        'player_off_3',
        'player_off_4',
        'player_off_5',
        'player_off_6',
        'player_off_7',
        'player_off_8',
        'player_off_9',
        'player_off_10',
        'player_off_11',
        'player_off_12'
      )
    ) |>
    suppressWarnings() |>
    pivot_longer(
      cols = c(
        'player_off_1',
        'player_off_2',
        'player_off_3',
        'player_off_4',
        'player_off_5',
        'player_off_6',
        'player_off_7',
        'player_off_8',
        'player_off_9',
        'player_off_10',
        'player_off_11',
        'player_off_12'
      ),
      names_to = 'drop_column',
      values_to = 'players_off',
      values_drop_na = TRUE
    ) |>
    select(season,
           game_id,
           event_team,
           player_name = players_off,
           secondary_type,
           strength_code,
           strength_state,
           game_seconds_finish) |>
    unique() |>
    group_by(game_id, event_team, player_name) |>
    mutate(shift_idx = row_number()) |> 
    left_join(
      rosters |> 
        mutate(
          event_team = paste(location_name, team_name)
        ) |> 
        select(
          player_name = person_full_name,
          player_id = person_id,
          player_team_abbr = team_abbreviation,
          event_team
        ),
      by = c('player_name', 'event_team')
    )
  
  shifts_data <- shifts_start_data |> 
    left_join(
      shifts_end_data,
      by = c(
        'season',
        'game_id',
        'event_team',
        'player_name',
        'player_id',
        'player_team_abbr',
        'shift_idx'
      ),
      suffix = c('_shift_start', '_shift_finish')
    ) %>% 
    mutate(
      shift_length = game_seconds_finish - game_seconds_start
      ) |> 
    group_by(
      season, 
      game_id, 
      player_id,
      player_name, 
      player_team_abbr,
      strength_code_shift_start, 
      strength_state_shift_start
    ) |> 
    summarise(
      n_shifts = n(),
      toi = sum(shift_length, na.rm = T)
    ) |> 
    mutate(
      avg_shift = toi / n_shifts
    )
  
  # Penalty stats ---------------------------------------------------------
  
  total_pim_taken_data <- pbp |>
    dplyr::filter(event == 'Penalty' &
                    penalty_severity != 'Bench Minor') |>
    dplyr::rename(player_id = event_player_1_id,
                  player_name = event_player_1_name) |>
    dplyr::group_by(season, game_id, player_id, player_name) |>
    dplyr::summarise(penalty_minutes_taken = sum(penalty_minutes, na.rm = T))
  
  total_pim_drawn_data <- pbp |>
    dplyr::filter(event == 'Penalty' &
                    penalty_severity != 'Bench Minor') |>
    dplyr::rename(player_id = event_player_2_id,
                  player_name = event_player_2_name) |>
    dplyr::group_by(season, game_id, player_id, player_name) |>
    dplyr::summarise(penalty_minutes_drawn = sum(penalty_minutes, na.rm = T)) |> 
    filter(!is.na(player_id))
  
  total_penalties_taken_data <- pbp |>
    dplyr::filter(event == 'Penalty' &
                    penalty_severity != 'Bench Minor') |>
    dplyr::select(
      season,
      game_id,
      event_idx,
      secondary_type,
      player_name = event_player_1_name,
      player_id = event_player_1_id,
      player_type = event_player_1_type
    ) |>
    dplyr::group_by(season,
                    game_id,
                    secondary_type,
                    player_id,
                    player_name) |>
    dplyr::count() |>
    mutate(
      secondary_type = paste('Taken', secondary_type)
    ) |> 
    tidyr::spread(key = secondary_type, value = n, fill = 0) |>
    janitor::clean_names()
  
  total_penalties_drawn_data <- pbp |>
    dplyr::filter(event == 'Penalty' &
                    penalty_severity != 'Bench Minor') |>
    dplyr::select(
      season,
      game_id,
      event_idx,
      secondary_type,
      player_name = event_player_2_name,
      player_id = event_player_2_id,
      player_type = event_player_2_type
    ) |>
    dplyr::group_by(season,
                    game_id,
                    secondary_type,
                    player_id,
                    player_name) |>
    dplyr::count() |>
    mutate(
      secondary_type = paste('Drawn', secondary_type)
    ) |> 
    tidyr::spread(key = secondary_type, value = n, fill = 0) |>
    janitor::clean_names()
  
  penalty_data <- total_pim_taken_data |>
    dplyr::left_join(total_penalties_taken_data,
                     by = c('season', 'game_id', 'player_id', 'player_name')) |> 
    dplyr::left_join(total_pim_drawn_data, 
                     by = c('season', 'game_id', 'player_id', 'player_name')) |> 
    dplyr::left_join(total_penalties_drawn_data,
                     by = c('season', 'game_id', 'player_id', 'player_name'))
  
  
  # Combine all stats -------------------------------------------------------
  
  if (length(which(data_seasons >= 2013)) > 0) {
    snaps_df <-
      nflreadr::load_snap_counts(seasons = data_seasons[data_seasons >= 2013]) %>%
      left_join(
        roster_df %>%
          select(gsis_id, pfr_id, first_name, last_name),
        by = c('pfr_player_id' = 'pfr_id')
      ) %>%
      left_join(schedule_df %>%
                  select(game_id, week),
                by = c('game_id', 'week')) %>%
      mutate(
        name_snaps = paste0(substr(first_name, 1, 1), '.', last_name),
        offense_snaps = offense_snaps %>% as.integer(),
        offense_pct = offense_pct,
        defense_snaps = defense_snaps %>% as.integer(),
        defense_pct = defense_pct,
        st_snaps = st_snaps %>% as.integer(),
        st_pct = st_pct
      ) %>%
      select(
        player_id = gsis_id,
        week,
        season,
        name_snaps,
        team_snaps = team,
        offense_snaps:st_pct
      )
  } else {
    snaps_df <- tibble() %>%
      mutate(
        player_id = as.character(NA),
        week = as.integer(NA),
        season = as.integer(NA),
        name_snaps = as.character(NA)
      )
  }
  
  
  # Combine all stats -------------------------------------------------------
  
  # combine all the stats together
  player_df <- pass_df %>%
    dplyr::full_join(rush_df, by = c("player_id", "week", "season")) %>%
    dplyr::full_join(rec_df, by = c("player_id", "week", "season")) %>%
    dplyr::full_join(st_tds, by = c("player_id", "week", "season")) %>%
    dplyr::full_join(snaps_df, by = c("player_id", "week", "season")) %>%
    dplyr::left_join(s_type, by = c("season", "week")) %>%
    dplyr::mutate(
      player_name = dplyr::case_when(
        !is.na(.data$name_pass) ~ .data$name_pass,!is.na(.data$name_rush) ~ .data$name_rush,!is.na(.data$name_receiver) ~ .data$name_receiver,!is.na(.data$name_snaps) ~ .data$name_snaps,
        TRUE ~ .data$name_st
      ),
      recent_team = dplyr::case_when(
        !is.na(.data$team_pass) ~ .data$team_pass,!is.na(.data$team_rush) ~ .data$team_rush,!is.na(.data$team_receiver) ~ .data$team_receiver,!is.na(.data$team_snaps) ~ .data$team_snaps,
        TRUE ~ .data$team_st
      )
    ) %>%
    dplyr::left_join(
      data %>% select(season, game_id, week, recent_team = posteam) %>% unique(),
      by = c('season', 'week', 'recent_team')
    ) %>%
    dplyr::select(tidyselect::any_of(
      c(
        # id information
        "player_id",
        "player_name",
        "recent_team",
        "season",
        "week",
        "game_id",
        "season_type",
        
        # offense stats
        "offense_snaps",
        "offense_pct",
        
        # passing stats
        "completions",
        "attempts",
        "passing_yards",
        "passing_tds",
        "interceptions",
        "sacks",
        "sack_yards",
        "sack_fumbles",
        "sack_fumbles_lost",
        "passing_air_yards",
        "passing_yards_after_catch",
        "passing_first_downs",
        "passing_epa",
        "passing_2pt_conversions",
        "pacr",
        "anya",
        "dakota",
        
        # rushing stats
        "carries",
        "rushing_yards",
        "rushing_tds",
        "rushing_fumbles",
        "rushing_fumbles_lost",
        "rushing_first_downs",
        "rushing_epa",
        "rushing_2pt_conversions",
        "hvt",
        
        # receiving stats
        "receptions",
        "targets",
        "receiving_yards",
        "receiving_tds",
        "receiving_fumbles",
        "receiving_fumbles_lost",
        "receiving_air_yards",
        "receiving_yards_after_catch",
        "receiving_first_downs",
        "receiving_epa",
        "receiving_2pt_conversions",
        "racr",
        "target_share",
        "air_yards_share",
        "wopr",
        "hvt",
        
        # special teams
        "special_teams_tds",
        "st_snaps",
        "st_pct"
        
      )
    )) %>%
    dplyr::filter(!is.na(.data$player_id))
  
  player_df_nas <- is.na(player_df)
  epa_index <-
    which(
      dimnames(player_df_nas)[[2]] %in% c(
        "season_type",
        "game_id",
        "week",
        "passing_epa",
        "rushing_epa",
        "receiving_epa",
        "dakota",
        "racr",
        "target_share",
        "air_yards_share",
        "wopr",
        "pacr"
      )
    )
  player_df_nas[, epa_index] <- c(FALSE)
  
  player_df[player_df_nas] <- 0
  
  player_df <- player_df %>%
    dplyr::mutate(
      fantasy_points =
        1 / 25 * .data$passing_yards +
        4 * .data$passing_tds+-2 * .data$interceptions +
        1 / 10 * (.data$rushing_yards + .data$receiving_yards) +
        6 * (
          .data$rushing_tds + .data$receiving_tds + .data$special_teams_tds
        ) +
        2 * (
          .data$passing_2pt_conversions + .data$rushing_2pt_conversions + .data$receiving_2pt_conversions
        )+-2 * (
          .data$sack_fumbles_lost + .data$rushing_fumbles_lost + .data$receiving_fumbles_lost
        ),
      
      fantasy_points_ppr = .data$fantasy_points + .data$receptions,
      fantasy_points_half_ppr = .data$fantasy_points + (.data$receptions * .5)
    ) %>%
    dplyr::arrange(.data$player_id, .data$season, .data$week)
  
  
  # if user doesn't want week-by-week input, aggregate the whole df
  if (isFALSE(weekly)) {
    player_df <- player_df %>%
      mutate(
        potential_offense_snaps = .data$offense_snaps / .data$offense_pct,
        potential_st_snaps = .data$st_snaps / .data$st_pct
      ) %>%
      dplyr::group_by(.data$player_id) %>%
      dplyr::summarise(
        player_name = custom_mode(.data$player_name),
        games = dplyr::n(),
        recent_team = dplyr::last(.data$recent_team),
        offense_snaps = sum(.data$offense_snaps),
        # potential_offense_snaps = sum(.data$potential_offense_snaps), # total potential_snaps
        offense_pct = sum(.data$offense_snaps) / sum(.data$potential_offense_snaps),
        # calculate total snaps percentage
        # passing
        completions = sum(.data$completions),
        attempts = sum(.data$attempts),
        passing_yards = sum(.data$passing_yards),
        passing_tds = sum(.data$passing_tds),
        interceptions = sum(.data$interceptions),
        sacks = sum(.data$sacks),
        sack_yards = sum(.data$sack_yards),
        sack_fumbles = sum(.data$sack_fumbles),
        sack_fumbles_lost = sum(.data$sack_fumbles_lost),
        passing_air_yards = sum(.data$passing_air_yards),
        passing_yards_after_catch = sum(.data$passing_yards_after_catch),
        passing_first_downs = sum(.data$passing_first_downs),
        passing_epa = dplyr::if_else(all(is.na(
          .data$passing_epa
        )), NA_real_, sum(.data$passing_epa, na.rm = TRUE)),
        passing_2pt_conversions = sum(.data$passing_2pt_conversions),
        pacr = .data$passing_yards / .data$passing_air_yards,
        anya = (
          passing_yards - sack_yards + (20 * passing_tds) - (45 * interceptions)
        ) / (attempts + sacks),
        
        # rushing
        carries = sum(.data$carries),
        rushing_yards = sum(.data$rushing_yards),
        rushing_tds = sum(.data$rushing_tds),
        rushing_fumbles = sum(.data$rushing_fumbles),
        rushing_fumbles_lost = sum(.data$rushing_fumbles_lost),
        rushing_first_downs = sum(.data$rushing_first_downs),
        rushing_epa = dplyr::if_else(all(is.na(
          .data$rushing_epa
        )), NA_real_, sum(.data$rushing_epa, na.rm = TRUE)),
        rushing_2pt_conversions = sum(.data$rushing_2pt_conversions),
        hvt = sum(.data$hvt, na.rm = TRUE),
        
        # receiving
        receptions = sum(.data$receptions),
        targets = sum(.data$targets),
        receiving_yards = sum(.data$receiving_yards),
        receiving_tds = sum(.data$receiving_tds),
        receiving_fumbles = sum(.data$receiving_fumbles),
        receiving_fumbles_lost = sum(.data$receiving_fumbles_lost),
        receiving_air_yards = sum(.data$receiving_air_yards),
        receiving_yards_after_catch = sum(.data$receiving_yards_after_catch),
        receiving_first_downs = sum(.data$receiving_first_downs),
        receiving_epa = dplyr::if_else(
          all(is.na(.data$receiving_epa)),
          NA_real_,
          sum(.data$receiving_epa, na.rm = TRUE)
        ),
        receiving_2pt_conversions = sum(.data$receiving_2pt_conversions),
        racr = .data$receiving_yards / .data$receiving_air_yards,
        target_share = dplyr::if_else(
          all(is.na(.data$target_share)),
          NA_real_,
          mean(.data$target_share, na.rm = TRUE)
        ),
        air_yards_share = dplyr::if_else(
          all(is.na(.data$air_yards_share)),
          NA_real_,
          mean(.data$air_yards_share, na.rm = TRUE)
        ),
        wopr = 1.5 * .data$target_share + 0.7 * .data$air_yards_share,
        
        # special teams
        special_teams_tds = sum(.data$special_teams_tds),
        st_snaps = sum(.data$st_snaps),
        # potential_st_snaps = sum(.data$potential_st_snaps), # total potential_snaps
        st_pct = sum(.data$st_snaps) / sum(.data$potential_st_snaps),
        # calculate total snaps percentage
        
        # fantasy
        fantasy_points = sum(.data$fantasy_points),
        fantasy_points_ppr = sum(.data$fantasy_points_ppr),
        fantasy_points_half_ppr = sum(.data$fantasy_points_half_ppr)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        hvt_percentage = hvt / carries,
        hvt_per_game = hvt / games,
        racr = dplyr::case_when(
          is.nan(.data$racr) ~ NA_real_,
          .data$receiving_air_yards == 0 ~ 0,
          # following Josh Hermsmeyer's definition, RACR stays < 0 for RBs (and FBs) and is set to
          # 0 for Receivers. The list "racr_ids" includes all known RB and FB gsis_ids
          .data$receiving_air_yards < 0 &
            !.data$player_id %in% racr_ids$gsis_id ~ 0,
          TRUE ~ .data$racr
        ),
        pacr = dplyr::case_when(
          is.nan(.data$pacr) ~ NA_real_,
          .data$passing_air_yards <= 0 ~ 0,
          TRUE ~ .data$pacr
        )
      ) %>%
      add_dakota(pbp = pbp, weekly = weekly) %>%
      dplyr::select(.data$player_id:.data$pacr,
                    .data$dakota,
                    dplyr::everything())
  }
  
  return(player_df)
}



# Player stats ------------------------------------------------------------

calculate_player_stats_mod <- function(pbp = pbp_df) {
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
    "Jaret.Anderson-Dolan",
    "Zach.Aston-Reese",
    "Nicolas.Aube-Kubel",
    "Pierre-Edouard.Bellemare",
    "Alex.Barré-Boulet",
    "Jacob.Bernard-Docker",
    "Calvin.de Haan",
    "Michael.Del Zotto",
    "Pierre-Luc.Dubois",
    "Oliver.Ekman-Larsson",
    "Joel.Eriksson Ek",
    "Marc-Andre.Fleury",
    "Benoit-Olivier.Groulx",
    "Axel.Jonsson-Fjallby",
    "K'Andre.Miller",
    "Ryan.Nugent-Hopkins",
    "Pierre-Olivier.Joseph",
    "Liam.O'Brien",
    "Drew.O'Connor",
    "Logan.O'Connor",
    "Ryan.O'Reilly",
    "Jean-Gabriel.Pageau",
    "James.van Riemsdyk",
    "Trevor.van Riemsdyk",
    "Tim.Stützle",
    "Andrei.Svechnikov",
    "Marc-Edouard.Vlasic",
    NULL
  )
  
  # Prepare data ------------------------------------------------------------
  
  # filter down to the 2 dfs we need
  
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
      event_team_abbr
    ) |>
    dplyr::group_by(season,
                    game_id,
                    event,
                    player_id,
                    player_name) |>
    dplyr::count() |>
    dplyr::mutate(
      event = dplyr::case_when(
        event == 'Goal' ~ 'Goals',
        event == 'Blocked Shot' ~ 'Blocked Shots',
        event == 'Faceoff' ~ 'Faceoffs Won',
        event == 'Hit' ~ 'Hits',
        event == 'Penalty' ~ 'Penalties Taken',
        event == 'Takeaway' ~ 'Takeaways',
        event == 'Giveaway' ~ 'Giveaways,',
        TRUE ~ event
      )
    ) |>
    ungroup() |> 
    tidyr::spread(key = event, value = n) |>
    janitor::clean_names()
  
  # Count all event_player_2 events
  secondary_events_data <- pbp |>
    dplyr::filter(
      event %in% pbp.main_events &
        !is.na(event_player_2_id) &
        !(
          event %in% c(st.shot_events, 'Takeaway', 'Giveaway', 'Missed Shot')
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
      event_team_abbr
    ) |>
    dplyr::group_by(season,
                    game_id,
                    event,
                    player_id,
                    player_name) |>
    dplyr::count() |>
    dplyr::mutate(
      event = dplyr::case_when(
        event == 'Shot' ~ 'Shots Against',
        event == 'Blocked Shot' ~ 'Shots Blocked',
        event == 'Faceoff' ~ 'Faceoffs Lost',
        event == 'Hit' ~ 'Hits Taken',
        event == 'Penalty' ~ 'Penalties Drawn',
        TRUE ~ event
      )
    ) |>
    ungroup() |> 
    tidyr::spread(key = event, value = n) |>
    janitor::clean_names()
  
  all_events_data <- main_events_data |>
    dplyr::left_join(secondary_events_data,
                     by = c('season', 'game_id', 'player_id', 'player_name'))
  
  # Get data seasons
  data_seasons <- pbp %>% pull(season) %>% unique()
  
  
  # On-Ice Stats ------------------------------------------------------------
  
  corsi_data <- pbp |>
    filter(event %in% pbp.corsi_events) |>
    select(
      season,
      game_id,
      event_idx,
      event,
      event_team_abbr,
      home_abbreviation,
      away_abbreviation,
      contains('_on_'),
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
        player_team_abbr == event_team_abbr & event != 'Blocked Shot' ~ 1,
        player_team_abbr != event_team_abbr &
          event == 'Blocked Shot' ~ 1,
        player_team_abbr != event_team_abbr &
          event != 'Blocked Shot' ~ -1,
        player_team_abbr == event_team_abbr &
          event == 'Blocked Shot' ~ -1
      ),
      is_corsi_event = 1,
      corsi_for = ifelse(player_team_abbr == event_team_abbr, 1, 0),
      corsi_against = ifelse(player_team_abbr != event_team_abbr, 1, 0),
      strength_state = case_when(
        player_team_abbr != event_team_abbr &
          player_team_abbr != home_abbreviation & 
          !(strength_state %in% pbp.even_strength) & 
          corsi_against == 1 ~ paste0(substr(strength_state, 3, 3), 
                                      substr(strength_state, 2, 2), 
                                      substr(strength_state, 1, 1)),
        TRUE ~ strength_state
      ),
      strength = case_when(
        strength_state %in% c('5v5', '4v4', '3v3')
      )
    ) |> 
    group_by(season,
             # game_id,
             on_ice_player_name,
             player_team_abbr,
             strength_state) |> 
    summarise(
      total_corsi_events = sum(is_corsi_event, na.rm = T),
      cumulative_corsi = sum(cumulative_corsi, na.rm = T),
      corsi_for = sum(corsi_for, na.rm = T),
      corsi_against = sum(corsi_against, na.rm = T)
    ) |>
    mutate(
      corsi_pct = corsi_for / total_corsi_events
    ) |> 
    ungroup()
  
  fenwick_data <- pbp |>
    filter(event %in% pbp.fenwick_events) |>
    select(
      season,
      game_id,
      event_idx,
      event,
      event_team_abbr,
      home_abbreviation,
      away_abbreviation,
      contains('_on_'),
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
      fenwick = case_when(
        player_team_abbr == event_team_abbr ~ 1,
        player_team_abbr != event_team_abbr ~ -1
      ),
      is_fenwick_event = 1,
      fenwick_for = ifelse(player_team_abbr == event_team_abbr, 1, 0),
      fenwick_against = ifelse(player_team_abbr != event_team_abbr, 1, 0)
    ) |>
    # select(season,
    #        game_id,
    #        player_team_abbr,
    #        on_ice_player_name,
    #        is_fenwick_event,
    #        fenwick_for,
    #        fenwick_against) |>
    group_by(season, game_id, on_ice_player_name, player_team_abbr, strength_state) |>
    summarise(
      player_team_abbr = first(player_team_abbr),
      
      total_fenwick_events = sum(is_fenwick_event, na.rm = T),
      fenwick_for = sum(fenwick_for, na.rm = T),
      fenwick_against = sum(fenwick_against, na.rm = T)
    ) |>
    mutate(
      fenwick_pct = fenwick_for / total_fenwick_events
    ) |> 
    ungroup()
  
  on_ice_data <- corsi_data |>
    left_join(fenwick_data,
              by = c(
                'season',
                'game_id',
                'on_ice_player_name',
                'player_team_abbr'
              )) |>
    rename(player_name = on_ice_player_name)
  
  on_ice_data |> 
    group_by(player_name, player_team_abbr) |> 
    summarise(total_corsi_events = sum(total_corsi_events, na.rm = T),
              corsi_for = sum(corsi_for, na.rm = T),
              corsi_against = sum(corsi_against, na.rm = T),
              total_fenwick_events = sum(total_fenwick_events, na.rm = T),
              fenwick_for = sum(fenwick_for, na.rm = T),
              fenwick_against = sum(fenwick_against, na.rm = T)
    ) |> 
    mutate(corsi_pct = corsi_for / total_corsi_events,
           fenwick_pct = fenwick_for / total_fenwick_events) |> 
    arrange(-corsi_pct)
  
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
  
  # shifts_start_data <-
  pbp |>
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
      event_type,
      # game_seconds_remaining,
      NULL
    ) |>
    arrange(game_id, event_idx) |>
    # select(players_on) |>
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
    select(game_id,
           event_team,
           players = players_on,
           game_seconds_start) |>
    unique() |>
    group_by(game_id, event_team, players) |>
    mutate(shift_idx = row_number()) |>
    left_join(
      pbp |>
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
          event_type,
          # game_seconds_remaining,
          NULL
        ) |>
        arrange(game_id, event_idx) |>
        # select(players_on) |>
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
        select(game_id,
               event_team,
               players = players_off,
               game_seconds_finish) |>
        unique() |>
        group_by(game_id, event_team, players) |>
        mutate(shift_idx = row_number()),
      by = c('game_id', 'event_team', 'players', 'shift_idx')
    ) |>
    mutate(shift_length = game_seconds_finish - game_seconds_start)
  
  # Penalty stats ---------------------------------------------------------
  
  total_pim_data <- pbp |>
    dplyr::filter(event == 'Penalty' &
                    penalty_severity != 'Bench Minor') |>
    dplyr::rename(player_id = event_player_1_id,
                  player_name = event_player_1_name) |>
    dplyr::group_by(season, game_id, player_id, player_name) |>
    dplyr::summarise(penalty_minutes = sum(penalty_minutes, na.rm = T))
  
  total_penalties_data <- pbp |>
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
    # dplyr::mutate(
    #   event = dplyr::case_when(
    #     secondary_type == 'Boarding' ~ 'Boarding Penalties',
    #     secondary_type == 'Butt ending' ~ 'Butt ending Penalties',
    #     secondary_type == 'Closing hand on puck' ~ 'Closing hand on puck Penalties',
    #     secondary_type == 'Penalty' ~ 'Penalties Drawn',
    #     TRUE ~ event
    #   )
    # ) |>
    tidyr::spread(key = secondary_type, value = n) |>
    janitor::clean_names()
  
  penalty_data <- total_pim_data |>
    dplyr::left_join(total_penalties_data,
                     by = c('season', 'game_id', 'player_id', 'player_name'))
  
  # Receiving stats ---------------------------------------------------------
  
  # receiver df 1: primary receiver
  rec <- data %>%
    dplyr::filter(!is.na(.data$receiver_player_id)) %>%
    dplyr::mutate(hvt = ifelse(yardline_100 <= 10, 1, 0)) %>%
    dplyr::group_by(.data$receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      name_receiver = dplyr::first(.data$receiver_player_name),
      team_receiver = dplyr::first(.data$posteam),
      yards = sum(.data$receiving_yards, na.rm = TRUE),
      receptions = sum(.data$complete_pass == 1),
      targets = dplyr::n(),
      tds = sum(.data$td_player_id == .data$receiver_player_id, na.rm = TRUE),
      receiving_fumbles = sum(
        .data$fumble == 1 &
          .data$fumbled_1_player_id == .data$receiver_player_id &
          is.na(.data$lateral_receiver_player_id)
      ),
      receiving_fumbles_lost = sum(
        .data$fumble_lost == 1 &
          .data$fumbled_1_player_id == .data$receiver_player_id &
          is.na(.data$lateral_receiver_player_id)
      ),
      receiving_air_yards = sum(.data$air_yards, na.rm = TRUE),
      receiving_yards_after_catch = sum(.data$yards_after_catch, na.rm = TRUE),
      receiving_first_downs = sum(
        .data$first_down_pass & is.na(.data$lateral_receiver_player_id)
      ),
      receiving_epa = sum(.data$epa, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # receiver df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_receiver_player_id)) %>%
    dplyr::mutate(hvt = ifelse(yardline_100 <= 10, 1, 0)) %>%
    dplyr::group_by(.data$lateral_receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      lateral_yards = sum(.data$lateral_receiving_yards, na.rm = TRUE),
      lateral_tds = sum(
        .data$td_player_id == .data$lateral_receiver_player_id,
        na.rm = TRUE
      ),
      lateral_att = dplyr::n(),
      lateral_fds = sum(.data$first_down_pass, na.rm = T),
      lateral_fumbles = sum(.data$fumble, na.rm = T),
      lateral_fumbles_lost = sum(.data$fumble_lost, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(receiver_player_id = .data$lateral_receiver_player_id) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          .data$type == "lateral_receiving" &
            .data$season %in% data$season & .data$week %in% data$week
        ) %>%
        dplyr::select(
          "season",
          "week",
          "receiver_player_id" = .data$gsis_player_id,
          "lateral_yards" = .data$yards
        ) %>%
        dplyr::mutate(lateral_tds = 0L, lateral_att = 1L)
    )
  
  # receiver df 3: team receiving for WOPR
  rec_team <- data %>%
    dplyr::filter(!is.na(.data$receiver_player_id)) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarize(
      team_targets = dplyr::n(),
      team_air_yards = sum(.data$air_yards, na.rm = TRUE),
    ) %>%
    dplyr::ungroup()
  
  # rec df: join
  rec_df <- rec %>%
    dplyr::left_join(laterals, by = c("receiver_player_id", "week", "season")) %>%
    dplyr::left_join(rec_team, by = c("team_receiver" = "posteam", "week", "season")) %>%
    dplyr::mutate(
      lateral_yards = dplyr::if_else(is.na(.data$lateral_yards), 0, .data$lateral_yards),
      lateral_tds = dplyr::if_else(is.na(.data$lateral_tds), 0L, .data$lateral_tds),
      lateral_fumbles = dplyr::if_else(is.na(.data$lateral_fumbles), 0, .data$lateral_fumbles),
      lateral_fumbles_lost = dplyr::if_else(
        is.na(.data$lateral_fumbles_lost),
        0,
        .data$lateral_fumbles_lost
      ),
      lateral_fds = dplyr::if_else(is.na(.data$lateral_fds), 0, .data$lateral_fds)
    ) %>%
    dplyr::mutate(
      receiving_yards = .data$yards + .data$lateral_yards,
      receiving_tds = .data$tds + .data$lateral_tds,
      receiving_yards_after_catch = .data$receiving_yards_after_catch + .data$lateral_yards,
      receiving_first_downs = .data$receiving_first_downs + .data$lateral_fds,
      receiving_fumbles = .data$receiving_fumbles + .data$lateral_fumbles,
      receiving_fumbles_lost = .data$receiving_fumbles_lost + .data$lateral_fumbles_lost,
      racr = .data$receiving_yards / .data$receiving_air_yards,
      racr = dplyr::case_when(
        is.nan(.data$racr) ~ NA_real_,
        .data$receiving_air_yards == 0 ~ 0,
        # following Josh Hermsmeyer's definition, RACR stays < 0 for RBs (and FBs) and is set to
        # 0 for Receivers. The list "racr_ids" includes all known RB and FB gsis_ids
        .data$receiving_air_yards < 0 &
          !.data$receiver_player_id %in% racr_ids$gsis_id ~ 0,
        TRUE ~ .data$racr
      ),
      target_share = .data$targets / .data$team_targets,
      air_yards_share = .data$receiving_air_yards / .data$team_air_yards,
      wopr = 1.5 * .data$target_share + 0.7 * .data$air_yards_share
    ) %>%
    dplyr::rename(player_id = .data$receiver_player_id) %>%
    dplyr::select(
      "player_id",
      "week",
      "season",
      "name_receiver",
      "team_receiver",
      "receiving_yards",
      "receiving_air_yards",
      "receiving_yards_after_catch",
      "receptions",
      "targets",
      "receiving_tds",
      "receiving_fumbles",
      "receiving_fumbles_lost",
      "receiving_first_downs",
      "receiving_epa",
      "racr",
      "target_share",
      "air_yards_share",
      "wopr"
    )
  
  rec_two_points <- two_points %>%
    dplyr::filter(.data$pass_attempt == 1) %>%
    dplyr::group_by(.data$receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarise(
      # need name_receiver and team_receiver here for the full join in the next pipe
      name_receiver = custom_mode(.data$receiver_player_name),
      team_receiver = custom_mode(.data$posteam),
      receiving_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(player_id = .data$receiver_player_id) %>%
    dplyr::ungroup()
  
  rec_df <- rec_df %>%
    # need a full join because players without receiving stats that recorded
    # a receiving two point are dropped in any other join
    dplyr::full_join(
      rec_two_points,
      by = c(
        "player_id",
        "week",
        "season",
        "name_receiver",
        "team_receiver"
      )
    ) %>%
    dplyr::mutate(
      receiving_2pt_conversions = dplyr::if_else(
        is.na(.data$receiving_2pt_conversions),
        0L,
        .data$receiving_2pt_conversions
      )
    ) %>%
    dplyr::filter(!is.na(.data$player_id))
  
  rec_df_nas <- is.na(rec_df)
  epa_index <-
    which(
      dimnames(rec_df_nas)[[2]] == c(
        "receiving_epa",
        "racr",
        "target_share",
        "air_yards_share",
        "wopr"
      )
    )
  rec_df_nas[, epa_index] <- c(FALSE)
  
  rec_df[rec_df_nas] <- 0
  
  
  # Special Teams -----------------------------------------------------------
  
  st_tds <- pbp %>%
    dplyr::filter(.data$special == 1 &
                    !is.na(.data$td_player_id)) %>%
    dplyr::group_by(.data$td_player_id, .data$week, .data$season) %>%
    dplyr::summarise(
      name_st = custom_mode(.data$td_player_name),
      team_st = custom_mode(.data$td_team),
      special_teams_tds = sum(.data$touchdown, na.rm = TRUE)
    ) %>%
    dplyr::rename(player_id = .data$td_player_id)
  
  
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

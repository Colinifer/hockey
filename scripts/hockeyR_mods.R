
# Player stats ------------------------------------------------------------

calculate_player_stats_mod <- function(pbp) {
  
  pbp_season <- pbp %>% 
    pull(season) %>% 
    unique()
  
  rosters <- nhlapi::nhl_teams_rosters() %>% 
    unnest(roster.roster) %>% 
    as_tibble() %>% 
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
  
  sc.main_events <- c('Goal', 'Shot', 'Missed Shot', 'Blocked Shot', 'Hit', 'Giveaway', 'Takeaway', 'Faceoff', 'Penalty')
  
  st.shot_events <-     c('Shot',  'Goal')
  st.fenwick_events <-  c('Shot', 'Goal', 'Missed Shot')
  st.corsi_events <-    c('Shot', 'Goal', 'Missed Shot', 'Blocked Shot')
  st.strength_states <- c('3v3', '5v5', '4v4', '5v4', '4v5', '5v3', '3v5', 
                          '4v3', '3v4', '5v6', '6v5', '4v6', '6v4', '3v6', 
                          '6v3')
  st.even_strength <-   c('5v5', '4v4', '3v3')
  st.pp_strength <-     c('5v4', '4v5', '5v3', '3v5', '4v3', '3v4')
  st.empty_net <-       c('5v6', '6v5', '4v6', '6v4', '3v6', '6v3')
  
  # Prepare data ------------------------------------------------------------
  
  # filter down to the 2 dfs we need
  
  # Count all event_player_1 events
  main_events_data <- pbp |> 
    dplyr::arrange(
      game_id, 
      event_idx) |> 
    dplyr::filter(event %in% sc.main_events) |> 
    dplyr::select(
      game_id,
      event_idx,
      event,
      player_name = event_player_1_name,
      player_id = event_player_1_id,
      player_type = event_player_1_type,
      event_team_abbr
    ) |> 
    dplyr::group_by(
      event, 
      player_id,
      player_name
      ) |> 
    dplyr::count() |> 
    dplyr::mutate(
      event = dplyr::case_when(
        event == 'Blocked Shot' ~ 'Blocked Shots',
        event == 'Faceoff' ~ 'Faceoffs Won',
        event == 'Hit' ~ 'Hits',
        event == 'Penalty' ~ 'Penalties Taken',
        TRUE ~ event
      )
    ) |> 
    tidyr::spread(key = event, value = n) |> 
    janitor::clean_names()
  
  # Count all event_player_2 events
  secondary_events_data <- pbp |> 
    dplyr::arrange(
      game_id, 
      event_idx) |> 
    dplyr::filter(event %in% sc.main_events & 
             !is.na(event_player_2_id) &
             !(event %in% c(st.shot_events, 'Takeaway', 'Giveaway', 'Missed Shot'))) |> 
    dplyr::select(
      game_id,
      event_idx,
      event,
      player_name = event_player_2_name,
      player_id = event_player_2_id,
      player_type = event_player_2_type,
      event_team_abbr
    ) |> 
    dplyr::group_by(
      event, 
      player_id,
      player_name
    ) |> 
    dplyr::count() |> 
    dplyr::mutate(
      event = dplyr::case_when(
        event == 'Blocked Shot' ~ 'Shots Blocked',
        event == 'Faceoff' ~ 'Faceoffs Lost',
        event == 'Hit' ~ 'Hits Taken',
        event == 'Penalty' ~ 'Penalties Drawn',
        TRUE ~ event
      )
    ) |> 
    tidyr::spread(key = event, value = n) |> 
    janitor::clean_names()
  
  all_events_data <- main_events_data |> 
    dplyr::left_join(secondary_events_data, by = c('player_id', 'player_name'))
  
  # Get data seasons
  data_seasons <- pbp %>% pull(season) %>% unique()
  
  # On Ice stats ----------------------------------------------------------
  
  
  
  # Penalty stats ---------------------------------------------------------
  
  # rush df 1: primary rusher
  rushes <- data %>%
    dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
    dplyr::group_by(.data$rusher_player_id, .data$week, .data$season) %>% 
    dplyr::mutate(
      hvt = ifelse(yardline_100 <= 10, 1, 0)
    ) %>% 
    dplyr::summarize(
      name_rush = dplyr::first(.data$rusher_player_name),
      team_rush = dplyr::first(.data$posteam),
      yards = sum(.data$rushing_yards, na.rm = TRUE),
      tds = sum(.data$td_player_id == .data$rusher_player_id, na.rm = TRUE),
      carries = dplyr::n(),
      rushing_fumbles = sum(.data$fumble == 1 & .data$fumbled_1_player_id == .data$rusher_player_id & is.na(.data$lateral_rusher_player_id)),
      rushing_fumbles_lost = sum(.data$fumble_lost == 1 & .data$fumbled_1_player_id == .data$rusher_player_id & is.na(.data$lateral_rusher_player_id)),
      rushing_first_downs = sum(.data$first_down_rush & is.na(.data$lateral_rusher_player_id)),
      rushing_epa = sum(.data$epa, na.rm = TRUE),
      hvt = sum(.data$hvt)
    ) %>%
    dplyr::ungroup()
  
  # rush df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_rusher_player_id)) %>%
    dplyr::group_by(.data$lateral_rusher_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      lateral_yards = sum(.data$lateral_rushing_yards, na.rm = TRUE),
      lateral_fds = sum(.data$first_down_rush, na.rm = TRUE),
      lateral_tds = sum(.data$td_player_id == .data$lateral_rusher_player_id, na.rm = TRUE),
      lateral_att = dplyr::n(),
      lateral_fumbles =  sum(.data$fumble, na.rm = TRUE),
      lateral_fumbles_lost = sum(.data$fumble_lost, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(rusher_player_id = .data$lateral_rusher_player_id) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          .data$type == "lateral_rushing" & .data$season %in% data$season & .data$week %in% data$week
        ) %>%
        dplyr::select("season", "week", "rusher_player_id" = .data$gsis_player_id, "lateral_yards" = .data$yards) %>%
        dplyr::mutate(lateral_tds = 0L, lateral_att = 1L)
    )
  
  # rush df: join
  rush_df <- rushes %>%
    dplyr::left_join(laterals, by = c("rusher_player_id", "week", "season")) %>%
    dplyr::mutate(
      lateral_yards = dplyr::if_else(is.na(.data$lateral_yards), 0, .data$lateral_yards),
      lateral_tds = dplyr::if_else(is.na(.data$lateral_tds), 0L, .data$lateral_tds),
      lateral_fumbles = dplyr::if_else(is.na(.data$lateral_fumbles), 0, .data$lateral_fumbles),
      lateral_fumbles_lost = dplyr::if_else(is.na(.data$lateral_fumbles_lost), 0, .data$lateral_fumbles_lost),
      lateral_fds = dplyr::if_else(is.na(.data$lateral_fds), 0, .data$lateral_fds)
    ) %>%
    dplyr::mutate(
      rushing_yards = .data$yards + .data$lateral_yards,
      rushing_tds = .data$tds + .data$lateral_tds,
      rushing_first_downs = .data$rushing_first_downs + .data$lateral_fds,
      rushing_fumbles = .data$rushing_fumbles + .data$lateral_fumbles,
      rushing_fumbles_lost = .data$rushing_fumbles_lost + .data$lateral_fumbles_lost,
      hvt_percentage = hvt / carries
    ) %>%
    dplyr::rename(player_id = .data$rusher_player_id) %>%
    dplyr::select("player_id", "week", "season", "name_rush", "team_rush",
                  "rushing_yards", "carries", "rushing_tds", "rushing_fumbles",
                  "rushing_fumbles_lost", "rushing_first_downs", "rushing_epa", "hvt") %>%
    dplyr::ungroup()
  
  rush_two_points <- two_points %>%
    dplyr::filter(.data$rush_attempt == 1) %>%
    dplyr::group_by(.data$rusher_player_id, .data$week, .data$season) %>%
    dplyr::summarise(
      # need name_rush and team_rush here for the full join in the next pipe
      name_rush = custom_mode(.data$rusher_player_name),
      team_rush = custom_mode(.data$posteam),
      rushing_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(player_id = .data$rusher_player_id) %>%
    dplyr::ungroup()
  
  rush_df <- rush_df %>%
    # need a full join because players without rushing stats that recorded
    # a rushing two point (mostly QBs) are dropped in any other join
    dplyr::full_join(rush_two_points, by = c("player_id", "week", "season", "name_rush", "team_rush")) %>%
    dplyr::mutate(rushing_2pt_conversions = dplyr::if_else(is.na(.data$rushing_2pt_conversions), 0L, .data$rushing_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$player_id))
  
  rush_df_nas <- is.na(rush_df)
  epa_index <- which(dimnames(rush_df_nas)[[2]] == "rushing_epa")
  rush_df_nas[,epa_index] <- c(FALSE)
  
  rush_df[rush_df_nas] <- 0
  
  # Receiving stats ---------------------------------------------------------
  
  # receiver df 1: primary receiver
  rec <- data %>%
    dplyr::filter(!is.na(.data$receiver_player_id)) %>% 
    dplyr::mutate(
      hvt = ifelse(yardline_100 <= 10, 1, 0)
    ) %>% 
    dplyr::group_by(.data$receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      name_receiver = dplyr::first(.data$receiver_player_name),
      team_receiver = dplyr::first(.data$posteam),
      yards = sum(.data$receiving_yards, na.rm = TRUE),
      receptions = sum(.data$complete_pass == 1),
      targets = dplyr::n(),
      tds = sum(.data$td_player_id == .data$receiver_player_id, na.rm = TRUE),
      receiving_fumbles = sum(.data$fumble == 1 & .data$fumbled_1_player_id == .data$receiver_player_id & is.na(.data$lateral_receiver_player_id)),
      receiving_fumbles_lost = sum(.data$fumble_lost == 1 & .data$fumbled_1_player_id == .data$receiver_player_id & is.na(.data$lateral_receiver_player_id)),
      receiving_air_yards = sum(.data$air_yards, na.rm = TRUE),
      receiving_yards_after_catch = sum(.data$yards_after_catch, na.rm = TRUE),
      receiving_first_downs = sum(.data$first_down_pass & is.na(.data$lateral_receiver_player_id)),
      receiving_epa = sum(.data$epa, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # receiver df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_receiver_player_id)) %>%
    dplyr::mutate(
      hvt = ifelse(yardline_100 <= 10, 1, 0)
    ) %>% 
    dplyr::group_by(.data$lateral_receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      lateral_yards = sum(.data$lateral_receiving_yards, na.rm = TRUE),
      lateral_tds = sum(.data$td_player_id == .data$lateral_receiver_player_id, na.rm = TRUE),
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
          .data$type == "lateral_receiving" & .data$season %in% data$season & .data$week %in% data$week
        ) %>%
        dplyr::select("season", "week", "receiver_player_id" = .data$gsis_player_id, "lateral_yards" = .data$yards) %>%
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
      lateral_fumbles_lost = dplyr::if_else(is.na(.data$lateral_fumbles_lost), 0, .data$lateral_fumbles_lost),
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
        .data$receiving_air_yards < 0 & !.data$receiver_player_id %in% racr_ids$gsis_id ~ 0,
        TRUE ~ .data$racr
      ),
      target_share = .data$targets / .data$team_targets,
      air_yards_share = .data$receiving_air_yards / .data$team_air_yards,
      wopr = 1.5 * .data$target_share + 0.7 * .data$air_yards_share
    ) %>%
    dplyr::rename(player_id = .data$receiver_player_id) %>%
    dplyr::select("player_id", "week", "season", "name_receiver", "team_receiver",
                  "receiving_yards", "receiving_air_yards", "receiving_yards_after_catch",
                  "receptions", "targets", "receiving_tds", "receiving_fumbles",
                  "receiving_fumbles_lost", "receiving_first_downs", "receiving_epa",
                  "racr", "target_share", "air_yards_share", "wopr")
  
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
    dplyr::full_join(rec_two_points, by = c("player_id", "week", "season", "name_receiver", "team_receiver")) %>%
    dplyr::mutate(receiving_2pt_conversions = dplyr::if_else(is.na(.data$receiving_2pt_conversions), 0L, .data$receiving_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$player_id))
  
  rec_df_nas <- is.na(rec_df)
  epa_index <- which(dimnames(rec_df_nas)[[2]] == c("receiving_epa", "racr", "target_share", "air_yards_share", "wopr"))
  rec_df_nas[,epa_index] <- c(FALSE)
  
  rec_df[rec_df_nas] <- 0
  
  
  # Special Teams -----------------------------------------------------------
  
  st_tds <- pbp %>%
    dplyr::filter(.data$special == 1 & !is.na(.data$td_player_id)) %>%
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
      left_join(roster_df %>%
                  select(gsis_id, pfr_id, first_name, last_name),
                by = c('pfr_player_id' = 'pfr_id')) %>% 
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
      mutate(player_id = as.character(NA),
             week = as.integer(NA),
             season = as.integer(NA),
             name_snaps = as.character(NA))
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
        !is.na(.data$name_pass) ~ .data$name_pass,
        !is.na(.data$name_rush) ~ .data$name_rush,
        !is.na(.data$name_receiver) ~ .data$name_receiver,
        !is.na(.data$name_snaps) ~ .data$name_snaps,
        TRUE ~ .data$name_st
      ),
      recent_team = dplyr::case_when(
        !is.na(.data$team_pass) ~ .data$team_pass,
        !is.na(.data$team_rush) ~ .data$team_rush,
        !is.na(.data$team_receiver) ~ .data$team_receiver,
        !is.na(.data$team_snaps) ~ .data$team_snaps,
        TRUE ~ .data$team_st
      )
    ) %>% 
    dplyr::left_join(data %>% select(season, game_id, week, recent_team = posteam) %>% unique(), by = c('season', 'week', 'recent_team')) %>% 
    dplyr::select(tidyselect::any_of(c(
      
      # id information
      "player_id", "player_name", "recent_team", "season", "week", "game_id", "season_type",
      
      # offense stats
      "offense_snaps", "offense_pct",
      
      # passing stats
      "completions", "attempts", "passing_yards", "passing_tds", "interceptions",
      "sacks", "sack_yards", "sack_fumbles", "sack_fumbles_lost", "passing_air_yards", "passing_yards_after_catch",
      "passing_first_downs", "passing_epa", "passing_2pt_conversions", "pacr", "anya", "dakota",
      
      # rushing stats
      "carries", "rushing_yards", "rushing_tds", "rushing_fumbles", "rushing_fumbles_lost",
      "rushing_first_downs", "rushing_epa", "rushing_2pt_conversions", "hvt",
      
      # receiving stats
      "receptions", "targets", "receiving_yards", "receiving_tds", "receiving_fumbles",
      "receiving_fumbles_lost", "receiving_air_yards", "receiving_yards_after_catch",
      "receiving_first_downs", "receiving_epa", "receiving_2pt_conversions", "racr",
      "target_share", "air_yards_share", "wopr", "hvt",
      
      # special teams
      "special_teams_tds", "st_snaps", "st_pct"
      
    ))) %>%
    dplyr::filter(!is.na(.data$player_id))
  
  player_df_nas <- is.na(player_df)
  epa_index <- which(dimnames(player_df_nas)[[2]] %in% c("season_type", "game_id", "week", "passing_epa", "rushing_epa", "receiving_epa", "dakota", "racr", "target_share", "air_yards_share", "wopr", "pacr"))
  player_df_nas[,epa_index] <- c(FALSE)
  
  player_df[player_df_nas] <- 0
  
  player_df <- player_df %>%
    dplyr::mutate(
      fantasy_points =
        1 / 25 * .data$passing_yards +
        4 * .data$passing_tds +
        -2 * .data$interceptions +
        1 / 10 * (.data$rushing_yards + .data$receiving_yards) +
        6 * (.data$rushing_tds + .data$receiving_tds + .data$special_teams_tds) +
        2 * (.data$passing_2pt_conversions + .data$rushing_2pt_conversions + .data$receiving_2pt_conversions) +
        -2 * (.data$sack_fumbles_lost + .data$rushing_fumbles_lost + .data$receiving_fumbles_lost),
      
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
        offense_pct = sum(.data$offense_snaps) / sum(.data$potential_offense_snaps), # calculate total snaps percentage
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
        passing_epa = dplyr::if_else(all(is.na(.data$passing_epa)), NA_real_, sum(.data$passing_epa, na.rm = TRUE)),
        passing_2pt_conversions = sum(.data$passing_2pt_conversions),
        pacr = .data$passing_yards / .data$passing_air_yards,
        anya = (passing_yards - sack_yards + (20 * passing_tds) - (45 * interceptions)) / (attempts + sacks),
        
        # rushing
        carries = sum(.data$carries),
        rushing_yards = sum(.data$rushing_yards),
        rushing_tds = sum(.data$rushing_tds),
        rushing_fumbles = sum(.data$rushing_fumbles),
        rushing_fumbles_lost = sum(.data$rushing_fumbles_lost),
        rushing_first_downs = sum(.data$rushing_first_downs),
        rushing_epa = dplyr::if_else(all(is.na(.data$rushing_epa)), NA_real_, sum(.data$rushing_epa, na.rm = TRUE)),
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
        receiving_epa = dplyr::if_else(all(is.na(.data$receiving_epa)), NA_real_, sum(.data$receiving_epa, na.rm = TRUE)),
        receiving_2pt_conversions = sum(.data$receiving_2pt_conversions),
        racr = .data$receiving_yards / .data$receiving_air_yards,
        target_share = dplyr::if_else(all(is.na(.data$target_share)), NA_real_, mean(.data$target_share, na.rm = TRUE)),
        air_yards_share = dplyr::if_else(all(is.na(.data$air_yards_share)), NA_real_, mean(.data$air_yards_share, na.rm = TRUE)),
        wopr = 1.5 * .data$target_share + 0.7 * .data$air_yards_share,
        
        # special teams
        special_teams_tds = sum(.data$special_teams_tds),
        st_snaps = sum(.data$st_snaps),
        # potential_st_snaps = sum(.data$potential_st_snaps), # total potential_snaps
        st_pct = sum(.data$st_snaps) / sum(.data$potential_st_snaps), # calculate total snaps percentage
        
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
          .data$receiving_air_yards < 0 & !.data$player_id %in% racr_ids$gsis_id ~ 0,
          TRUE ~ .data$racr
        ),
        pacr = dplyr::case_when(
          is.nan(.data$pacr) ~ NA_real_,
          .data$passing_air_yards <= 0 ~ 0,
          TRUE ~ .data$pacr
        )
      ) %>% 
      add_dakota(pbp = pbp, weekly = weekly) %>%
      dplyr::select(
        .data$player_id:.data$pacr,
        .data$dakota,
        dplyr::everything()
      )
  }
  
  return(player_df)
}

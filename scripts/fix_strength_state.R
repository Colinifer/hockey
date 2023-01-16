fix_strength_states <- function(pbp) {
  
  pbp_clean <- pbp |>
    arrange(game_id, event_idx) |> 
    mutate(
      secondary_type = case_when(
        event == 'Change' & 
          game_seconds %% 1200 == 0 & 
          period_seconds == 0 ~ 'Line Change',
        TRUE ~ secondary_type
      ),
      strength_state = case_when(
        event == 'Change' & 
          period_seconds == 0 & 
          lead(event) == 'Faceoff' ~ lead(strength_state),
        event == 'Change' & 
          period_seconds == 0 & 
          lead(event, 2) == 'Faceoff' ~ lead(strength_state, 2),
        # End of game
        # Need to make sure the event is a change, period end is the next event, and no stoppage in previous event
        # Same event_team
        event == 'Change' & 
          lead(event, 2) == 'Period End' &
          lag(event) != 'Stoppage' &
          event_team == lag(event_team) ~ lag(strength_state),
        event == 'Change' & 
          lead(event) == 'Period End' & 
          lag(event, 2) != 'Stoppage' &
          event_team == lag(event_team, 2) ~ lag(strength_state, 2),
        # Different event_team
        event == 'Change' & 
          lead(event, 2) == 'Period End' &
          lag(event) != 'Stoppage' &
          event_team != lag(event_team) ~ paste0(substr(lag(strength_state), 3, 3),'v',substr(lag(strength_state), 1, 1)),
        event == 'Change' & 
          lead(event) == 'Period End' & 
          lag(event, 2) != 'Stoppage' &
          event_team != lag(event_team, 2) ~ paste0(substr(lag(strength_state, 2), 3, 3),'v',substr(lag(strength_state, 2), 1, 1)),
        
        # If stoppage
        event == 'Change' & 
          lead(event, 2) == 'Period End' &
          lag(event) == 'Stoppage' &
          event_team == lag(event_team, 2) ~ lag(strength_state, 2),
        event == 'Change' & 
          lead(event) == 'Period End' & 
          lag(event, 2) == 'Stoppage' &
          event_team == lag(event_team, 3) ~ lag(strength_state, 3),
        # Different event_team
        event == 'Change' & 
          lead(event, 2) == 'Period End' &
          lag(event) == 'Stoppage' &
          event_team != lag(event_team, 2) ~ paste0(substr(lag(strength_state, 2), 3, 3),'v',substr(lag(strength_state, 2), 1, 1)),
        event == 'Change' & 
          lead(event) == 'Period End' & 
          lag(event, 2) == 'Stoppage' &
          event_team != lag(event_team, 3) ~ paste0(substr(lag(strength_state, 3), 3, 3),'v',substr(lag(strength_state, 3), 1, 1)),
        
        # event == 'Change' & 
        #   game_seconds %% 1200 == 0 & 
        #   period_seconds == 0 &
        #   substr(strength_state, 3, 3) == '0' ~ paste0(substr(lead(strength_state), 3, 3),
        #                                                substr(lead(strength_state), 2, 2),
        #                                                substr(lead(strength_state), 1, 1)),
        # event == 'Change' &
        #   ((game_seconds %% 1200 == 0 & 
        #       period_seconds == 1200) | 
        #      period > 3) & 
        #   substr(strength_state, 3, 3) == '0' ~ paste0(substr(lag(strength_state), 3, 3), 
        #                                                substr(strength_state, 2, 2), 
        #                                                num_off - 1),
        # event == 'Change' & 
        #   ((game_seconds %% 1200 == 0 & 
        #       period_seconds == 1200) | 
        #      period > 3) & 
        #   substr(strength_state, 1, 1) == '0' ~ paste0(num_off - 1, 
        #                                                substr(strength_state, 2, 3)),
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
    ) # |> filter(grepl('0', strength_state) == TRUE & !(event %in% c('Game Scheduled', 'Period End', 'Game End'))) |> select(game_id, event_idx, event, secondary_type, period, period_seconds, game_seconds, strength_state, description)
  
  return(pbp_clean)
  
}

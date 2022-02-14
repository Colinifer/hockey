sc.main_events <- c("GOAL", "SHOT", "MISS", "BLOCK", "HIT", "GIVE", "TAKE", "FAC", "PENL")

Team_ID_vec <- c(
  "ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY",
  "CHI", "COL", "DAL", "DET", "EDM", "FLA", "L.A", "MIN",
  "MTL", "N.J", "NSH", "NYI", "NYR", "OTT", "PHI",
  "PIT", "S.J", "STL", "T.B", "TOR", "VAN", "WPG", "WSH",
  "PHX", "ATL", "VGK", "L.V"
)

sc.scrape_events_HTM <- function(game_id_fun, season_id_fun, attempts = 3) {

  url_events_HTM <- NULL
  try_count <-  attempts

  while (!is.character(url_events_HTM) & try_count > 0) {

    url_events_HTM <- try(
      RCurl::getURL(
        paste0(
          "http://www.nhl.com/scores/htmlreports/",
          as.character(season_id_fun),
          "/PL0",
          as.character(substr(game_id_fun, 6, 10)),
          ".HTM"
        )
      )
    )

    try_count <- try_count - 1

  }

  ## Pull out events data
  events_body_text <- rvest::html_text(
    rvest::html_nodes(
      xml2::read_html(url_events_HTM),
      ".bborder"
    )
  )

}

events_HTM <- sc.scrape_events_HTM(
  game_id_fun = game_id,
  season_id_fun = season_id,
  attempts = 3
)

events_data %>%
  matrix(
    byrow = TRUE,
    ncol = 8
  ) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  select(
    eventIdx =          X1,
    game_period =       X2,
    strength =          X3,
    times =             X4,
    event_type =        X5,
    event_description = X6,
    away_skaters =      X7,
    home_skaters =      X8
  ) %>%
  filter(game_period != "Per") %>%
  mutate(
    eventIdx =     as.numeric(eventIdx),
    game_id =      as.character(game_id_fun),
    game_period =  as.numeric(game_period),
    ## Extract shot types, times, penalty type, and challenge types
    event_detail = case_when(
      event_type %in% c("SHOT", "MISS", "BLOCK", "GOAL") ~ str_extract(event_description, ",\\s*[a-zA-Z|-]+,") %>% gsub(",|\\s", "", .),
      event_type %in% c("PSTR", "PEND", "SOC", "GEND") ~   str_extract(event_description, "[0-9]+:[0-9]+\\s*[A-Z]+"),
      event_type == "PENL" ~ str_extract(event_description, "[(][0-9]+\\s[a-z]*[)]") %>% gsub("[(]|[)]|\\s*", "", .),
      event_type == "CHL" ~  str_extract(event_description, "-[a-zA-Z\\s]+-") %>% gsub("\\s*-|-\\s*", "", .)
    ),
    ## Fix period & game end recording errors (rare)
    times = case_when(
      event_type == "PEND" & game_period == 4 & max(game_period) > 4 & times == "-16:0-120:00" ~ "5:000:00",
      event_type %in% c("PEND", "GEND") & game_period == 4 & max(game_period) == 4 & times == "-16:0-120:00" ~ last(na.omit(times)),
      TRUE ~ times
    ),
    time_elapsed = str_extract(times, "[0-9]+:[0-9]{2}"),
    game_seconds = suppressWarnings(period_to_seconds(ms(time_elapsed))),
    game_seconds = (game_period * 1200) + game_seconds
  ) %>%
  group_by(eventIdx) %>%
  mutate(
    event_description = gsub("\\bPHX\\b", "ARI", event_description),   ## change PHX to ARI in event description

    event_team = ifelse(event_type %in% c(sc.main_events, "CHL", "DELPEN"), str_extract(event_description, "^[A-Z]\\.[A-Z]|^[A-Z]+"), NA),
    event_team = ifelse(!event_team %in% Team_ID_vec, NA, event_team),  ## ensure event_team extacted is an actual team

    event_player_1 = case_when(
      event_type %in% c("GOAL", "HIT", "MISS", "BLOCK", "FAC") ~
        str_extract(event_description, "([A-Z]+\\.[A-Z]+|[A-Z]+)\\s*#[0-9]+"),

      event_type %in% c("SHOT", "GIVE", "TAKE") ~
        gsub("ONGOAL\\s*-\\s*|GIVEAWAY\\s*-\\s*|TAKEAWAY\\s*-\\s*", "", event_description) %>% str_extract(., "([A-Z]+\\.[A-Z]+|[A-Z]+)\\s*(#[0-9]+|[0-9]+)"),

      event_type == "PENL" & !grepl("TEAM", event_description) ~                                       ## normal penalites
        str_extract(event_description, "([A-Z]+\\.[A-Z]+|[A-Z]+)\\s*#[0-9]+"),

      event_type == "PENL" & grepl("TEAM", event_description) & grepl("#[0-9]+", event_description) ~  ## bench minors (delay of game, faceoff violation, etc.)
        paste(event_team, str_extract(event_description, "#[0-9]+"))
    ),
    event_player_2 = case_when(
      event_type %in% c("BLOCK", "FAC", "HIT", "PENL") & length(str_extract_all(event_description, "#[0-9]+")[[1]]) > 1 ~  ## ensure a player is present
        str_extract_all(event_description, "([A-Z]+\\.[A-Z]+|[A-Z]+)\\s*#[0-9]+")[[1]][2],

      event_type == "GOAL" & length(str_extract_all(event_description, "#[0-9]+")[[1]]) > 1 ~  ## ensure a player is present
        paste(event_team, str_extract_all(event_description, "#[0-9]+")[[1]][2])
    ),
    event_player_3 = case_when(
      event_type == "GOAL" & length(str_extract_all(event_description, "#[0-9]+")[[1]]) > 2 ~  ## ensure a player is present
        paste(event_team, str_extract_all(event_description, "#[0-9]+")[[1]][3])
    ),

    event_zone = str_extract(event_description, "[a-zA-Z]{3}\\.\\s*[zZ]one") %>% gsub("\\.\\s*[zZ]one", "", .)
  ) %>%
  ungroup() %>%
  mutate_at(
    vars(event_player_1:event_player_3),
    list(~gsub("#|\\s*", "", .))
  ) %>%
  mutate_at(
    vars(event_player_1:event_player_3),  ## remove event players for true team/bench penalties
    list(~ifelse(grepl("[tT]oo\\s*many\\s*men|[A-Z]+\\s*[A-Z]+\\s*[bB]ench[(]", event_description), NA, .))
  ) %>%
  select(-c(home_skaters, away_skaters)) %>%
  as_tibble()

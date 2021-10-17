# Most of this package assumes my private R package initR is pre-installed
# and an existing PostgreSQL environment is running on your local
# machine and/or network.


# Packages & Init Setup ---------------------------------------------------

proj_name <- 'hockey'
# devtools::install_github('Colinifer/initR', auth_token = Sys.getenv('authtoken'))
# devtools::install_github('bbc/bbplot')
# devtools::install_github('war-on-ice/nhlplot')

pkgs <- c(
  'devtools',
  'tidyverse',
  'RPostgres',
  'RPostgreSQL',
  # 'RMariaDB', # deprecated, new environment is running PostgreSQL
  'DBI',
  'readr',
  'pander',
  'na.tools',
  'devtools',
  'teamcolors',
  'glue',
  'dplyr',
  'rvest',
  'arrow',
  'RCurl',
  'tictoc',
  'animation',
  'gt',
  'DT',
  'ggimage',
  'ggpubr',
  'ggthemes',
  'bbplot',
  'ggtext',
  'ggforce',
  'ggridges',
  'ggrepel',
  'hexbin',
  'ggbeeswarm',
  'extrafont',
  'RCurl',
  'xml2',
  'rvest',
  'jsonlite',
  'foreach',
  'lubridate',
  'snakecase',
  'Matrix',
  'nhlapi',
  'sportyR',
  'hockeyR',
  'nhlplot',
  'initR'
)
installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
if (any(c('bbplot', 'initR') %in%
        rownames(installed.packages()) == FALSE)) {
  library(devtools)
  devtools::install_github('bbc/bbplot')
  devtools::install_github('Colinifer/initR', auth_token = Sys.getenv('authtoken'))
}
# Load all installed packages, install and load any uninstalled packages
invisible(lapply(pkgs, function(x) {
  suppressMessages(suppressWarnings(library(
    x,
    warn.conflicts = FALSE,
    quietly = TRUE,
    character.only = TRUE
  )))
}))
rm(pkgs, installed_packages)

# Initialize working directory --------------------------------------------

fx.setdir(proj_name)

# Create standard objects -------------------------------------------------

con <- initR::fx.db_con(x.host = 'localhost') # connect to DB using personal initR functions
dbListTables(con)
map(.x=dbListTables(con), ~tbl(con, .x))
dbDisconnect(con)

'%notin%' <- Negate('%in%') # opposite of %in%
options(tibble.print_min=10) # expand default tibble preview

current_season <- 2021
current_full_season <- glue('{current_season}{current_season+1}')
year <- substr(Sys.Date(), 1, 4)
date <- Sys.Date()

today <- format(Sys.Date(), '%Y-%d-%m')

# f.scrape <- paste0('data/', list.files(path = 'data/', pattern = 'pbp_scrape'))

# Open backup parquet files for fast offline viewing
# schedule_ds <- open_dataset('data/schedule/', partitioning = 'year')
# game_info_ds <- open_dataset('data/game_info/', partitioning = 'year')
# pbp_base_ds <- open_dataset('data/pbp_base/', partitioning = 'year')
# pbp_extras_ds <- open_dataset('data/pbp_extras', partitioning = 'year')
# player_shifts_ds <- open_dataset('data/player_shifts', partitioning = 'year')
# player_periods_ds <- open_dataset('data/player_periods', partitioning = 'year')
# roster_ds <- open_dataset('data/roster/', partitioning = 'year')
# scratches_ds <- open_dataset('data/scratches/', partitioning = 'year')
# events_summary_ds <- open_dataset('data/events_summary/', partitioning = 'year')
# report_ds <- open_dataset('data/report/', partitioning = 'year')
# mp_games_ds <- open_dataset('data/moneypuck/games/', partitioning = 'year')
# mp_players_ds <- open_dataset('data/moneypuck/players/', partitioning = 'year')
# nst_ds <- open_dataset('data/nst/', partitioning = 'year')

# Source other files with various functions and ggproto objects
source('plots/assets/plot_theme.R', echo = F)
source('scripts/EH_scrape_functions.R', echo = F)
source('scripts/m1_update_db.R', echo = F)
source('scripts/m1_scrape_sources.R', echo = F)
# source('scripts/all_functions.R')
# source('scripts/all_stats.R')
# source('scripts/misc_functions.R')

# Get latest rosters
active_players <- nhlapi::nhl_teams_rosters() %>% 
  unnest(roster.roster) %>% 
  as_tibble() %>% 
  janitor::clean_names()

# Load roster db and join with latest roster scrape
con <- fx.db_con(x.host = 'localhost')
roster_df <- tbl(con, 'roster') %>% 
  filter(season == current_full_season) %>% 
  select(-game_id,
         -game_date,
         -opponent,
         -is_home) %>% 
  collect() %>% 
  unique() %>% 
  as_tibble() %>% 
  rename(
    # player,
    first_name = firstname,
    last_name = lastname,
    jersey_number = player_num
  ) %>% 
  mutate(
    full_name = glue('{first_name} {last_name}')
  ) %>% 
  left_join(
    active_players %>% 
      select(
        team_name,
        full_name = person_full_name,
        jersey_number,
        player_id = person_id,
        NULL
      ) %>% 
      mutate(
        full_name = full_name %>% toupper(),
        jersey_number = jersey_number %>% as.double()
      ),
    by = c('full_name',
           'jersey_number')
  ) %>% 
  left_join(
    teamcolors %>% 
      select(
        league,
        team_name = mascot,
        logo
      ) %>% 
      filter(league == 'nhl') %>% 
      select(
        -league
      ),
    by = c('team_name')
  ) %>% 
  unique() %>% 
  mutate(
    headshot_url = glue('https://cms.nhl.bamgrid.com/images/headshots/current/168x168/{player_id}.jpg'),
    action_shot_url = glue('https://cms.nhl.bamgrid.com/images/actionshots/{player_id}.jpg')
  )

dbDisconnect(con)

# Update database ---------------------------------------------------------

con <- fx.db_con(x.host = 'localhost')
pbp_df <- hockeyR::scrape_season(2011)
dbWriteTable(con, 'hockeyR_pbp', value = pbp_df)

pbp_df <- hockeyR::scrape_season(2011)
dbAppendTable(con, 'hockeyR_pbp', value = pbp_df)

# Updates databse with latest games play-by-play from current seasons
map(current_season, annual_nhl_query)
# Updates database with latest data from Moneypuck
map(current_season, fx.scrape_moneypuck)
# Latest data from NaturalStatTrick
map(current_season, fx.scrape_nst)


# Sample links:
# http://www.nhl.com/scores/htmlreports/20192020/RO030113.HTM # Roster
# http://www.nhl.com/scores/htmlreports/20192020/PL030113.HTM # Play by Play
# http://www.nhl.com/scores/htmlreports/20192020/GS020113.HTM # Game Summary
# http://www.nhl.com/scores/htmlreports/20192020/TV030113.HTM # Visitor TOI Shift Report
# http://www.nhl.com/scores/htmlreports/20192020/TH030113.HTM # Home TOI Shift Report
# http://www.nhl.com/scores/htmlreports/20192020/SS030113.HTM # Shot Summary
# http://www.nhl.com/scores/htmlreports/20192020/ES020967.HTM # Event Summary

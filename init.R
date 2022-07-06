# Most of this package assumes my private R package initR is pre-installed
# and an existing PostgreSQL environment is running on your local
# machine and/or network.


# Packages & Init Setup ---------------------------------------------------

proj_name <- 'hockey'
# devtools::install_github('Colinifer/initR', auth_token = Sys.getenv('authtoken'))
# devtools::install_github('bbc/bbplot')
# devtools::install_github('war-on-ice/nhlplot')
# devtools::install_github('danmorse314/hockeyR')

pkgs <- c(
  # Core packages
  'devtools',
  'tidyverse',
  'glue',
  'lubridate',
  'rvest',
  'initR',
  
  # Hockey packages
  'nhlapi',
  'teamcolors',
  'sportyR',
  'hockeyR',
  'nhlplot',
  'elo',
  
  # DB Packages
  'odbc',
  'RPostgres',
  
  # Web packages
  'RCurl',
  
  # Stats packages
  'pracma',
  # 'DescTools',
  'zoo',
  'Matrix',
  'MLmetrics',
  
  # Table packages
  'gt',
  'reactable',
  'png',
  'DT',
  
  # ggplot packages
  'ggthemes',
  'ggimage',
  'ggforce',
  'ggridges',
  'ggrepel',
  'ggpmisc',
  'ggbeeswarm',
  'cowplot',
  'webshot',
  'gridExtra',
  'grid',
  'animation',
  'viridis',
  'ggpubr',
  'bbplot',
  'ggtext',
  'hexbin',
  
  # Font packages
  'extrafont',
  'shadowtext',
  
  # Misc. packages
  'furrr',
  'tidytext',
  'na.tools',
  'tictoc',
  'shiny',
  'qs',
  
  # Unnecessary packages
  'pander',
  'distill',
  'arrow', # incompatible w/ Apple ARM
  'foreach',
  
  NULL
)

initR::fx.load_packages(pkgs) |> 
  suppressMessages()

options(tibble.print_min=25)
`%notin%` <- Negate(`%in%`)

# Initialize working directory --------------------------------------------

fx.setdir(proj_name)

# Create standard objects -------------------------------------------------

con <- initR::fx.db_con(x.host = 'localhost') # connect to DB using personal initR functions
dbListTables(con)
map(.x=dbListTables(con), ~tbl(con, .x))

current_season <- 2021
current_full_season <- glue('{current_season}{current_season+1}')
year <- substr(Sys.Date(), 1, 4)
date <- Sys.Date()

today <- format(Sys.Date(), '%Y-%d-%m')

# Source other files with various functions and ggproto objects
source_files <- c(
  'plots/assets/plot_theme.R',
  'scripts/EH_scrape_functions.R',
  'scripts/m1_update_db.R',
  'scripts/m1_scrape_sources.R',
  'scripts/fix_strength_state.R',
  # 'scripts/all_functions.R',
  # 'scripts/all_stats.R',
  # 'scripts/misc_functions.R',
  NULL
)

map(.x = source_files, ~source(.x, echo = F)) %>% 
  invisible()

# Get latest rosters
active_players <- nhlapi::nhl_teams_rosters() %>% 
  unnest(roster.roster) %>% 
  as_tibble() %>% 
  janitor::clean_names()

# Load roster db and join with latest roster scrape
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


pbp_df <- tbl(con, 'hockeyR_pbp') |> 
  filter(season == current_full_season) |> 
  collect()

dbDisconnect(con)

# Update database ---------------------------------------------------------

# Updates database with latest games play-by-play from current seasons
map(current_season, annual_nhl_query) # EH
map(current_season, fx.hockeyr_update) # hockeyR
map(current_season, fx.upload_nhl_game_shifts) # Shifts (API)
map(current_season, fx.scrape_moneypuck) # Moneypuck
map(current_season, fx.scrape_nst) # NatStatTrick


# Sample links:
# http://www.nhl.com/scores/htmlreports/20192020/RO030113.HTM # Roster
# http://www.nhl.com/scores/htmlreports/20192020/PL030113.HTM # Play by Play
# http://www.nhl.com/scores/htmlreports/20192020/GS020113.HTM # Game Summary
# http://www.nhl.com/scores/htmlreports/20192020/TV030113.HTM # Visitor TOI Shift Report
# http://www.nhl.com/scores/htmlreports/20192020/TH030113.HTM # Home TOI Shift Report
# http://www.nhl.com/scores/htmlreports/20192020/SS030113.HTM # Shot Summary
# http://www.nhl.com/scores/htmlreports/20192020/ES020967.HTM # Event Summary

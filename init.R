# Packages & Init Setup ---------------------------------------------------
proj_name <- 'hockey'
# setwd('~/Documents/dev/hockey')

# devtools::install_github('bbc/bbplot')
# devtools::install_github('war-on-ice/nhlplot')

pkgs <- c(
  'devtools',
  'tidyverse',
  'RMariaDB',
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
  'ggbeeswarm',
  'extrafont',
  'RCurl',
  'xml2',
  'rvest',
  'jsonlite',
  'foreach',
  'lubridate',
  'snakecase',
  'nhlapi',
  'nhlplot',
  'initR'
)
installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
if (any('bbplot' %in%
        rownames(installed.packages()) == FALSE)) {
  library(devtools)
  devtools::install_github('bbc/bbplot')
}
invisible(lapply(pkgs, library, character.only = TRUE))
rm(pkgs, installed_packages)

'%notin%' <- Negate('%in%')

options(tibble.print_min=25)

# Initialize Working Directory --------------------------------------------

fx.setdir(proj_name)

# Create standard objects -------------------------------------------------
if ((
  Sys.Date() %>% lubridate::wday() > 1 & # If day is greater than Sunday
  Sys.Date() %>% lubridate::wday() < 6 & # and day is less than Saturday
  Sys.time() %>% format("%H") %>% as.integer() >= 17 & # and greater than 5PM
  Sys.time() %>% format("%H") %>% as.integer() <= 23 # and less than 12AM
) == TRUE) {
  source("../initR/con.R")
  dbListTables(con)
  dbDisconnect(con)
}

current_season <- 2020
year <- substr(Sys.Date(), 1, 4)
date <- Sys.Date()

today <- format(Sys.Date(), '%Y-%d-%m')
source('plots/assets/plot_theme.R', echo = F)
source('EH_scrape_functions.R')
source('scripts/scrape_sources.R')
# source('functions/add_to_table.R')

f.scrape <- paste0('data/', list.files(path = 'data/', pattern = 'pbp_scrape'))
# f.scrape[3] %>% lapply(fx.add_to_table)

# If scrape isn't caught up
# u.scrape_interval <- 250
# source('playground/addToTable.R')


schedule_ds <- open_dataset('data/schedule/', partitioning = 'year')
game_info_ds <- open_dataset('data/game_info/', partitioning = 'year')
pbp_base_ds <- open_dataset('data/pbp_base/', partitioning = 'year')
pbp_extras_ds <- open_dataset('data/pbp_extras', partitioning = 'year')
player_shifts_ds <- open_dataset('data/player_shifts', partitioning = 'year')
player_periods_ds <- open_dataset('data/player_periods', partitioning = 'year')
roster_ds <- open_dataset('data/roster/', partitioning = 'year')
scratches_ds <- open_dataset('data/scratches/', partitioning = 'year')
events_summary_ds <- open_dataset('data/events_summary/', partitioning = 'year')
report_ds <- open_dataset('data/report/', partitioning = 'year')

active_players <- nhlapi::nhl_teams_rosters() %>% 
  unnest(roster.roster) %>% 
  as_tibble()


roster_df <- roster_ds %>% 
  filter(season == '20202021') %>% 
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
        team_name = teamName,
        full_name = person.fullName,
        jersey_number = jerseyNumber,
        player_id = person.id
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


# cd Documents/dev/GitHub/Cloned/pppontusw-dl-nhltv/
# docker run -v ~/Documents/dev/GitHub/Cloned/pppontusw-dl-nhltv:/home/nhltv/media -it pontusw/nhltv:2.2.3 nhltv --team VGK -u welshfam -p 72Valley186 -t VGK -q 900 -b 1 -d ./games/ --short-debug

# http://www.nhl.com/scores/htmlreports/20192020/RO030113.HTM # Roster
# http://www.nhl.com/scores/htmlreports/20192020/PL030113.HTM # Play by Play
# http://www.nhl.com/scores/htmlreports/20192020/GS020113.HTM # Game Summary
# http://www.nhl.com/scores/htmlreports/20192020/TV030113.HTM # Visitor TOI Shift Report
# http://www.nhl.com/scores/htmlreports/20192020/TH030113.HTM # Home TOI Shift Report
# http://www.nhl.com/scores/htmlreports/20192020/SS030113.HTM # Shot Summary
# http://www.nhl.com/scores/htmlreports/20192020/ES020967.HTM # Event Summary

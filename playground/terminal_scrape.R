# Packages & Init Setup ---------------------------------------------------
setwd("/Users/ColinWelsh/Documents/dev/hockey")
proj_name <- "hockey"

# devtools::install_github('bbc/bbplot')

pkgs <- c(
  "devtools",
  "tidyverse",
  "RMariaDB",
  "DBI",
  "readr",
  "pander",
  "na.tools",
  "ggimage",
  "devtools",
  "teamcolors",
  "glue",
  "dplyr",
  "RCurl",
  "tictoc",
  "animation",
  "gt",
  "DT",
  "ggthemes",
  "bbplot",
  "ggtext",
  "ggforce",
  "ggridges",
  "ggrepel",
  "ggbeeswarm",
  "extrafont",
  "RCurl",
  "xml2",
  "rvest",
  "jsonlite",
  "foreach",
  "lubridate",
  "snakecase",
  "nhlapi"
)
installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
invisible(lapply(pkgs, library, character.only = TRUE))
rm(pkgs, installed_packages)

# Initialize Working Directory --------------------------------------------

source("../initR/init.R")
fx.setdir(proj_name)

# Create standard objects -------------------------------------------------
f.con <- "../initR/con.R"

source(f.con)
dbListTables(con)
dbDisconnect(con)

userYear <- substr(Sys.Date(), 1, 4)
userDate <- Sys.Date()

today <- format(Sys.Date(), "%Y-%d-%m")
source("EH_scrape_functions.R")

# source(f.con)
schedule <- readRDS("data/schedule.rds")

# game_ids1 <- schedule %>% 
#   filter(season > 20092010 & season <= 20112012) %>% 
#   select(game_id) %>% 
#   pull(game_id)
# # game_ids1[1]
# # game_ids1[length(game_ids1)]
# pbp_scrape1 <-
#   sc.scrape_pbp(games = game_ids1)
# saveRDS(pbp_scrape1, file="data/pbp_scrape1.rds")
# rm(game_ids1, pbp_scrape1)

# game_ids2 <- schedule %>% 
#   filter(season > 20112012 & season <= 20132014) %>% 
#   select(game_id) %>% 
#   pull(game_id)
# # game_ids2[1]
# # game_ids2[length(game_ids2)]
# pbp_scrape2 <-
#   sc.scrape_pbp(games = game_ids2) # 300 was last
# saveRDS(pbp_scrape2, file="data/pbp_scrape2.rds")
# rm(game_ids2, pbp_scrape2)

# game_ids3 <- schedule %>%
#   filter(season > 20132014 & season <= 20152016) %>%
#   select(game_id) %>%
#   pull(game_id)
# game_ids3[1]
# game_ids3[length(game_ids3)]
# pbp_scrape3 <-
#   sc.scrape_pbp(games = game_ids3) # 300 was last
# saveRDS(pbp_scrape3, file="data/pbp_scrape3.rds")
# rm(game_ids3, pbp_scrape3)

# game_ids4 <- schedule %>%
#   filter(season > 20152016 & season <= 20172018) %>%
#   select(game_id) %>%
#   pull(game_id)
# game_ids4[1]
# game_ids4[length(game_ids3)]
# pbp_scrape4 <-
#   sc.scrape_pbp(games = game_ids4) # 300 was last
# saveRDS(pbp_scrape4, file="data/pbp_scrape4.rds")
# rm(game_ids4, pbp_scrape4)

game_ids5 <- schedule %>%
  filter(season > 20172018 & season <= 20192020) %>%
  select(game_id) %>%
  pull(game_id)
game_ids5[1]
game_ids5[length(game_ids5)]
pbp_scrape5 <-
  sc.scrape_pbp(games = game_ids5) # 300 was last
saveRDS(pbp_scrape5, file="data/pbp_scrape5.rds")
rm(game_ids5, pbp_scrape5)
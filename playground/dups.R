source(f.con)
pbp_extras <- dbGetQuery(con, "SELECT * FROM `hockey`.`pbp_extras` WHERE ('game_id' >= '20142015');")
pbp_extras_dups <- pbp_extras %>% distinct()
dbSendQuery(con, "DELETE FROM `hockey`.`pbp_extras` WHERE ('season' >= '20142015');")
dbWriteTable(con, value = pbp_extras_dups, name = "pbp_extras", append = TRUE)

pbp_base <- dbGetQuery(con, "SELECT * FROM `hockey`.`pbp_base` WHERE ('season' >= '20142015');")
pbp_base_dups <- pbp_base %>% distinct()
dbSendQuery(con, "DELETE FROM `hockey`.`pbp_base` WHERE ('season' >= '20142015');")
dbWriteTable(con, pbp_base_dups, "pbp_base", append = TRUE)

dbDisconnect(con)

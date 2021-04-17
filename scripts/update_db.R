function (dbdir = ".", dbname = "pbp_db", tblname = "nflfastR_pbp", 
          force_rebuild = FALSE, db_connection = NULL) 
{
  rule_header("Update nflfastR Play-by-Play Database")
  if (!is_installed("DBI") | !is_installed("purrr") | (!is_installed("RSQLite") & 
                                                       is.null(db_connection))) {
    usethis::ui_stop("{my_time()} | Packages {usethis::ui_value('DBI')}, {usethis::ui_value('RSQLite')} and {usethis::ui_value('purrr')} required for database communication. Please install them.")
  }
  if (any(force_rebuild == "NEW")) {
    usethis::ui_stop("{my_time()} | The argument {usethis::ui_value('force_rebuild = NEW')} is only for internal usage!")
  }
  if (!(is.logical(force_rebuild) | is.numeric(force_rebuild))) {
    usethis::ui_stop("{my_time()} | The argument {usethis::ui_value('force_rebuild')} has to be either logical or numeric!")
  }
  if (!dir.exists(dbdir) & is.null(db_connection)) {
    usethis::ui_oops("{my_time()} | Directory {usethis::ui_path(dbdir)} doesn't exist yet. Try creating...")
    dir.create(dbdir)
  }
  if (is.null(db_connection)) {
    connection <- DBI::dbConnect(RSQLite::SQLite(), glue::glue("{dbdir}/{dbname}"))
  }
  else {
    connection <- db_connection
  }
  if (!DBI::dbExistsTable(connection, tblname)) {
    build_db(tblname, connection, rebuild = "NEW")
  }
  else if (DBI::dbExistsTable(connection, tblname) & all(force_rebuild != 
                                                         FALSE)) {
    build_db(tblname, connection, rebuild = force_rebuild)
  }
  user_message("Checking for missing completed games...", 
               "todo")
  completed_games <- load_lees_games() %>% dplyr::filter(.data$season >= 
                                                           1999, !is.na(.data$result), !.data$game_id %in% c("1999_01_BAL_STL", 
                                                                                                             "2000_06_BUF_MIA", "2000_03_SD_KC")) %>% dplyr::arrange(.data$gameday) %>% 
    dplyr::pull(.data$game_id)
  missing <- get_missing_games(completed_games, connection, 
                               tblname)
  if (length(missing) > 16) {
    build_db(tblname, connection, show_message = FALSE, 
             rebuild = as.numeric(unique(stringr::str_sub(missing, 
                                                          1, 4))))
    missing <- get_missing_games(completed_games, connection, 
                                 tblname)
  }
  if (length(missing) > 0) {
    new_pbp <- build_nflfastR_pbp(missing, rules = FALSE)
    if (nrow(new_pbp) == 0) {
      user_message("Raw data of new games are not yet ready. Please try again in about 10 minutes.", 
                   "oops")
    }
    else {
      user_message("Appending new data to database...", 
                   "todo")
      DBI::dbWriteTable(connection, tblname, new_pbp, 
                        append = TRUE)
    }
  }
  message_completed("Database update completed", in_builder = TRUE)
  usethis::ui_info("{my_time()} | Path to your db: {usethis::ui_path(DBI::dbGetInfo(connection)$dbname)}")
  if (is.null(db_connection)) 
    DBI::dbDisconnect(connection)
  rule_footer("DONE")
}

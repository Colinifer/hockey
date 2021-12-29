events_summary <- dbGetQuery(con, "SELECT * FROM 'hockey'.'events_summary' WHERE ('season' == '20192020');")

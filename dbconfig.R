pkgs <- c("DBI", "RMariaDB")

installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}

invisible(lapply(pkgs, library, character.only = TRUE))

# DB Variables
db_user <- 'publiccolin'
db_password <- 'VYgIlmCeNg3wIoLe'
db_name <- 'hockey'
db_table <- '2019schedule'
db_host <- '72.78.233.235' # for local access
db_port <- 3306

mydb <-  DBI::dbConnect(RMariaDB::MariaDB(), user = db_user, password = db_password,
                   dbname = db_name, host = db_host, port = db_port)
s <- paste0("select * from ", db_table)
## rs <- dbSendQuery(mydb, s)
## df <-  fetch(rs, n = -1)
## fetch(res = rs, n = -1)
on.exit(dbDisconnect(mydb))

# Connect to my-db as defined in ~/.my.cnf
con <- dbConnect(RMariaDB::MariaDB(), group = "my-db")


dbListTables(con)
dbWriteTable(con, "mtcars", mtcars)
dbListTables(con)

dbListFields(con, "mtcars")
dbReadTable(con, "mtcars")

# You can fetch all results:
res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
dbFetch(res)
dbClearResult(res)

# Or a chunk at a time
res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
while(!dbHasCompleted(res)){
  chunk <- dbFetch(res, n = 5)
  print(nrow(chunk))
}
# Clear the result
dbClearResult(res)

# Disconnect from the database
dbDisconnect(con)
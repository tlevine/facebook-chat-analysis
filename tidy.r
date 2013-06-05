#!/usr/bin/env Rscript
library(RSQLite)

# IO DataFrame
load <- function() {
  connection = dbConnect('SQLite', dbname = '/tmp/logs.db')
  dbReadTable(connection, name = 'log_status')
}

# IO ()
main <- function () {
  df = load()
}


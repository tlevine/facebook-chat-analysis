#!/usr/bin/env Rscript
library(RSQLite)

# IO data.frame
load <- function() {
  connection = dbConnect('SQLite', dbname = '/tmp/logs.db')
  dbGetQuery(connection, 'SELECT "uid", "nick", "ts", "status" FROM log_status;')
}

# data.frame -> data.frame
set.types <- function(df) {
  df$ts <- as.POSIXct(df$ts, origin = '1970-01-01')
  df$uid <- as.factor(df$uid)
  df$nick <- as.factor(df$nick)
  df$status <- factor(df$status, levels = c('avail', 'notavail'))
  df
}

# IO ()
main <- function () {
  df <- load()
  df <- set.types(df)
  df
}


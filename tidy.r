#!/usr/bin/env Rscript
library(RSQLite)

# IO data.frame
load <- function() {
  connection = dbConnect('SQLite', dbname = '/tmp/logs.db')
  dbGetQuery(connection, 'SELECT "uid", "nick", "ts", "status" FROM log_status;')
}

# data.frame -> data.frame
set.types <- function(df) {
  df$date <- as.POSIXct(df$ts, origin = '1970-01-01')
  df$ts <- NULL
  df$uid <- as.factor(df$uid)
  df$nick <- as.factor(df$nick)
  df$status <- factor(df$status, levels = c('avail', 'notavail'))
  df
}

# Total duration online based on the date column
# If you want to group by uid or date, do that first.
# data.frame -> data.frame
duration <- function(df, start.date, end.date) {
  # Skip empty data frames.
  if (nrow(df) == 0) {
    return(0)
  }

  # Deal with sessions spanning date cuts
  if (df$status[1] == 'notavail') {
    head.duration <- df$date[1] - start.date
    df <- df[-1,]
  } else {
    head.duration <- 0
  }
  if (df$status[nrow(df)] == 'avail') {
    tail.duration <- end.date - df$date[nrow(df)]
    df <- df[-nrow(df),]
  } else {
    tail.duration <- 0
  }

  # Now we can assume that the data frame starts with 'avail', ends with 'notavail' and alternates.
  avail <- ((1:nrow(df)) %% 2) == 1
  cumsum(df[-avail,'date'] - df[avail,'date']) + head.duration + tail.duration
}

# IO ()
main <- function () {
  df <- load()
  df <- set.types(df)
  df
}


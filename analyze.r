#!/usr/bin/env Rscript
library(RSQLite)
library(lubridate)
library(ggplot2)
library(plyr)

# Time intervals
INTERVALS <- list(hour = hours, day = days, week = weeks, month = months)

# IO data.frame
load <- function(limit = NULL) {
  if (is.numeric(limit)) {
    limit <- paste('LIMIT', limit)
  } else {
    limit <- ''
  }
  connection <- dbConnect('SQLite', dbname = '/tmp/logs.db')
  dbGetQuery(connection, paste('SELECT "uid", "nick", "ts", "status" FROM log_status', limit, ';'))
}

# data.frame -> data.frame
munge <- function(df) {
  # Convert types
  df$date <- as.POSIXct(df$ts, origin = '1970-01-01')
  df$uid <- as.factor(df$uid)
  df$nick <- as.factor(df$nick)
  df$status <- factor(df$status, levels = c('avail', 'notavail'))

  # Bin by time
  for (interval in names(INTERVALS)) {
    start <- paste(interval,'start',sep='.')
    end <- paste(interval,'end',sep='.')
    df[,start] <- as.POSIXct(cut.POSIXt(df$date, interval))
    df[,end] <- df[,start] + INTERVALS[[interval]](1)
  }

  df
}

# Total duration online based on the ts column
# If you want to group by uid or date, do that first.
# data.frame -> num (seconds)
duration <- function(df, start.ts, end.ts) {
  # Skip empty data frames.
  if (nrow(df) == 0) {
    return(0)
  }

  # Deal with sessions spanning ts cuts
  if (df$status[1] == 'notavail') {
    head.duration <- start.ts - df$ts[1]
    df <- df[-1,]
  } else {
    head.duration <- 0
  }
  if (nrow(df) == 0) {
    return(head.duration)
  }

  if (df$status[nrow(df)] == 'avail') {
    tail.duration <- end.ts - df$ts[nrow(df)]
    df <- df[-nrow(df),]
  } else {
    tail.duration <- 0
  }
  if (nrow(df) == 0) {
    return(head.duration + tail.duration)
  }

  # Now we can assume that the data frame starts with 'avail', ends with 'notavail' and alternates.
  avail <- ((1:nrow(df)) %% 2) == 1
  notavail <- ((1:nrow(df)) %% 2) == 0
  sum(df[notavail,'ts'] - df[avail,'ts']) + head.duration + tail.duration
}

# data.frame -> data.frame
ply.duration <- function(df, interval) {
  start <- paste(interval,'start',sep='.')
  end <- paste(interval,'end',sep='.')
  df <- df[df[,start] > min(df[,start]) & (df[,end] < max(df[,end])),]
  if (nrow(df) == 0) {
    # Return an empty data frame if the data aren't good.
    df[1,][-1,]
  } else {
    # ply and calculate duration and whatnot.
    ddply(df, c('uid', start), function(df) {
      c(seconds.online = duration(df, as.numeric(strftime(df[1,start], '%s')), as.numeric(strftime(df[1,end], '%s'))))
    })
  }
}

# IO data.frame
df.dev <- function () {
  df <- load(limit = 1000)
  df <- munge(df)
  df
}

model <- function(df) {
  f <- is.online ~ seconds.online.hourly.median +
    seconds.online.userly.median +
    day.of.week + hour.of.day

  df.hour <- ply.duration(df, 'hour')
  df.hour$is.online <- df.hour$seconds.online > 0
  userly <- ddply(df.hour, 'uid', function(df.hour){
    c(seconds.online.userly.median = median(df.hour$seconds.online))
  })[c('uid','seconds.online.userly.median')]
  hourly <- ddply(df.hour, 'hour.start', function(df.hour){
    c(seconds.online.hourly.median = median(df.hour$seconds.online))
  })[c('hour.start','seconds.online.hourly.median')]
  df.hour <- plyr::join(df.hour, hourly)
  df.hour <- plyr::join(df.hour, userly)
  df.hour$day.of.week <- factor(strftime(df.hour$hour.start, '%A'),
    levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
  df.hour$hour.of.day <- as.numeric(strftime(df.hour$hour.start, '%H'))
}

# IO ()
main <- function() {
  df <- load()
  df <- munge(df)
}

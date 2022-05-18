
#################################################
# this R file contains 3 R functions that change the date level of a dataframe.
# week2day : turns weekly data to daily data by evenly distribute the weekly total to 7 days within that week),
# day2month: roll the daily data to month (as in Jan 1, Feb 1, etc.)
# week2month : turns weekly data to monthly (day2month %>% week2month)
# month2day : TBD
# The only argument that these functions takes in is a dataframe
# that consists of a time column (required to be the 1st column), and at least one column
# that consists of the data that you would like to modify to another time unit.
# 2021-04-12  Julia Liu : added week2month_panel() function that does week to month change for panel data.
#                         df : panel dataframe
#                         crosssection : the panel field 
#                         time : the time field
# 2021-10-20  Julia Liu : fixed an error with fun=="mean"
#################################################
week2day <- function(df, fun="sum") {
  names(df)[1] = "date_tmp"
  df <- df[order(df$date_tmp), ]
  vname <- names(df)
  
  df2 <- data.frame(seq.Date(as.Date(df$date_tmp[1]), as.Date(df$date_tmp[nrow(df)]+6), "day"))
  names(df2) = "date_tmp"
  
  for (i in 2:ncol(df)) {
    df2[[vname[i] ]] <- rep(df[[vname[i]]], each=7)
    if(fun == "sum") {
      df2[[vname[i] ]] <- df2[[vname[i] ]]/7
    }
  }
  return(df2)
}

day2month <- function(df, fun="sum") {
  df$Date = as.Date(cut(df$date_tmp, breaks = "month"))
  df$date_tmp = NULL
  if(fun=="mean") {
    df2 <- df %>% group_by(Date) %>% summarise_each(funs(mean))
  } else {
    df2 <- df %>% group_by(Date) %>% summarise_each(funs(sum))
  }
  return(df2)
}

week2month <- function(df, fun="sum") {
  df2 <- week2day(df, fun=fun)
  df3 <- day2month(df2, fun=fun)
  return(df3)
}

week2month_panel <- function(df, crosssection, time, fun="sum") {
  # let's confirm that the crosssection and time are in the df. stop the code if not found
  if(!(crosssection %in% names(df))) {
    stop("the crosssection you specified is not in the dataset.")
  }
  if(!(time %in% names(df))) {
    stop("the time variable you specified is not in the dataset.")
  }
  
  # let's move the time variable to the 1st column
  df <- df[, c(time, names(df)[-grep(time, names(df))])]
  split_data <- base::split(df, df[[crosssection]])
  output <- list()
  for (i in 1:length(split_data)) {
    cs <- unique(split_data[[i]][[crosssection]])
    split_data[[i]][[crosssection]] <- NULL
    intermediat <- week2day(split_data[[i]], fun=fun)
    output[[i]] <- day2month(intermediat, fun=fun)
    output[[i]][[crosssection]] <- cs
  }
  output <- do.call("rbind", output)
  return(output)
}
#month2day <- function(df) {
#  names(df)[1] = "date"
#  df <- df[order(df$date), ]
#  vname <- names(df)
#  
#  nday_lastmonth <- as.numeric(ceiling_date(as.Date(df$date[nrow(df)]), "month") - as.Date(df$date[nrow(df)]))

#  df2 <- data.frame(seq.Date(as.Date(df$date[1]), as.Date(df$date[nrow(df)]+nday_lastmonth-1), "day"))
#  names(df2) = "date"
#}


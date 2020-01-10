#' Execute Waterfall
#'
#' @param Dataframe
#'
#' @return
#' @export
#'
#' @description
#'
#' @examples
#'

library(lubridate)
library(tidyverse)
waterfall <- function(df, yago = c(yago_startdate, yago_finishdate), cy = c(cy_startdate, cy_finishdate)) {
  dates <- c(yago, cy)
  for (d in dates) {
    if (class(d) != "numeric" | nchar(d) != 8) {
      stop("The input date ", d, " needs to be 8 integers representing YYYYMMDD")
    }
  }
  yago_name <- paste("d", as.character(yago[1]), as.character(yago[2]), sep = "_")
  cy_name <- paste("d" , as.character(cy[1]), as.character(cy[2]), sep = "_")
  cy <- seq(ymd(cy[1]), ymd(cy[2]), 1)
  yago <- seq(ymd(yago[1]), ymd(yago[2]), 1)
  names(df)[1] <- "Date"
  t <- df %>% 
    #select relevant quarters
    filter(Date %in% c(cy, yago)) %>%
    #Classify CY and YAGO
    mutate(Time_Period = if_else(Date %in% cy, "CY", "YAGO")) %>% 
    #Drop Date
    select(-Date) %>%
    #Aggregate by Quarter
    group_by(Time_Period) %>%
    summarize_each(funs(sum)) %>%
    #Make the contributions their own column
    gather(Channel, Contribution, -Time_Period) %>%
    #split out the quarters (needed for output)
    spread(Time_Period, Contribution) %>% 
    select(Channel, YAGO, CY) %>% 
    mutate(Diff = CY - YAGO) %>% 
    arrange(desc(Diff))
  names(t)[2:3] <- c(yago_name, cy_name)
  return(t)
}








#needs(car, lmtest, tidyverse, lubridate, stringr, readxl)

row_div <- function(row, divisor) {
  return(as_tibble(lapply(row, function(x) if(is.numeric(x) == TRUE) x/divisor else x)))
}

make_daily <- function(df, start, finish){
  daily <- df %>% mutate(Date = ymd("2000-01-01"))
  daily <- daily[daily[start] > "5000-01-01", ]
  
  for (row in 1:nrow(df)){
    tmp <- df[row, ]
    gap <- as.double(tmp[[finish]] - tmp[[start]] +1)
    tmp <- row_div(tmp, gap)
    period <- seq(tmp[[start]], tmp[[finish]], 1)
    
    #Repeat the same divided row for each day and append it to the daily df
    for(day in 1:length(tmp[[start]]:tmp[[finish]])){
      daily <- bind_rows(daily, tmp) %>% 
        mutate(Date = if_else(is.na(Date) == TRUE, period[day], Date))
    }
  }
  daily
}
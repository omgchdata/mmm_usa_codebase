###Notes:
## df = [dataframe], requires time column to be first column in df, model_ready data file   
## startDate = [date] start date for CY 52 wks
## endDate = [date] end date of for CY 52 wks
## pie = "CY", returns piechart current year spend breakdown  
## pie = "YAGO", returns piechart year ago spend breakdown
## pie = NULL, returns spend breakdown dataframe no piechart
## Requires: spend variables to sum contains string spend
## source("Z:/BrandScience/msmp/R/meida_spend.R")
## E.X 1: spend_summary(mod_obj$data, mod_obj$SimStart, mod_obj$SimEnd)
## E.X 2: spend_summary(mod_obj$data, mod_obj$SimStart, mod_obj$SimEnd, pie = "CY")
spend_summary <- function(df, startDate, endDate, pie = NULL) {
  #convert string to date
  if(class(startDate) != "Date") {
    startDate <- mdy(startDate)
  } 
  if(class(endDate) != "Date") {
    endDate <- mdy(endDate)
  }
  #error if 52wk time range isn't provided
  if(endDate - startDate != 357) {
    stop("Start & End dates do not represent 52wks, fix date input")
  }
  #calculate YAGO spend
  spendYAGO <- df[unlist(df[,1]) >= (startDate - 364) & unlist(df[,1]) <= (endDate - 364),] %>% 
    select(ends_with("Spend", ignore.case = T)) %>% 
    colSums(na.rm = T) 
  YAGO <- as.tibble(spendYAGO)
  #calculate CY spend
  spendCY <- df[unlist(df[,1]) >= startDate & unlist(df[,1]) <= endDate,] %>% 
    select(ends_with("Spend", ignore.case = T)) %>% 
    colSums(na.rm = T) 
  CY <- as.tibble(spendCY)
  #calculate percent change
  pctChange <- bind_cols(YAGO, CY) %>%
    transmute(pctChange = round((value1 - value)/value*100, digits = 0))
  pctChange$pctChange[pctChange$pctChange == "Inf"] <- 100
  pctChange$pctChange[pctChange$pctChange == "NaN"] <- 0
  variables <- as.tibble(names(spendCY))
  #create spend df
  SpendSummary <- bind_cols(variables, CY, YAGO, pctChange) %>%
    transmute(Channel = value,
              CY =  round(value1, digits = 2),
              YAGO = round(value2, digits = 2),
              PercentChange = pctChange) %>%
    #arrange df by decreasing CY spend
    arrange(desc(CY))
  if (is.null(pie)) {
    return(SpendSummary)
  }
  #creat pie charts if pie variable provided
  if(pie == "CY") {
    library(plotly)
    pieChart <- plot_ly(SpendSummary, labels = ~Channel, values = ~CY, type = 'pie',
                    insidetextfont = list(color = '#FFFFFF')) %>%
      layout(title = 'CY Spend Breakdown',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    print(SpendSummary)
    return(pieChart)
  }
  if(pie == "YAGO") {
    library(plotly)
    pieChart <- plot_ly(SpendSummary, labels = ~Channel, values = ~YAGO, type = 'pie',
                        insidetextfont = list(color = '#FFFFFF')) %>%
      layout(title = 'YAGO Spend Breakdown',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    print(SpendSummary)
    return(pieChart)
  }
} 

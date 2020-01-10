##Notes:
#'metric_summary'function calculates CY,YAGO (FY18, FY17), (CPM),(CPP)
#of each metric/channel in model ready data.

##Source:
#source(paste(server, "/Annalect/BrandScience/msmp/R/metric_summary.R", sep = ""))

##HOW TO USE:
#metric_summary(df, startDate of CY, endDate of CY, channel/metric)

#e.g.
#metric_summary(omd_model_ready, "10/03/2016", "09/25/2017", "display")
#metric_summary(mediaBrandhealth, "10/03/2016", "09/25/2017", "Spend")
#metric_summary(ontrade_model_ready,"1/9/2017","1/1/2018","Oshega")

#######################SUB FUNCTION####################################
#a function calculating CY sum of a metric in a channel
#e.g. returns sum of CY Impressions for
#US GreyGoose Digital Video Channel
metric_sumCY <- function(df, startDate, endDate, channel, metric) {
  #calculate variableCY of the channel
  variableSumCY <- df[unlist(df[,1]) >= startDate & unlist(df[,1]) <= endDate,] %>%
    select(contains(channel, ignore.case = T)) %>%
    select(contains(metric, ignore.case = T)) %>%
    colSums(na.rm = T)
  return(variableSumCY)
}
#similar as above, but for YAGO
metric_sumYAGO <- function(df, startDate, endDate, channel, metric) {
  variableSumYAGO <- df[unlist(df[,1]) >= (startDate - 364) & unlist(df[,1]) <= (endDate - 364),] %>%
    select(contains(channel, ignore.case = T)) %>%
    select(contains(metric, ignore.case = T)) %>%
    colSums(na.rm = T)

}
#########################MAIN FUNCTION##############################
metric_summary <- function(df,startDate, endDate,channel) {
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

  df <- mutate(df, Week = as.Date(Week, format = "%m/%d/%Y"))

  #calculate YAGO sum of all metrics in the channel
  channelYAGO <- df[unlist(df[,1]) >= (startDate - 364) & unlist(df[,1]) <= (endDate - 364),] %>%
    select(contains(channel, ignore.case = T)) %>%
    colSums(na.rm = T)
  YAGO <- as.tibble(channelYAGO)
  #calculate spYAGO of the channel
  spYAGO <- metric_sumYAGO(df, startDate, endDate, channel, "Spend")
  #calculate impYAGO of the channel
  impYAGO <- metric_sumYAGO(df, startDate, endDate, channel, "Impressions")
  #calculate EQgrYAGO of the channel
  grpsYAGO <- metric_sumYAGO(df, startDate, endDate, channel, "GRPs")
  clickYAGO <- metric_sumYAGO(df, startDate, endDate, channel, "Clicks")
  compYAGO <- metric_sumYAGO(df, startDate, endDate, channel, "VideoCompletes")
  #calculate CY sum of all metrics in the channel
  channelCY <- df[unlist(df[,1]) >= startDate & unlist(df[,1]) <= endDate,] %>%
    select(contains(channel, ignore.case = T)) %>%
    colSums(na.rm = T)
  CY <- as.tibble(channelCY)
  #calculate spCY of the channel
  spCY <- metric_sumCY(df, startDate, endDate, channel, "Spend")
  #calculate impCY of the channel
  impCY <- metric_sumCY(df, startDate, endDate, channel, "Impressions")
  grpsCY <- metric_sumCY(df, startDate, endDate, channel, "GRPs")
  clickCY <- metric_sumCY(df, startDate, endDate, channel, "Clicks")
  compCY <- metric_sumCY(df, startDate, endDate, channel, "VideoCompletes")
  #calculate percent change
  pctChange <- bind_cols(YAGO, CY) %>%
    transmute(pctChange = round((value1 - value)/value*100, digits = 0))
  pctChange$pctChange[pctChange$pctChange == "Inf"] <- 100
  pctChange$pctChange[pctChange$pctChange == "NaN"] <- 0
  variables <- as.tibble(names(channelCY))
  #create df
  channelSummary <- bind_cols(variables, CY, YAGO, pctChange) %>%
    transmute(Channel = value,
              CY =  round(value1, digits = 2),
              YAGO = round(value2, digits = 2),
              PercentChange = pctChange)
  #create a tibble storing CPM
  tb1 <- tibble(
    Channel= "CPM",
    CY = as.double(spCY/impCY * 1000),
    YAGO = as.double(spYAGO/impYAGO * 1000),
    PercentChange = round((CY/YAGO - 1)*100, digits = 0))
  #create a tibble storing CPP
  tb2 <- tibble(
    Channel= "CPP",
    CY = as.double(spCY/grpsCY),
    YAGO = as.double(spYAGO/grpsYAGO),
    PercentChange = round((CY/YAGO - 1)*100, digits = 0))
  tb3 <- tibble(
    Channel= "Cost per Click",
    CY = as.double(spCY/clickCY),
    YAGO = as.double(spYAGO/clickYAGO),
    PercentChange = round((CY/YAGO - 1)*100, digits = 0))
  tb4 <- tibble(
    Channel= "Cost per Complete",
    CY = as.double(spCY/compCY),
    YAGO = as.double(spYAGO/compYAGO),
    PercentChange = round((CY/YAGO - 1)*100, digits = 0))
  #bind rows
  channelSummary <- bind_rows(channelSummary,tb1,tb2,tb3,tb4)
  return(channelSummary)
}


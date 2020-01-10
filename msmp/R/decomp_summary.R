###Notes:
## df = [dataframe], requires time column to be first column in df, final decomposition file for all weeks  
## startDate = [date] start date for range of decomp (i.e start date for 52 wks) 
## endDate = [date] end date of range for decomp (i.e end date for 52 wks)
## dependentVar = [string] dependent variable we want to divide all sums by to produce percent decomp, E.X: "Desire"
## spec = *optional* [dataframe], specifically mod_obj$spec to calculate media contribution
## source("Z:/BrandScience/msmp/R/decomp_summary.R")
## E.X 1: decomp_summary(mod_obj$Decomposition, mod_obj$SimStart, mod_obj$SimEnd, "dependentVar")
## E.X. 2: decomp_summary(mod_obj$Decomposition, mod_obj$SimStart, mod_obj$SimEnd, "dependentVar", mod_obj$spec)

decomp_summary <- function (df, startDate, endDate, dependentVar, spec = NULL) {
  temp <-df[unlist(df[,1]) >= startDate & unlist(df[,1]) <= endDate,]
  output <- data.frame(apply(temp[,-1], 2, FUN = sum)) %>% 
              transmute(Variables = colnames(temp[,-1]),
                        Sum = `apply.temp....1...2..FUN...sum.`)
  dependentVarSum <- output$Sum[output$Variables == dependentVar]
  output <- output %>%  mutate(Percent = (Sum/dependentVarSum)*100)
  #if mod_obj$spec provided, calculates total media contribution based on variable type (marketing)
  if(is.null(spec) == F) {
    spec <- spec %>% filter(Variable_Type == "Marketing")
    marketingVars <- spec$Trans_Variable
    media <- output %>% 
      mutate(Percent = (Sum/dependentVarSum)*100) %>% 
      filter(grepl(paste(marketingVars, collapse = "|"), Variables)) %>% 
      group_by("Variables") %>% 
      summarise(Percent = sum(Percent, na.rm = TRUE))
    mediaContribution <- round(media$Percent, digits = 4)
    print(paste0("Total Media Contribution: ", mediaContribution))
  }
  return(output)
}
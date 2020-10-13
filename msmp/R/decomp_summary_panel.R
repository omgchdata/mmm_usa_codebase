###Notes:
## df = [dataframe], requires time column to be first column in df, final decomposition file for all weeks  
## startDate = [date] start date for range of decomp (i.e start date for 52 wks) 
## endDate = [date] end date of range for decomp (i.e end date for 52 wks)
## dependentVar = [string] dependent variable we want to divide all sums by to produce percent decomp, E.X: "Desire"
## spec = *optional* [dataframe], specifically mod_obj$spec to calculate media contribution
## source("Z:/BrandScience/msmp/R/decomp_summary.R")
## E.X 1: decomp_summary(mod_obj$Decomposition, mod_obj$SimStart, mod_obj$SimEnd, "dependentVar")
## E.X. 2: decomp_summary(mod_obj$Decomposition, mod_obj$SimStart, mod_obj$SimEnd, "dependentVar", mod_obj$spec)

decomp_summary_prelim <- function (df, startDate, endDate, dependentVar) {
  temp <-df[unlist(df[,1]) >= startDate & unlist(df[,1]) <= endDate,]
  output <- data.frame(apply(temp[,-c(1,2,4)], 2, FUN = sum)) %>% 
              transmute(Variables = colnames(temp[,-c(1,2,4)]),
                        Sum = `apply.temp....c.1..2..4....2..FUN...sum.`)
  dependentVarSum <- output$Sum[output$Variables == dependentVar]
  output <- output %>%  mutate(Percent = (Sum/dependentVarSum)*100)

  return(output)
}

decomp_individual <- function(panelitem,obj) {
  dsum <- decomp_summary_prelim(obj$Decomposition_panel[obj$Decomposition_panel[,names(obj$Model$coefficients)[1]] == panelitem,], obj$SimStart, obj$SimEnd, as.character(obj$spec[1,1]))
  dsum$item <- panelitem
  names(dsum)[names(dsum)=="item"] <- names(obj$Model$coefficients)[1]
  dsum <- dsum[,c(4,1,2,3)]
  return(dsum)
}


panel_decomp <- function(obj2) {
  decomplist <- lapply(X = as.list(unique(obj2$Model$coefficients[,1])), decomp_individual,obj=obj2)
  bind_rows(decomplist)

  }



###################################
# Created by Helen Yan Nov. 2018
# update notes:
# Julia Liu 2020-05-15 : fixed a bug and removed portion that is specific to Bacardi
# Julia Liu 2020-06-03 : using "join" with the model spec sheet to get to AggregateVariable level.
# Yi Wu 2020-09-16 : enable custom date ranges
############################################################################
library(lubridate)
library(tidyverse)
library(reshape)
# For first two parameters, use mod_obj$Decomposition and mod_obj$spec
DueToChart <- function(df, spec, startdate_current, enddate_current, startdate_previous, enddate_previous, current_name = "Current_Period", previous_name = "Past_Period") {
  names(df)[1] <- "Week"
  if("predicted" %in% names(df)) {
    df$predicted <- NULL
  }
  depvar <- spec$Orig_Variable[tolower(spec$Variable_Type) == "dependent"]
  spec$AggregateVariable[tolower(spec$Variable_Type) == "dependent"] <- "KPI"
  dueto_obj <- list()
  decomp_pct <- df
  decomp_pct$Yearly <- 0
  decomp_pct$Yearly[decomp_pct$Week <= enddate_current & decomp_pct$Week >= startdate_current] <- 'Current_Period'
  decomp_pct$Yearly[decomp_pct$Week <= enddate_previous & decomp_pct$Week >= startdate_previous] <- 'Past_Period'  

  lkup <- spec[, c("Orig_Variable", "Trans_Variable", "AggregateVariable", "Variable_Type")]
  lkup$d_var <- ifelse(lkup$AggregateVariable!="KPI", paste("d_", lkup$Trans_Variable, sep=""), lkup$Trans_Variable)
  
  pct <- t(rowsum(decomp_pct[,2:ncol(df)], group = decomp_pct$Yearly)) # adjust column numbers based on real decomp

  Residual <- pct[1,] - colSums(pct[-1,]) # calculate residual
  #Residual <- pct[1] - pct[-1] # calculate residual

  pct <- rbind(pct,Residual)
  pct <- as.data.frame(pct)
 
  pct$d_var <- row.names(pct)
  pct <- dplyr::right_join(lkup, pct)
  pct$AggregateVariable[pct$d_var == "Residual"] <- "Residual"
  pct <- pct[, c("AggregateVariable", "Current_Period", "Past_Period")]
  
  # aggregate decomp by AggregateVariable name
  pctnew <- pct %>%
    dplyr::group_by(AggregateVariable) %>%
    dplyr::summarise(Past_Period = sum(Past_Period), Current_Period = sum(Current_Period))
  pctnew <- pctnew[!is.na(pctnew$AggregateVariable), ]
  
  # calculate percentage change based on Past_Period KPI
  pctnew$diff <- (pctnew$Current_Period - pctnew$Past_Period)/pctnew$Past_Period[pctnew$AggregateVariable =="KPI"]

  pctnew <- pctnew[order(pctnew$diff), ]


  theme_set(theme_bw())
  pctnew$DueTo <- ifelse(pctnew$diff > 0, "Positive", "Negative")
  pctnew$AggregateVariable <- 
    factor(pctnew$AggregateVariable, levels = c(pctnew$AggregateVariable[pctnew$AggregateVariable != "KPI"], "KPI"))
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }
  pct$Current_PeriodPCT <- percent(pct$Current_Period/pct$Current_Period[pct$AggregateVariable =="KPI"])

  chart <-
  ggplot(pctnew, aes(x = AggregateVariable, y = diff)) +
    geom_bar(stat='identity', aes(fill = DueTo), width = 0.5) +
    scale_fill_manual(name="Due-to Effect",
                      labels = c("Negative Effect", "Positive Effect"),
                      values = c("Negative"="#f8766d","Positive"="#00ba38")) +
    labs(subtitle=paste0(current_name," vs. ", previous_name),
         title= paste("Due-to Chart", sep = " ")) +
    coord_flip()+
    guides(fill = guide_legend(reverse = TRUE)) +              # flip ordering of legend without altering ordering in plot
    geom_text(aes(label = sprintf("%.2f%%", diff * 100),
                  x = AggregateVariable,
                  y = diff + ifelse(diff >= 0, 0, -0.01)),    # adjust label position of negative numbers
              hjust = -0.1,
              position = position_dodge(width = 1)) +
    scale_y_continuous(label = scales::percent, expand = c(.01, .01)) + # expand y axis
    labs(x = "Media Type", y = "Decomposition Percentage Change")

  #dueto_obj$pct <- pct[c(1,ncol(pct)-1,ncol(pct)-3,ncol(pct)-4,ncol(pct))]
  dueto_obj$pct <- pctnew
  dueto_obj$chart <- chart
  
  names(dueto_obj$pct)[2] <- previous_name
  names(dueto_obj$pct)[3] <- current_name 
  
  return(dueto_obj)
}


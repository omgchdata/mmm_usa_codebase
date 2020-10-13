
###################################
# Created by Helen Yan Nov. 2018
# update notes:
# Julia Liu 2020-05-15 : fixed a bug and removed portion that is specific to Bacardi
# Julia Liu 2020-06-03 : using "join" with the model spec sheet to get to AggregateVariable level.
############################################################################
library(lubridate)
library(tidyverse)
library(reshape)
DueToChart <- function(df, spec) {
  names(df)[1] <- "Week"
  depvar <- spec$Orig_Variable[tolower(spec$Variable_Type) == "dependent"]
  spec$AggregateVariable[tolower(spec$Variable_Type) == "dependent"] <- "KPI"
  dueto_obj <- list()
  decomp_pct <- df
  decomp_pct$Yearly <- 0
  lastweek <- max(decomp_pct$Week)
  decomp_pct$Yearly[decomp_pct$Week <= lastweek & decomp_pct$Week >= (lastweek - weeks(51))] <- 'CurrentYear'             # the latest 52 weeks
  decomp_pct$Yearly[decomp_pct$Week <= (lastweek - weeks(52)) & decomp_pct$Week > (lastweek - weeks(104))] <- 'YearAgo'  # one year ago

  lkup <- spec[, c("Orig_Variable", "Trans_Variable", "AggregateVariable", "Variable_Type")]
  lkup$d_var <- ifelse(lkup$AggregateVariable!="KPI", paste("d_", lkup$Trans_Variable, sep=""), lkup$Trans_Variable)
  
  pct <- t(rowsum(decomp_pct[,2:ncol(df)], group = decomp_pct$Yearly)) # adjust column numbers based on real decomp

  Residual <- pct[1,] - colSums(pct[-1,]) # calculate residual
  #Residual <- pct[1] - pct[-1] # calculate residual

  pct <- rbind(pct,Residual)
  pct <- as.data.frame(pct)
 
  pct$d_var <- row.names(pct)
  pct <- right_join(lkup, pct)
  pct$AggregateVariable[pct$d_var == "Residual"] <- "Residual"
  pct <- pct[, c("AggregateVariable", "CurrentYear", "YearAgo")]
  
  # aggregate decomp by AggregateVariable name
  pctnew <- pct %>%
    group_by(AggregateVariable) %>%
    summarise(YearAgo = sum(YearAgo), CurrentYear = sum(CurrentYear))
  pctnew <- pctnew[!is.na(pctnew$AggregateVariable), ]
  
  # calculate percentage change based on YearAgo KPI
  pctnew$diff <- (pctnew$CurrentYear - pctnew$YearAgo)/pctnew$YearAgo[pctnew$AggregateVariable =="KPI"]

  pctnew <- pctnew[order(pctnew$diff), ]


  theme_set(theme_bw())
  pctnew$DueTo <- ifelse(pctnew$diff > 0, "Positive", "Negative")
  pctnew$AggregateVariable <- 
    factor(pctnew$AggregateVariable, levels = c(pctnew$AggregateVariable[pctnew$AggregateVariable != "KPI"], "KPI"))
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }
  pct$CurrentYearPCT <- percent(pct$CurrentYear/pct$CurrentYear[pct$AggregateVariable =="KPI"])

  chart <-
  ggplot(pctnew, aes(x = AggregateVariable, y = diff)) +
    geom_bar(stat='identity', aes(fill = DueTo), width = 0.5) +
    scale_fill_manual(name="Due-to Effect",
                      labels = c("Negative Effect", "Positive Effect"),
                      values = c("Negative"="#f8766d","Positive"="#00ba38")) +
    labs(subtitle="Current Year vs. Year Ago",
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
  return(dueto_obj)
}


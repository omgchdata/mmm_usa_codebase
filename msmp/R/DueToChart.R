#' Execute Due-to Chart
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
#'


library(lubridate)
library(tidyverse)
library(reshape)
DueToChart <- function(df, spec) {
  dueto_obj <- list()
  decomp_pct <- df
  decomp_pct$Yearly <- 0
  lastweek <- max(decomp_pct$Week)
  decomp_pct$Yearly[decomp_pct$Week <= lastweek & decomp_pct$Week >= (lastweek - weeks(51))] <- 'CurrentYear'             # the latest 52 weeks
  decomp_pct$Yearly[decomp_pct$Week <= (lastweek - weeks(52)) & decomp_pct$Week > (lastweek - weeks(104))] <- 'YearAgo'  # one year ago

 # pct <- as.data.frame(pct)
  #length <- length(pct)
  pct <- t(rowsum(decomp_pct[,2:ncol(df)], group = decomp_pct$Yearly)) # adjust column numbers based on real decomp
  #pct <- as.data.frame(pct)
  Residual <- pct[1,] - colSums(pct[-1,]) # calculate residual
  #Residual <- pct[1] - pct[-1] # calculate residual
  
  pct <- rbind(pct,Residual)
  pct <- as.data.frame(pct)

  row.names(pct) <- sub("d_", "", row.names(pct))    # remove "d_" in the variable names
  row.names(pct) <- sub("_t$", "", row.names(pct))   # remove "_t" in the variable names

  # use Model_Spec as mapping table to standardize variable names
  pct <- merge(x = pct, y = spec[c("Orig_Variable", "AggregateVariable")], by.x = 0, by.y = "Orig_Variable", all.x = TRUE)
  # categorize Residual, Familiarity and Sentiment
  pct$cat <- ifelse(pct$Row.names %in% c("Intercept"), "Intercept", 
                    ifelse(str_detect(pct$Row.names, "Residual"), "Residual", #Category Growth
                            ifelse(str_detect(pct$Row.names, "Familiarity"), "Familiarity",
                                   ifelse(str_detect(pct$Row.names, "Sentiment") |str_detect(pct$Row.names, "SocialListening"),"Sentiment",
                                          ifelse(str_detect(pct$Row.names, "FBIG"),"Facebook/Instagram",
                                                 ifelse(str_detect(pct$Row.names, "PR_"),"PR",
                                                        ifelse(str_detect(pct$Row.names, "Pinterest"),"Pinterest", 
                                                               ifelse(str_detect(pct$Row.names, "Print"),"Print",
                                                                      ifelse(str_detect(pct$Row.names, "DigitalVideo"),"DigitalVideo",
                                                                             ifelse(str_detect(pct$Row.names, "You"),"Youtube", # sometimes it's YouTube, some are Youtube
                                                                                    ifelse(str_detect(pct$Row.names, "Digital_Display"),"Display",
                                                                                            ifelse(str_detect(pct$Row.names, "OOH"),"OOH",
                                                                                                   ifelse(str_detect(pct$Row.names, "Events"),"Events",
                                                                                                          ifelse(str_detect(pct$Row.names, "Sponsorship"),"Sponsorship",
                                                                                                                 pct$AggregateVariable))))))))))))))

  # aggregate decomp by cat name
  pctnew <- pct %>%
    group_by(cat) %>%
    summarise(YearAgo = sum(YearAgo), CurrentYear = sum(CurrentYear))

  # calculate percentage change based on YearAgo KPI
  pctnew$diff <- (pctnew$CurrentYear - pctnew$YearAgo)/pctnew$YearAgo[pctnew$cat=="KPI"]

  pctnew <- pctnew[order(pctnew$diff), ]


  theme_set(theme_bw())
  pctnew$DueTo <- ifelse(pctnew$diff > 0, "Positive", "Negative")
  pctnew$cat <- factor(pctnew$cat, levels = c(pctnew$cat[pctnew$cat != "KPI"], "KPI"))
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }
  pct$CurrentYearPCT <- percent(pct$CurrentYear/pct$CurrentYear[pct$cat =="KPI"])

  DueToChart <-
  ggplot(pctnew, aes(x = cat, y = diff)) +
    geom_bar(stat='identity', aes(fill = DueTo), width = 0.5) +
    scale_fill_manual(name="Due-to Effect",
                      labels = c("Negative Effect", "Positive Effect"),
                      values = c("Negative"="#f8766d","Positive"="#00ba38")) +
    labs(subtitle="Current Year vs. Year Ago",
         title= paste("Due-to Chart", sep = " ")) +
    coord_flip()+
    guides(fill = guide_legend(reverse = TRUE)) +              # flip ordering of legend without altering ordering in plot
    geom_text(aes(label = sprintf("%.2f%%", diff * 100),
                  x = cat,
                  y = diff + ifelse(diff >= 0, 0, -0.01)),    # adjust label position of negative numbers
              hjust = -0.1,
              position = position_dodge(width = 1)) +
    scale_y_continuous(label = scales::percent, expand = c(.01, .01)) + # expand y axis
    labs(x = "Media Type", y = "Decomposition Percentage Change")

  dueto_obj$pct <- pct[c(1,ncol(pct)-1,ncol(pct)-3,ncol(pct)-4,ncol(pct))]
  dueto_obj$chart <- DueToChart
  return(dueto_obj)
}


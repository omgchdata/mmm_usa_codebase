
#################################
# Julia Liu created 2020-06--9
# This function generates decomposition at the aggregated variable level 
# update notes:
# 2020-06-16 Julia Liu : tidy up the response curves:
#                       1) roll up to aggregate variable level
#                       2) scales the nested response curves to the unnested curves.
# 2020-08-25 Julia Liu : added a if statement to see if obj$responsecurve exisits or not. 
#                        If it doesn't, skip the response curve unnesting.
#                       
###########################################################
post_unnesting <- function(obj) {
  
  if(is.null(obj$vlkup)) {
    print("there is no vlkup table, using the spec table instead.")
    obj$vlkup <- obj$spec[, c("Orig_Variable", "Trans_Variable", "AggregateVariable", "Variable_Type")]
  }
  
  d <- obj$Decomposition
  d$time <- d[[obj$time]]
  d[[obj$time]] <- NULL
  
  d <- gather(d, "d_var", "contributions", -time)
  d <- left_join(d, obj$vlkup)
  dd <- d %>% group_by(AggregateVariable, time) %>% summarise(contributions = sum(contributions))
  
  obj$dueto_unnested <- DueToChart(obj$Decomposition, obj$vlkup)
  obj$Decomposition_Unnested_AggregateVariable <- dd

  p <- ggplot(dd[dd$AggregateVariable != "KPI",], aes(x=time, y=contributions, fill=AggregateVariable)) + 
    geom_area()
  
  obj$decomp_unnested_chart_stackedarea <- p
  
  
  vlkup <- obj$vlkup
  vlkup$rc_var <- paste("rc", vlkup$Orig_Variable, sep="_")

  
  d <- obj$Decomposition_Unnested_AggregateVariable
  d <- d[d$time >= obj$simstart & d$time <= obj$simend, ]
  unnested_d <- d %>% group_by(AggregateVariable) %>% summarise(contributions= sum(contributions))

  # work on response curves if the dataframe exisits
  if(!is.null(obj$responsecurve)) {
    rc <- obj$responsecurve %>% gather(rc_var, rc, -Delta)
    rc2 <- inner_join(rc, vlkup[, c("rc_var", "AggregateVariable")])
    rc3 <- rc2 %>% group_by(AggregateVariable, Delta ) %>% summarise(rc = sum(rc))
  
    # loop through the response curves
    v <- unique(rc3$AggregateVariable)
    rc4 <- rc3
    for (i in 1:length(v)) {
      s <- unnested_d$contributions[unnested_d$AggregateVariable == v[i]]/rc3$rc[rc3$AggregateVariable == v[i] & rc3$Delta == 0]
      rc4$rc[rc4$AggregateVariable == v[i]] <- rc4$rc[rc4$AggregateVariable == v[i]] * s
    }
  
    obj$responsecurve_nested_AggregateVariable <-   rc3 %>% spread(AggregateVariable, rc)
    obj$responsecurve_unnested_AggregateVariable  <- rc4 %>% spread(AggregateVariable, rc)
  }
  
  return(obj)
}


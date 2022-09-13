
contributions <- function(obj, start_date, end_date, by="variable") {
  depvar <- obj$spec[obj$spec$Variable_Type == "Dependent",]$Trans_Variable
  decomp_df <- obj$Decomposition
  
  total <- sum(decomp_df[decomp_df[[obj$Time]] >= start_date & decomp_df[[obj$Time]] <= end_date,][depvar])
  
  #names(decomp_df)[1] <- "Week"
  decomp_df[[obj$Time]] <- NULL
  
  contrib <- data.frame(apply(decomp_df, 2, sum))
  contrib$Variable <- row.names(contrib)
  names(contrib) <- c("decomp", "Variable")
  contrib <- contrib[, c("Variable", "decomp")]
  contrib$Variable <- gsub("d_", "", contrib$Variable)
  contrib$Percent <- contrib$decomp/contrib$decomp[contrib$Variable == "predicted"]

  if(tolower(by) == "aggregatevariable"){
    contrib$Trans_Variable <- contrib$Variable
    contrib <- left_join(contrib, obj$spec[, c("Trans_Variable", "AggregateVariable")])
    contrib$AggregateVariable[contrib$Trans_Variable == "predicted"] = "predicted"
    
    contrib$AggregateVariable[contrib$Variable == depvar] = depvar
    contrib <- contrib %>% group_by(AggregateVariable) %>%
      summarise(decomp = sum(decomp),
                Percent = sum(Percent))
    contrib <- contrib[contrib$AggregateVariable != depvar,]
    contrib <- contrib[contrib$AggregateVariable != "predicted",]
  } else {
    contrib <- contrib[contrib$Variable != depvar,]
    contrib <- contrib[contrib$Variable != "predicted",]
  }

 return(contrib)
}


################################################
# update notes:
# 2020-05-12 Julia Liu : added mod_obj$decomp_stackedarea_chart
# 2020-05-20 Julia Liu : added another stacked area chart (mod_obj$decomp_stackedarea_chart_origvar), this time at the Orig Variable level,
#                        instead of the AggregateVariable level. 
# Julia Liu 2021/03/02 : added reference point calculation. 
#                        You will need to add a column called "decomp_ref" in the _Variable.csv
#                        The function first check to see if this field exists. 
# Julia Liu 2021/09/14 : added a coefficient check at the beginning. 
# Julia Liu 2021/10/18 " check to see if "Intercept" field is in obj$data
#######################################################
Decomp <- function(obj, incl_spent = FALSE, loop = FALSE) {

  spec <- obj$spec
  if(is.null(obj$Model$coefficients)) {
    stop("The model object does not have coefficient. ")
  }
  if("Intercept" %in% spec$Orig_Variable & !("Intercept" %in% names(obj$data))) {
    obj$data$Intercept <- 1
  }
  x <- obj$data
  
  b <- obj$Model$coefficients
  depvar <- obj$spec$Trans_Variable[obj$spec$Variable_Type=="Dependent"]
  decomp <- x[, c(obj$Time, depvar)]

  calc_var <- spec$Trans_Variable[spec$Variable_Type!="Dependent"]
  dcmp_var <- paste("d", calc_var, sep="_")
  #print(dcmp_var)

  if(length(calc_var) <= 0) {
    stop("Please specify which variables you would like to decomp. ")
  }

  if(toupper(obj$ModelForm) == "LIN_LIN") {
    for(i in 1:length(calc_var)) {
      cat("Calculate decomposition of ", calc_var[i], "\n")
      decomp[[dcmp_var[i] ]] <- b$Estimate[b$Variables == calc_var[i]] * x[[calc_var[i] ]]
      if("decomp_ref" %in% tolower(names(spec))) {
        ref <- spec$decomp_ref[spec$Trans_Variable == calc_var[i]]
        cat("reference point =", ref, "\n")
        decomp[[dcmp_var[i]]] <- decomp[[dcmp_var[i]]] - ref
      }
    }

  } else if(toupper(obj$ModelForm) == "LOG_LOG") {
    stop("Sorry, haven't implemented yet. ")
    cat("The reference point is based on ", Reference, "\n")
    for(i in 1:length(calc_var)) {

    }

  }

# include spent variable
  if(incl_spent) {
    for (i in 1:length(calc_var)) {
      spend_var <- spec$Spent_Variable[spec$Trans_Variable == calc_var[i]]
      if( !is.na(spend_var) ) {
        decomp[[spend_var]] <- x[[ spend_var ]]
      }
    }
  }
  
  obj$Decomposition <- decomp
  
  if(!loop) {
  d <- obj$Decomposition
  if(!("time" %in% names(d))) {
    d$time <- d[[obj$Time]]
    d[[obj$Time]] <- NULL
  }
  d <- gather(d, key = "d_var", value = "decomp", -time)
  
  spec <- obj$spec
  spec$d_var = paste("d", spec$Trans_Variable, sep="_")
  d <- left_join(d, spec[, c("Orig_Variable", "AggregateVariable", "d_var")])
  dd <- d %>% dplyr::group_by(AggregateVariable, time) %>% dplyr::summarise(decomp = sum(decomp))
  dd$AggregateVariable <- tolower(dd$AggregateVariable)
  # put the "base" last in the factor order.
  dd$AggregateVariable <- factor(dd$AggregateVariable,levels=c(setdiff(unique(dd$AggregateVariable), "base"), "base"))
  
  p <- ggplot(dd[!is.na(dd$AggregateVariable),], aes(x=time, y=decomp, fill=AggregateVariable)) + 
    geom_area()
  
  obj$decomp_chart_stackedarea <- p
  
  obj$decomp_chart_stackedarea_origvar <- 
    ggplot(d[!is.na(d$Orig_Variable),], aes(x=time, y=decomp, fill=Orig_Variable)) + 
    geom_area()
  }
  
  return(obj)
}



#######################################
# Julia Liu created on March 15 2020
# This is based on the Decomp.R 
Decomp <- function(obj, incl_spent = FALSE) {


  x <- obj$data
  spec <- obj$spec
  b <- obj$Model$coefficients
  b$Error <- NULL
  b$Tvalue <- NULL
  b$Variables <- paste("beta", b$Variables, sep="_")
  b <- tidyr::spread(b, Variables, Estimate)
  depvar <- obj$spec$Trans_Variable[obj$spec$Variable_Type=="Dependent"]
  depvar_orig <- obj$spec$Orig_Variable[obj$spec$Variable_Type=="Dependent"]
  cs <- obj$CS
  if(is.null(cs)) {
    stop("Please define cross section/panel (CS) in ModelSetup.csv. ")
  }
  if (depvar==depvar_orig) {
    decomp <- x[, c(obj$Time, cs, depvar)]
  } else {
    decomp <- x[, c(obj$Time, cs, depvar_orig, depvar)]
  }
  
  x <- left_join(x, b)     # dataframe that contains the data and betas
  calc_var <- spec$Trans_Variable[tolower(spec$Variable_Type) !="dependent"]
  dcmp_var <- paste("d", calc_var, sep="_")
  beta_var <- paste("beta", calc_var, sep="_")

  if(length(calc_var) <= 0) {
    stop("Please specify which variables you would like to decomp. ")
  }

  if(toupper(obj$ModelForm) == "LIN_LIN") {
    for(i in 1:length(calc_var)) {
      cat("Calculate decomposition of ", calc_var[i], "\n")
      decomp[[dcmp_var[i] ]] <- x[[beta_var[i]]] * x[[calc_var[i] ]]
# if the dep var is mean-centered, reverse it here
      if(toupper(spec$Transform[spec$Trans_Variable == depvar]) == "Y" & toupper(spec$TransformType[spec$Trans_Variable == depvar]) == "MC") {
        if(tolower(dcmp_var[i]) == "d_intercept") {
          decomp[[dcmp_var[i] ]] <- decomp[[dcmp_var[i]]] * x$scl + x$cen
        } else {
          decomp[[dcmp_var[i] ]] <- decomp[[dcmp_var[i]]] * x$scl
        }
      }
    }
    
    # this takes care of a mean-centered model that does not have intercept 
    if (toupper(spec$Transform[spec$Trans_Variable == depvar]) == "Y" & 
        tolower(spec$TransformType[spec$Trans_Variable == depvar]) == "mc" & 
        !("intercept" %in% tolower(calc_var)) ) {
      decomp$mc_center <- x$cen
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
  
  obj$Decomposition_panel <- decomp
  
  # create national level decomposition 
  decomp$time <- decomp[[obj$Time]]
  d_nat <- decomp %>% group_by(time) %>% summarise_if(is.numeric, sum, na.rm = TRUE)
  names(d_nat)[1] <- obj$Time
  obj$Decomposition <- d_nat
  
  # create stacked area decomp chart at modeled variable level ($decomp_chart_stackedarea_origvar)
  # and aggregated variable level ($decomp_chart_stackedarea)
  
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
  
  return(obj)
}


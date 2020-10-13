##########################################
#  simulate (response curve) in msmp framework
#  2020/04/16 Julia Liu : added responsecurve_panel() function. This function should be used when you have a panel model.
#  2020/05/21 Julia Liu : made changes so that the marginal is caculated even when spent information is not available
##########################################
responsecurve = function(obj, showPlot=FALSE) {

  SimuVar <- obj$spec$Orig_Variable[toupper(obj$spec$Simulate) == "Y" & obj$spec$Include == 1 ]
  if(length(SimuVar) == 0) {
    stop("Please specify the varilables that you would like to simulate by setting N to Y in the _Variable.csv")
  } else {
    cat("Generating response curves for the following variables", SimuVar, "\n")
  }
  spec <- obj$spec
  orig_data <- obj$data
  depvar <- spec$Trans_Variable[tolower(spec$Variable_Type) == "dependent"]

  SimStart <- obj$SimStart
  SimEnd <- obj$SimEnd
  delta = seq(-1, 3, obj$mroi_step)
  deltaname = paste("P", delta, sep="")
  deltaname = sub("P-", "N", deltaname)
  
  coef <- obj$Model$coefficients
  b <- coef$Estimate
  
  SimuResult = data.frame(obj$data[[obj$Time]])
  names(SimuResult) = obj$Time
  
  for (i in 1:length(SimuVar)) {
    cat("generating response curve for:", SimuVar[i], "\n")
    transpec = spec[spec$Orig_Variable == SimuVar[i],]
    for (k in 1:length(delta)) {
      #tmp = obj$data
      #tmp[[transpec$Orig_Variable]] = tmp[[transpec$Orig_Variable]]*(1+delta[k])
      obj$data <- orig_data
      obj$data[[transpec$Orig_Variable]] = obj$data[[transpec$Orig_Variable]]*(1+delta[k])
      obj <- Transform(obj, print=FALSE)
      X = as.matrix(obj$data[,coef$Variables])
      SimuResult[[ paste(transpec$Orig_Variable, deltaname[k], sep="") ]] = X %*% b
    }
  }
  
  # cut out the simulation period
  SimuResult = SimuResult[SimuResult[[obj$Time]] >= SimStart & SimuResult[[obj$Time]] <= SimEnd,]

  y = apply(SimuResult[,-1], 2, sum)
  
  RC = matrix(nrow=length(delta), ncol=length(SimuVar))
  for (i in 1:length(SimuVar) ) {
    #  RC[,i] = as.numeric(y[paste(SimuVar[1], deltaname, sep="")])
    v = paste(SimuVar[i], deltaname, sep="")
    RC[,i] = y[v] - y[v[1]]
    
    # if the depvar was mean-centered, reverse it here:
    if(toupper(spec$Transform[spec$Trans_Variable == depvar]) == "Y" & toupper(spec$TransformType[spec$Trans_Variable == depvar]) == "MC") {
      #if(tolower(SimuVar[i]) == "intercept") {
      #  decomp[[dcmp_var[i] ]] <- decomp[[dcmp_var[i]]] * x$scl + x$cen
      #} else {
        RC[,i] <- RC[,i] * mean(obj$data$scl)
      #}
    }
  }
  RC = data.frame(cbind(delta, RC))
  inc_SimuVar <- paste("rc",SimuVar,sep="_")
  names(RC) = c("Delta", inc_SimuVar)
  
  for (i in 1:length(SimuVar) ) {
    spt <- obj$spec$Spent_Variable[obj$spec$Orig_Variable == SimuVar[i]]
    if(!is.na(spt)) {
      RC[[spt]] <- (delta+1) * sum(obj$data[[spt]][obj$data[[obj$Time]] >= SimStart & obj$data[[obj$Time]] <= SimEnd])
    }
    if(showPlot) {
      plot(delta, RC[[inc_SimuVar[i] ]], pch=20, col="blue", main=SimuVar[i], ylab="")
      readline("hit Enter to see the next response curve ...")
    }
  }
  
  RC$Delta <- as.numeric(RC$Delta)
  kpi <- rep(NA, length(SimuVar))
  spend <- rep(NA, length(SimuVar))
  mroi <- rep(NA, length(SimuVar))
  roi <- rep(NA, length(SimuVar))
  for (i in 1:length(SimuVar)) {
    v <- paste("rc", SimuVar[i], sep="_")
    spentv <- obj$spec$Spent_Variable[obj$spec$Orig_Variable == SimuVar[i]]
    kpi[i] <- RC[[v]][RC$Delta == 0]
    if(!is.na(spentv)) {
      spend[i] <- RC[[spentv]][RC$Delta == 0]
      if(spend[i] != 0) {
        mroi[i] <- 
          (RC[[v]][RC$Delta == as.character(obj$mroi_step)] - RC[[v]][RC$Delta == 0])/
          (RC[[spentv]][RC$Delta == as.character(obj$mroi_step)] - RC[[spentv]][RC$Delta == 0])
        roi[i] <- RC[[v]][RC$Delta == 0]/RC[[spentv]][RC$Delta == 0]
      }
    }
  }
  mroi <- data.frame(SimuVar, kpi, spend, roi, mroi)
  mroi$ratio <- ifelse(mroi$roi==0, NA, mroi$mroi/mroi$roi)
  
  obj$data <- orig_data
  obj$ResponseCurve <- RC
  obj$kpi_spent <- mroi
  names(obj$kpi_spent) <- c("Variables", "kpi", "spent", "kpi_per_spent", "mkpi_per_spent", "ratio")
  return(obj)
}

responsecurve_panel = function(obj, showPlot=FALSE) {
  
  orig_data <- obj$data
  store_coef <- obj$Model$coefficients
  coef <- obj$Model$coefficients
  cs <- unique(coef[[obj$CS]])
  
  rc <- list()
  # loop through the cross sections
  for (i in 1:length(cs) ) {
    obj$Model$coefficients <- coef[coef[[obj$CS]] == cs[i], ]
    obj$data <- orig_data[orig_data[[obj$CS]] == cs[i], ]
    cat("working on cross section", cs[i], "\n")
    tmp_obj <- responsecurve(obj, showPlot = showPlot)
    rc[[i]] <- cbind(cs[i], tmp_obj$ResponseCurve)
    names(rc[[i]])[1] <- obj$CS
  }
  
  rc <- do.call("rbind", rc)
  rc2 = rc %>% group_by(Delta) %>% summarise_if(is.numeric, function(x) sum(x, na.rm = TRUE))
  
  obj$ResponseCurve_panel <- rc
  obj$ResponseCurve <- rc2
  obj$Model$coefficients <- store_coef
  return(obj)
}

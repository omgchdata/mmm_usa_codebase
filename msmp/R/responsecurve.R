##########################################
#  simulate (response curve)
##########################################
responsecurve = function(obj, showPlot=FALSE) {

  SimuVar <- obj$spec$Orig_Variable[obj$spec$Simulate == "Y" & obj$spec$Include == 1 ]
  spec <- obj$spec
  orig_data <- obj$data
  
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
  mroi <- rep(0, length(SimuVar))
  roi <- rep(0, length(SimuVar))
  for (i in 1:length(SimuVar)) {
    v <- paste("rc", SimuVar[i], sep="_")
    spentv <- obj$spec$Spent_Variable[obj$spec$Orig_Variable == SimuVar[i]]
    if(!is.na(spentv)) {
    mroi[i] <- (RC[[v]][RC$Delta == as.character(obj$mroi_step)] - RC[[v]][RC$Delta == 0])/
      (RC[[spentv]][RC$Delta == as.character(obj$mroi_step)] - RC[[spentv]][RC$Delta == 0])
    roi[i] <- RC[[v]][RC$Delta == 0]/RC[[spentv]][RC$Delta == 0]
    }
  }
  mroi <- data.frame(SimuVar, roi, mroi)
  obj$data <- orig_data
  obj$ResponseCurve <- RC
  obj$kpi_spent <- mroi
  names(obj$kpi_spent) <- c("Variables", "kpi_per_spent", "mkpi_per_spent")
  return(obj)
}

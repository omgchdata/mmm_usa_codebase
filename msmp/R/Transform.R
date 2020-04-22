
##########################
# revision:
# Julia Liu 2020-03-05 : added Transform_panel
#                        for cross sectional/panel dataset
# Julia Liu 2020-03-09 : added "MC" (mean centered) transformation type
# Julia Liu 2020-03-18 : record the mean center parameters (scale&center) by add them to the mod_obj$data
#                        mod_obj$data$scl
#                        mod_obj$data$cen
##########################

library(compiler)
library(RcppRoll)

#' Transform data
#'
#' @param obj : data.frame
#'
#' @return
#' @export
#'
#' @description
#'
#'
#' @examples
Reach <- function(fa, fb, fc, fGRPs) {
  # Return reach data subject to fitted formula to a value r=a/(1+b*(GRPs/1000)^c)

  # fa = Alpha Coefficient in reach model
  # fb = Beta Coefficient in reach model
  # fc = Gamma Coefficient in reach model
  # fGRPs = single data point of GRPs at which to calculate reach

  fReach <- as.numeric(fGRPs > 0) * fa/(1 + fb * (fGRPs/1000)^fc)
  # Return calculated reach value
  return(fReach)

  # Example Use of Reach Function (solution=1.222065) test=Reach(0.79,-1,0.5,125)

}
RollingSum <- function(afGRPs, fDecay, nPeriod) {
  # Create a rolling sum of an AdStock to a vector of data

  # afGRPsMat = vector of GRP data
  # fDecay = single data point, decimal decay rate of media
  # nPeriod = integer value of number of observations to sum over

  afRollingSum <- roll_sumr(afGRPs, weights=sapply(nPeriod, function(x) ((1-fDecay)^((nPeriod-1):0))), normalize=F)
  afRollingSum[1:(nPeriod-1)] <- afGRPs[1:(nPeriod-1)]

  # Return the rolling sum of data
  return(afRollingSum)

  # Example use of Function test=RollingSum(afGRPs, 0.15, 4)
}
AdStock <- function(afGRPs, fdecayRate) {
  # Generate vector of AdStocked/Decayed GRPs as a function of input GRPs and decay rate to a value
  # y(t)=y(t-1)*d + x(t)

  # afGRPs = matrix (vertical vector) of GRP Data
  # fdecayRate = decimal version of decay rate

  afAdStockedGRPs <- Reduce(function(v,x) v *(1-fdecayRate) + x, x = afGRPs, accumulate = TRUE)
  #afAdStockedGRPsMat <- as.matrix(t(afAdStockedGRPsMat))
  # Return AdStocked GRPs Vector
  return(afAdStockedGRPs)

  # Example use of AdStock Function test=AdStock(data.matric(GRPs[3]),0.15)
}
adstock = cmpfun(AdStock)
AdResponse <- function(afGRPsMat, afCoeffsMat, params){
  # Generate the Effective Cover of a vector of input GRPs

  # afGRPs = vector of GRP data
  # afCoeffsMat = matrix (10 cols by 3 rows) of coefficients for reach models from 1+ to 10+
  # nEffFreq = integer value of Effective Frequency Parameter
  # nRecFreq = integer value of Recency Frequency Parameter
  # nPeriod = integer value of Response Period Parameter
  # fDecay = decimal value of decay rate parameter

  nEffFreq <- params[1]
  nRecFreq <- params[2]
  nPeriod <- params[3]
  fDecay <- params[4]

  # Define output matrix size
  fEffGRPs <- RollingSum(afGRPsMat, fDecay, nPeriod)
  fTotalEffGRPs <- adstock(afGRPsMat, fDecay)

  # sets a,b,c values based on Effective Frequency
  a <- afCoeffsMat[[1, nEffFreq]]
  b <- afCoeffsMat[[2, nEffFreq]]
  c <- afCoeffsMat[[3, nEffFreq]]

  # Edge case, if Recency Frequency is 0
  if(nRecFreq == 0){
    afAdResponse <- Reach(a, b, c, fTotalEffGRPs)
    return(afAdResponse);
  }

  # Edge case, if Recency Frequency and Effective Frequency is ==
  if(nRecFreq == nEffFreq){
    afAdResponse <- Reach(a, b, c, fEffGRPs)
    return(afAdResponse);
  }

  # a,b,c of fit curves
  a_s <- afCoeffsMat[1,]
  b_s <- afCoeffsMat[2,]
  c_s <- afCoeffsMat[3,]

  # create matrix for recency window grps
  recency_window_grps = matrix(0, nrow=length(afGRPsMat), ncol=1)
  # seqence of reach curves needed to run in order to find recency period
  reach_curves <- nRecFreq:(nEffFreq - 1)

  # recency GRP calculation
  one <- sapply(reach_curves, function(x) Reach(a_s[[x]], b_s[[x]], c_s[[x]], fEffGRPs))
  two <- sapply(reach_curves, function(x) Reach(a_s[[x + 1]], b_s[[x + 1]], c_s[[x + 1]], fEffGRPs))
  three <- sapply(reach_curves, function(x) Reach(a_s[[nEffFreq - x]], b_s[[nEffFreq - x]], c_s[[nEffFreq - x]], fTotalEffGRPs - fEffGRPs) )
  recency_window_grps <- rowSums(((one-two)*three/100))

  # calculate whether or not to add recency window grps
  equal_total_eff_grps <- (fTotalEffGRPs - fEffGRPs) > .001
  # effective GRPs
  eff_reach = Reach(a, b, c, fEffGRPs)
  # if within recency window, add recency window grps
  afAdresponse <- ifelse(equal_total_eff_grps, eff_reach + recency_window_grps, eff_reach)

  return(afAdresponse)
}
adr = cmpfun(AdResponse)
AdStockPD <- function(data, i, p){
  rowSums(as.data.frame(embed(c(rep(NA, p), data), p + 1) %*% ((1 - i) ^ seq(0,p,1))),na.rm = F)->output
  output[is.na(output)] <- 0
  return(output)
}

# data = mod_obj1$data$BROOKS_MAG
# i = .3
# p = 4
# rowSums(as.data.frame(embed(c(rep(NA, p), data), p + 1) %*% ((1 - i) ^ seq(0,p,1))),na.rm = F)
#obj = mod_obj

Transform = function(obj, print=TRUE) {
  x <- obj$data
  spec <- obj$spec
  fit_curves <- obj$fit_curves
  output <- list()
  for (i in 1:nrow(spec)) {
    if(spec$Transform[i] == "Y") {
      if(print) { cat("transform ", spec$Orig_Variable[i], "\n") }
      type <- unlist(strsplit(spec$TransformType[i], "_"))
      type <- toupper(type)
      data_vector <- x[[spec$Orig_Variable[i]]]
      for(j in 1:length(type)) {
        data_vector_transform <- list()
        if (type[j] == "ADSTOCK"){
          data_vector_transform <- AdStockPD(data_vector, spec$Decay[i], spec$Period[i])
        }
        if (type[j] == "ADSTOCKV2"){
          data_vector_transform <- adstock(data_vector, spec$Decay[i])
        }
        if (type[j] == "ADR"){
          data_vector_transform <- adr(data_vector, fit_curves, c(spec$Effective[i], spec$Recency[i], spec$Period[i], spec$Decay[i]))
        }
        if (type[j] == "LAG") {
          data_vector_transform <- lag(data_vector, spec$Lag[i], default = 0)
        }
        if (type[j] == "LOG") {
          data_vector_transform <- log(data_vector*spec$Scale[i] + 1)
        }
        if (type[j] == "POLY") {
          data_vector_transform <- myPoly(data_vector, spec$Alpha[i])
        }
        if (type[j] == "MA") {
          data_vector_transform <- rollmean(data_vector, spec$Window[i], align="right", fill=NA)
          data_vector_transform[which(is.na(data_vector_transform))] <- data_vector[which(is.na(data_vector_transform))]
        }
        if (type[j] == "MC") {
          data_vector_transform <- scale(data_vector)
          scl <- attr(data_vector_transform, "scaled:scale")
          cen <- attr(data_vector_transform, "scaled:center")
        }
        if (type[j] == "STEIN") {
          data_vector_transform <- shrinker(data_vector, bw=spec$Window[i], trim=spec$Trim[i])
        }
        if (type[j] == "CPT") {
          data_vector_transform <-
            cpt.meanvar(data_vector, minseglen = 6, penalty = "CROPS", pen.value = c(0, 100), method = "PELT")
        }
        if (type[j] == "POWER") {
          data_vector_transform <- (data_vector)^spec$Power[i]
        }
        if (type[j] == "NONE") {
          data_vector_transform <- data_vector
        }
        data_vector <- data_vector_transform
      }
      x[spec$Trans_Variable[i]] <- data_vector
      if (type[j] == "MC") {
        x$scl <- scl
        x$cen <- cen
      }
    }
    output <- x
  }
  obj$data <- output
  return(obj)
}


Transform_panel = function(obj, print=TRUE) {
  
  spec <- obj$spec
  fit_curves <- obj$fit_curves
  split_data <- base::split(obj$data, obj$data[[obj$CS]])
  output <- list()
  for (k in 1:length(split_data)) {
    x <- split_data[[k]]
    x <- x[order(x[[obj$Time]]), ]  
    cat("\n\n", "transform cross section", x[[obj$CS]][1], "...\n")
    for (i in 1:nrow(spec)) {
      if(spec$Transform[i] == "Y") {
        if(print) { cat("transform ", spec$Orig_Variable[i], "\n") }
        type <- unlist(strsplit(spec$TransformType[i], "_"))
        type <- toupper(type)
        data_vector <- x[[spec$Orig_Variable[i]]]
        for(j in 1:length(type)) {
          data_vector_transform <- list()
          if (type[j] == "ADSTOCK"){
            data_vector_transform <- AdStockPD(data_vector, spec$Decay[i], spec$Period[i])
          }
          if (type[j] == "ADSTOCKV2"){
            data_vector_transform <- adstock(data_vector, spec$Decay[i])
          }
          if (type[j] == "ADR"){
            data_vector_transform <- adr(data_vector, fit_curves, c(spec$Effective[i], spec$Recency[i], spec$Period[i], spec$Decay[i]))
          }
          if (type[j] == "LAG") {
            data_vector_transform <- lag(data_vector, spec$Lag[i], default = 0)
          }
          if (type[j] == "LOG") {
            data_vector_transform <- log(data_vector*spec$Scale[i] + 1)
          }
          if (type[j] == "POLY") {
            data_vector_transform <- myPoly(data_vector, spec$Alpha[i])
          }
          if (type[j] == "MA") {
            data_vector_transform <- rollmean(data_vector, spec$Window[i], align="right", fill=NA)
            data_vector_transform[which(is.na(data_vector_transform))] <- data_vector[which(is.na(data_vector_transform))]
          }
          if (type[j] == "MC") {
            data_vector_transform <- scale(data_vector)
            scl <- attr(data_vector_transform, "scaled:scale")
            cen <- attr(data_vector_transform, "scaled:center")
          }
          if (type[j] == "STEIN") {
            data_vector_transform <- shrinker(data_vector, bw=spec$Window[i], trim=spec$Trim[i])
          }
          if (type[j] == "CPT") {
            data_vector_transform <-
              cpt.meanvar(data_vector, minseglen = 6, penalty = "CROPS", pen.value = c(0, 100), method = "PELT")
          }
          if (type[j] == "POWER") {
            data_vector_transform <- (data_vector)^spec$Power[i]
          }
          if (type[j] == "NONE") {
            data_vector_transform <- data_vector
          }
          data_vector <- data_vector_transform
        }
        x[spec$Trans_Variable[i]] <- data_vector
        if (type[j] == "MC") {
          x$scl <- scl
          x$cen <- cen
        }
      }
      output[[k]] <- x
    }
    
  }
  output <- do.call("rbind", output)
  obj$data <- output
  return(obj)
}



######################################
# adr transformation function
# 2022-06-15: written by Mark Reggimenti (all the math behind the calculations) and Julia Liu (R)
# this function takes a vector of GRPs and does adr transformation.
# x : vector of GRPs (sorted by time);
# fitcurves : a data frame of a, b, c for 1+, 2+, and etc.
# E : effective (an integer 1 to however many frequency in the fit curve)
# R : Recency (an integer of 0 <= R <=E)
# P : period in terms of days ( an integer with values of 7, 14, 21, or 28 etc.)
# D : adstock decay rate with value 0<D<1
# update notes:
# 2022-06-21 Julia Liu : per Mark R, P=P+1
# 2022-07-07 Julia Liu : switched to new abc formula. 
#                        Users need to make sure the b's in fit curves need to be adjusted
# 2022-07-13 Julia Liu : cap the Period given decay D
######################################



# the new abc formula
reach <- function (a, b, c, g) {
  reach <- ifelse(g>0, a/(1 + (g/b)^c), 0)
  return(reach)
}

library(compiler)
adstock <- function(afGRPs, fdecayRate) {

  afAdStockedGRPs <- Reduce(function(v,x) v *(1-fdecayRate) + x, x = afGRPs, accumulate = TRUE)
  # Return AdStocked GRPs Vector
  return(afAdStockedGRPs)
}
adstock = cmpfun(adstock)

max_period = function(D=D, threshold = 0.05) {
  p <- log(threshold)/log(1-D)
  p <- round(p-1) * 7
  p <- ifelse(p <=0, 7, p)
  return(p)
}

grps_R_NR <- function(df, P, D) {
  v1 <- paste0("R_P", P)
  v2 <- paste0("NR_P", P)
  
  n <- P/7-1
  
  df[[v1]] = df$grps
  if(n>0) {
  for (i in 1:n) {
    df[[v1]] <- df[[v1]] + dplyr::lag(df$grps,i, default =0)*(1-D)^i
  }
  }
  df[[v2]] <- df$grps_adstk - df[[v1]]
  return(df)
}

adrnew <- function(x, fitcurves, E, R, P, D, date_level = "week"){

  # QA the inputs
  if(is.null(x) | is.null(fitcurves) | is.null(E) | is.null(R) | is.null(P) | is.null(D) ) {
    stop("please specify the auguments of the function. \n")
  }
  
  if(E > ncol(fitcurves)) {
    stop("The E is greater than the maximum frequency in the fit curve file. \n")
  }
  
  if(R > E) {
    stop("Recency can not be greater than E. \n")
  }
  
  P = P+7    # per Mark R.
  if(P <= 0 && P %% 7 !=0) {
    stop("The period parameter is in terms of days, it should be more than 0, and should be divisible by 7.")
  }
  
  max_p <- max_period(D, threshold = 0.01)
#  if(P > max_p) {
#    stop("Period P should be less or equal to ", max_p)
#  }
  
  if(D > 1) {
    stop("Decay (D) should be less than 1. \n")
  }
  
  fitcurves <- data.frame(fitcurves)
  
  # calculates adstocks
  x_adstk <- adstock(x, D)
  df <- data.frame(grps = x, grps_adstk=x_adstk)
  df <- grps_R_NR(df, P=P, D=D)

  v1 <- paste0("R_P", P)
  v2 <- paste0("NR_P", P)
  # calculate various reaches  
  for(i in 1:ncol(fitcurves)) {
    v3 <- paste0("R_reach", i, "plus")
    v4 <- paste0("R_reach", i)
    v5 <- paste0("NR_reach",i, "plus")
    df[[v3]] <- reach(fitcurves[1,i], fitcurves[2,i], fitcurves[3,i], df[[v1]])
    if(i < ncol(fitcurves)) {
      df[[v4]] <- df[[v3]]-reach(fitcurves[1,i+1], fitcurves[2,i+1], fitcurves[3,i+1], df[[v1]])
      #df[[v5]] <- reach(fitcurves[1,i], fitcurves[2,i], fitcurves[3,i], df[[v2]])
    } else {
      df[[v4]] <- df[[v3]]
    }
    df[[v5]] <- reach(fitcurves[1,i], fitcurves[2,i], fitcurves[3,i], df[[v2]])
  }
  
  if(E > R) {
    add1 <- paste0("R_reach", E, "plus")
    #add2 <- paste0("R_reach", seq(1, E-R, by=1))
    add2 <- paste0("R_reach", seq(R, E-1, by=1))
    # going backward to pair with add2
    add3 <- paste0("NR_reach", seq(E-R, 1, by=-1), "plus")
  } else {
    add1 <- paste0("R_reach", E, "plus")
  }
  
  if(R == 0) {    # when R==0
    df$grps_adr <- reach(fitcurves[1,E], fitcurves[2,E], fitcurves[3,E], df$grps_adstk)
  } else {
  if(E == R) {
    df$grps_adr <- df[[add1]]
  } else {
    tmp = rep(0, nrow(df))
    for(k in 1:length(add2)) {
      tmp <- tmp + (df[[add2[k] ]] * df[[add3[k] ]])/100
    }
    df$grps_adr <- df[[add1]] + tmp
  }
  }
  
  return(df$grps_adr)    # return the adr transformed grps as a vector
  #return(df)              # return the result as a dataframe for QA
}
adrnew <- cmpfun(adrnew)


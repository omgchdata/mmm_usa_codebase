#####################################
# fit abc function using orthogonal nonlinear least squares regression (onls)
# xaxis : a vector of x-axis of the curve
# yaxis : a vector of y-axis of the curve
# the output of the function is a dataframe with columns of "a", "b", and "c"
######################################
library(onls)

abc_onls = function(xaxis, yaxis) {
  abc_mod <- onls(yaxis ~ a/(1+b* xaxis^c), start=list(a=max(yaxis),b=max(xaxis),c=-0.5),
                  control = list(maxiter=1024, maxfev=1024),
                  lower = c(0,0,-Inf), upper=c(Inf, Inf, -0.1))
  res <- data.frame(t(coef(abc_mod)))
  return(res)
}

abc_onls_v2 = function(xaxis, yaxis) {
  abc_mod <- onls(yaxis ~ a0 + a/(1+b* xaxis^c), 
                  start=list(a0=min(yaxis), a=max(yaxis),b=max(xaxis),c=-0.5),
                  control = list(maxiter=1024, maxfev=1024),
                  lower = c(-Inf, 0,0,-Inf), upper=c(Inf, Inf, Inf, -0.1))
  res <- data.frame(t(coef(abc_mod)))
  return(res)
}


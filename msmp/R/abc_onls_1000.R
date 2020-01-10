#####################################
# fit abc function using orthogonal nonlinear least squares regression (onls)
# xaxis : a vector of x-axis of the curve
# yaxis : a vector of y-axis of the curve
# the output of the function is a dataframe with columns of "a", "b", and "c"
######################################
library(onls)
#####################
# ABC function
#####################
abc <- function(x, a, b, c) {
  d <- a/(1+b*x^c)
  return(d)
}

abc_1000 <- function(x, a, b, c) {
  d <- a/(1+b*(x/1000)^c)
  return(d)
}

############################
# first derivative of ABC function
############################
dabc <- function(x, a, b, c) {
  d <- (-a*b*c*x^(c-1))/((1+b*x^c)^2)
  return(d)
}

dabc_1000 <- function(x, a, b, c) {
  d <- (-a*b*c*1000^c*x^(c-1))/((1000^c + b*x^c)^2)
  return(d)
}

###################################
# onls fit to the abc_1000 function
###################################
abc_onls_1000 = function(xaxis, yaxis) {
  abc_mod <- onls(yaxis ~ a/(1+b* (xaxis/1000)^c), start=list(a=max(yaxis),b=max(xaxis/1000),c=-0.5),
                  control = list(maxiter=1024, maxfev=1024))
  res <- data.frame(t(coef(abc_mod)))
  return(res)
}


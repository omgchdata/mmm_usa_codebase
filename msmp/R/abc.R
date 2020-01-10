
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

############################
# 2nd derivative of ABC function
############################
ddabc <- function(x, a, b, c) {
  up <- -a*b*c*(c-1)*x^(c-2) * (1+b*x^c) + 2*a*b^2*c^2*x^(2*c-2)
  down <- (1+b*x^c)^3
  d <- up/down
  return(d)
}




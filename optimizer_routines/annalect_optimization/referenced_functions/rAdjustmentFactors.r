# File to provide Template Adjust Coeff Function for ChannelPlanning

AdjustCoeff=function(afBaseCoeffs, afAdjustmentMatrix, afQuestions){
  #afBaseCoeffs - dataframe of n Rows of Media with 4 columns - media name, a, b, c params
  #afAdjustmentMatrix - dataframe of n*3 Rows of Media and param information with 7 columns (media name, param, FactContact, FactSize, FactPurchase, FactOnline)
  #afQuestions - dataframe of 4 rows of question answers on scale 0-100
  
  afOutput = as.matrix(afBaseCoeffs[,2:4])
  afOutCoeff = as.matrix(afBaseCoeffs[,2:4])
  sliders = (afQuestions[,1]-50)/100.0
  for(i in 1:nrow(afBaseCoeffs)){
    for (j in 1:3) {
      adjFactors = as.numeric(as.matrix(afAdjustmentMatrix[j+(i-1)*3,3:6]))
      fAdjust = sliders %*% adjFactors
      afOutput[i,j] = as.numeric(as.matrix(afBaseCoeffs[i,j+1]))*(1+fAdjust)
    }
    
#    A = as.numeric(afOutput[i,1])
#    B = as.numeric(afOutput[i,2])
#    reachCoeffs = BrandToReachFxnCoeffs(A,B)
#    afOutCoeff[i,] = reachCoeffs
  }
  
  return(cbind(afBaseCoeffs[,1],as.data.frame(afOutput)))

}

reachFxn <- function(x,alpha,beta,gamma) {

y = alpha / (1 + beta * (x/1000)^gamma)

}


brandFxn <- function(x,A,B) {
  # y = A arctan(x/B)   
  y = A * atan (x / B)
}

BrandVsReachDistance <- function(par, x, A, B) {
  # convert brandFxn coeff to a reachFxn with equivalent coeffs   
  alpha = A * pi / 2 # analytic value
  beta = par[1] * B
  gamma = par[2]
  y1 = brandFxn(x,A,B)
  y2 = reachFxn(x,alpha,beta,gamma)
  diff = sum((y1-y2)^2)
}

BrandToReachFxnCoeffs <- function(A,B) {
  # initial guess for beta and gamma
  par = c(2, -1.1)
  x = c(1:1000)
  out <- optim(par,fn=BrandVsReachDistance,x=x,A=A,B=B)
  alpha = A * pi / 2 
  beta = out$par[1] * B
  gamma = out$par[2]
  
  return (c(alpha,beta,gamma))
}


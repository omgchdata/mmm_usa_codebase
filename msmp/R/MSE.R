MSE <- function(y, yhat) {
  mse <- sum((y-yhat)^2)/length(y)
  return(mse)
}

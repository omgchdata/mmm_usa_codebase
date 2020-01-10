MAPE = function(a, f) {
  mape = mean(abs(a-f)/a)
  return(mape)
}

MAPE_DF = function(df, a, f) {
  mape = mean(abs(df[[a]] - df[[f]])/df[[a]])
  return(mape)
}


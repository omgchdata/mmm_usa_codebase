MAPE = function(a, f) {
  mape = ifelse(a==0, NA, abs(a-f)/a)
  mape = mean(mape, na.rm=T)
  return(mape)
}

MAPE_DF = function(df, a, f) {
  mape = ifelse(df[[a]]==0, NA,(abs(df[[a]] - df[[f]])/df[[a]]))
  mape = mean(mape, na.rm=T)
  return(mape)
}


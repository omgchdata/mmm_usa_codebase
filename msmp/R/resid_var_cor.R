
resid_var_cor = function(obj, v) {

resid <- obj$Model$residuals

decay <- seq(0.1, 0.9, 0.1)
period <- seq(1, 10, 1)

result <- data.frame(expand.grid(decay, period))
names(result) <- c("Decay", "Period")
for (k in 1:length(v)) {
result[[v[k]]] <- 0
x1 <- obj$data[[v[k] ]]
for (i in period) {
  for (j in decay) {
    x2 <- AdStockPD(x1, j, i)
    result[[v[k]]][result$Decay == j & result$Period == i] <- cor(resid, x2)
  }
}
}
result <- result[order(result[[v]], decreasing = T), ]
return(result)
}

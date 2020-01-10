

fitABC_DF_1000 = function(rc) {
#  v2 <- names(rc)[grep("_contribution", names(rc))]
  v2 <- names(rc)[grep("rc_", names(rc))]
  v1 <- names(rc)[grep("_spend", names(rc))]
#  SimuVar <- gsub("_contribution", "", v2)
  SimuVar <- gsub("rc_", "", v2)

  result <- data.frame(SimuVar)
  result$a <- 0
  result$b <- 0
  result$c <- 0

  for (i in 1:length(SimuVar)) {

      cat("working on", SimuVar[i], "\n")

      abc_result <- abc_onls_1000(xaxis=rc[[ v1[i] ]], yaxis=rc[[ v2[i] ]])
      result$SimuVar[i] <- SimuVar[i]
      result$a[i] <- abc_result$a
      result$b[i] <- abc_result$b
      result$c[i] <- abc_result$c
      v3 <- paste("fit", SimuVar[i], sep="_")
      rc[[v3]] <- abc_1000(rc[[v1[i] ]], result$a[i], result$b[i], result$c[i])
      
      p = ggplot(rc, aes(x=rc[[v1[i] ]])) +
        geom_point(aes(y=rc[[v2[i] ]], colour="ResponseCurve"), size=1) +
        geom_line(aes(y=rc[[v3 ]], colour="Orthogonal NLS fit"), size=1) + 
        xlab(v1[i] ) + ylab(SimuVar[i]) 
      print(p) 
      readline("hit Enter to see the next response curve ...")

  }
  
  return(result)
}





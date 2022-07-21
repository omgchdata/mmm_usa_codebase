##############################
# This function fits response curves using the abc function
# update notes:
# Julia Liu 2021/10/20: added a if statement to make sure the response curve is non-zero
# Julia Liu 2022/07/20 : added the new abc formula. 
#                        by default, the fitter fits using this abc formula:  a/(1+b*x^c)
#                        set newABC=TRUE to use the new formula : a/(1 + (x/b)^c)
#######################################
library(onls)

fitABC = function(obj = mod_obj, newABC = FALSE, showPlot=FALSE) {
  spec <- obj$spec
  rc <- obj$ResponseCurve
  SimuVar <- spec$Orig_Variable[toupper(spec$Simulate) == "Y" & spec$Include == 1]

  result <- data.frame(SimuVar)
  result$Spend <- 0
  result$a <- 0
  result$b <- 0
  result$c <- 0
  p <- list()
  for (i in 1:length(SimuVar)) {
    v1 <- spec$Spent_Variable[spec$Orig_Variable == SimuVar[i]]
    if( !is.na(v1)) {
      cat("working on", v1, "\n")
      v2 <- paste("rc", SimuVar[i], sep="_")
      if(sum(rc[[v2]]) >0 & sum(rc[[v1]] >0 )) {
        #abc_result <- abc_onls(xaxis=rc[[v1]], yaxis=rc[[v2]])
        xaxis <- rc[[v1]]
        yaxis <- rc[[v2]]
        if(newABC) {
          abc_mod <- onls(yaxis ~ a/(1+(xaxis/b)^c), start=list(a=max(yaxis),b=mean(xaxis),c=-0.8),
                          control = list(maxiter=1024, maxfev=1024))
        } else {
          abc_mod <- onls(yaxis ~ a/(1+b* xaxis^c), start=list(a=max(yaxis),b=max(xaxis),c=-0.8),
                          control = list(maxiter=1024, maxfev=1024))
        }
        abc_result <- data.frame(t(coef(abc_mod)))
        result$SimuVar[i] <- SimuVar[i]
        result$a[i] <- abc_result$a
        result$b[i] <- abc_result$b
        result$c[i] <- abc_result$c
        result$Spend[i] <- rc[[v1]][rc$Delta==0]
        v3 <- paste("fit", SimuVar[i], sep="_")
        if(newABC) {
          rc[[v3]] <- abcNew(rc[[v1]], result$a[i], result$b[i], result$c[i])
        } else {
          rc[[v3]] <- abc(rc[[v1]], result$a[i], result$b[i], result$c[i])
        }
        p[[i]] = ggplot(rc, aes(x=rc[[v1]])) +
          geom_point(aes(y=rc[[v2]], colour="ResponseCurve"), size=1) +
          geom_line(aes(y=rc[[v3]], colour="Orthogonal NLS fit"), size=1) + 
          xlab(v1) + ylab(SimuVar[i]) 
        if(showPlot) {
          print(p[[i]])
          readline("hit Enter to see the next response curve ...")
        }
      } else {
        result$SimuVar[i] <- SimuVar[i]
        result$a[i] <- NA
        result$b[i] <- NA
        result$c[i] <- NA
        result$Spend[i] <- rc[[v1]][rc$Delta==0]
      }
# comment nlsLM tends not converge, decided to just use onls()
# another method, using nls.
#      abc_mod <- nlsLM(v2 ~ a/(1+b* v1^c), start=list(a=10000,b=1000,c=-0.5), data=tmp,
#                      control = list(maxiter=1024, maxfev=1024))
#      result$SimuVar[i] <- SimuVar[i]
#      result$a[i] <- coef(abc_mod)[1]
#      result$b[i] <- coef(abc_mod)[2]
#      result$c[i] <- coef(abc_mod)[3]
#      v4 <- paste("fit2", SimuVar[i], sep="_")
#      rc[[v4]] <- Reach(rc[[v1]], result$a[i], result$b[i], result$c[i])

    }
  }
  names(result) <- c("Media", "Spend", "A", "B", "C")
  obj$ResponseCurve <- rc
  obj$abc <- result
  obj$rc_fit_plots <- p[[1]]
  return(obj)
}





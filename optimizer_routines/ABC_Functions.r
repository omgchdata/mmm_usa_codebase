createABC <- function(input){

  # Load data from CSV file
  curvedata <- input

  revcols <- ncol(curvedata)
  curvecolname <- colnames(curvedata)[c(2:revcols)]

  # Initial variables
  coefmain <- NULL
  type <- rep("NLS",(revcols-1))
  coefY <- rep(0,3)
  R2 <- NULL

  # Minimise residual sum of squares function
  min.RSS <- function(data, par) {
    with(data, sum(((par[1]/(1+par[2]*(x^par[3])))-y)^2))
  }

  # Calculate pseudo E2 value

  fit <- function(data,par) {
    TSS <- sum((y-mean(y))^2)
    RSS <- with(data, sum(((par[1]/(1+par[2]*(x^par[3])))-y)^2))
    1-(RSS/TSS)
  }


  # Loop through revenue columns

  for(i in 2:revcols){

    # Split data
	x <- unname(unlist(curvedata[,1]))
    yi <- assign(paste("y", i, sep = ""), unname(unlist(curvedata[,i])))
    y <- yi[!is.na(yi)]
    x <- x[c(1:length(y))]

    # Extract trial coefficients
    lm1 <- suppressWarnings(lm(log(max(y)/(y-1))~ 1+log(x),singular.ok = T))
    A <- max(y)
    B <- exp(coef(lm1)[1])
    C <- coef(lm1)[2]

    dat <- data.frame(c(x), c(y))
    result <- list()

    # Attempt to fit NLS model, if singular or other error, fit best alternative
    tryCatch(coefy<-coef(nls(y ~ A/(1+B*(x)^C), start=list(A=A,B=B,C=C), control = list(maxiter = 5000))), error=function(e){

      result[[1]] <<- optim(par = c(max(y), exp(coef(lm1)[1]), coef(lm1)[2]),
                      min.RSS, data = dat, method="L-BFGS-B",lower=c(0,0,-1000),upper=c(10^30,10^30,-0.0001))
      result[[2]] <<- optim(par = c(max(y), exp(coef(lm1)[1]), coef(lm1)[2]),
                       min.RSS, data = dat, method="BFGS")
      result[[3]] <<- optim(par = c(max(y), exp(coef(lm1)[1]), coef(lm1)[2]),
                       min.RSS, data = dat, method="CG")
      result[[4]] <<- optim(par = c(max(y), exp(coef(lm1)[1]), coef(lm1)[2]),
                           min.RSS, data = dat, method="Nelder-Mead")


      R3 <<- c(fit(dat,result[[1]]$par),fit(dat,result[[2]]$par),fit(dat,result[[3]]$par),fit(dat,result[[4]]$par))
      coefy <<- result[[which.max(R3)]]$par
      type[i-1] <<- "OLS"
    })

    R2[i-1] <- fit(dat,coefy) #pseudo R2 value
    coefmain <- cbind(coefmain, coefy)
  }
  final.data <<- rbind(R2,coefmain,type)

  # Write CSV file
  colnames(final.data) <- curvecolname
  rownames(final.data) <- c("R2","a","b","c","Fit type")
  return(t(final.data))

}

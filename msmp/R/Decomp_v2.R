
#' This function takes model objects and does decomposition based on model form.
#' obj (list) : model object which is created by running Run_Model
#' incl_spent : 
#'
# Output : a object that contains all the decomp result
#'
#' @examples
Decomp <- function(obj, incl_spent = FALSE) {


  x <- obj$data
  spec <- obj$spec
  b <- obj$Model$coefficients
  b$Error <- NULL
  b$Tvalue <- NULL
  b$Variables <- paste("beta", b$Variables, sep="_")
  b <- tidyr::spread(b, Variables, Estimate)
  depvar <- obj$spec$Trans_Variable[obj$spec$Variable_Type=="Dependent"]
  depvar_orig <- obj$spec$Orig_Variable[obj$spec$Variable_Type=="Dependent"]
  cs <- obj$CS
  if (depvar==depvar_orig) {
    decomp <- x[, c(obj$Time, cs, depvar)]
  } else {
    decomp <- x[, c(obj$Time, cs, depvar_orig, depvar)]
  }
  
  x <- left_join(x, b)     # dataframe that contains the data and betas
  calc_var <- spec$Trans_Variable[spec$Variable_Type!="Dependent"]
  dcmp_var <- paste("d", calc_var, sep="_")
  beta_var <- paste("beta", calc_var, sep="_")
  #print(dcmp_var)

  if(length(calc_var) <= 0) {
    stop("Please specify which variables you would like to decomp. ")
  }

  if(toupper(obj$ModelForm) == "LIN_LIN") {
    for(i in 1:length(calc_var)) {
      cat("Calculate decomposition of ", calc_var[i], "\n")
      decomp[[dcmp_var[i] ]] <- x[[beta_var[i]]] * x[[calc_var[i] ]]
      if(toupper(spec$TransformType[spec$Trans_Variable == depvar]) == "MC") {
        if(tolower(dcmp_var[i]) == "d_intercept") {
          decomp[[dcmp_var[i] ]] <- unscale( decomp[[dcmp_var[i] ]], x[[depvar_orig]], center=T, scale=T)
        } else {
          decomp[[dcmp_var[i] ]] <- unscale( decomp[[dcmp_var[i] ]], x[[depvar_orig]], center=F, scale=T)
        }
        
      }
    }

  } else if(toupper(obj$ModelForm) == "LOG_LOG") {
    stop("Sorry, haven't implemented yet. ")
    cat("The reference point is based on ", Reference, "\n")
    for(i in 1:length(calc_var)) {

    }

  }

# include spent variable
  if(incl_spent) {
    for (i in 1:length(calc_var)) {
      spend_var <- spec$Spent_Variable[spec$Trans_Variable == calc_var[i]]
      if( !is.na(spend_var) ) {
        decomp[[spend_var]] <- x[[ spend_var ]]
      }
    }
  }
  
  obj$Decomposition <- decomp
  return(obj)
}


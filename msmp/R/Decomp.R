#' Decomp
#'
#' @param obj: list: Model
#' @param Reference: char: Reference point
#'
#' @return
#' @export
#'
#' @description
#'
#' This function takes model objects and does decomposition based on model form.
#' obj (list) : model object which is created by running Run_Model
#' Reference (char) : reference point for log model. default to min.
#'
# Output : a object that contains all the decomp result
#'
#' @examples
Decomp <- function(obj, incl_spent = FALSE) {


  x <- obj$data
  spec <- obj$spec
  b <- obj$Model$coefficients
  depvar <- obj$spec$Trans_Variable[obj$spec$Variable_Type=="Dependent"]
  decomp <- x[, c(obj$Time, depvar)]

  calc_var <- spec$Trans_Variable[spec$Variable_Type!="Dependent"]
  dcmp_var <- paste("d", calc_var, sep="_")
  #print(dcmp_var)

  if(length(calc_var) <= 0) {
    stop("Please specify which variables you would like to decomp. ")
  }

  if(toupper(obj$ModelForm) == "LIN_LIN") {
    for(i in 1:length(calc_var)) {
      cat("Calculate decomposition of ", calc_var[i], "\n")
      decomp[[dcmp_var[i] ]] <- b$Estimate[b$Variables == calc_var[i]] * x[[calc_var[i] ]]

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


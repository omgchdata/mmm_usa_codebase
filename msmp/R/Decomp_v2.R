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
Decomp <- function(obj) {
  
 # obj <- mod_obj
  x <- obj$data
  spec <- obj$spec
  varyby_var <- unique(spec$VaryBy)
  varyby_var <- setdiff(varyby_var,"None")
  
  b <- obj$Model$coefficients
  b$Error <- NULL
  if("Tvalue" %in% names(b)) {
    b$Tvalue <- NULL
  }
  
  b$Variables <- paste0("beta_", b$Variables)
  b <- spread(b, Variables, Estimate)
  x[[varyby_var]] <- as.character(x[[varyby_var]])
  x <- left_join(x, b)
  
  depvar <- obj$spec$Trans_Variable[obj$spec$Variable_Type=="Dependent"]
  decomp <- x[, c(varyby_var, obj$Time, depvar)]

  calc_var <- spec$Trans_Variable[spec$Variable_Type!="Dependent"]
  dcmp_var <- paste("d", calc_var, sep="_")
  beta_var <- paste("beta", calc_var, sep="_")
  if("decomp_ref" %in% tolower(names(spec))) {
    ref_val <- spec$decomp_ref[spec$Variable_Type!="Dependent"]
    cat("decomp reference point is defined. \n")
  }
  #print(dcmp_var)

  if(length(calc_var) <= 0) {
    stop("Please specify which variables you would like to decomp. ")
  }

  if(toupper(obj$ModelForm) == "LIN_LIN") {
    for(i in 1:length(calc_var)) {
      cat("Calculate decomposition of", calc_var[i], "with reference point of", ref_val[i], "\n")
      decomp[[dcmp_var[i] ]] <- x[[ beta_var[i] ]] * (x[[calc_var[i] ]]- ref_val[i])
    }

  } else if(toupper(obj$ModelForm) == "LOG_LOG") {
    stop("Sorry, haven't implemented yet. ")
  }

#  (x[[3]]/x[[4]]) %>% as.data.frame(.) -> TIV
#  
#  TIV$. * decomp[,3:ncol(decomp)] -> decomped
#  
#  bind_cols(x[,1:2], decomped) -> decomp
  
  obj$Decomposition <- decomp
  return(obj)
}


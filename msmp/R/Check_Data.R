
#' Check Data
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @description
#'
#' This funciton checks data issues:
#' - missing value (zero out the missing value if any)
#' - check the model spec file against the model dataset,
#'   and see whether there are any missing variables.
#' - sort data by DMA by time
#' - check any time gap (future)
#' It returns the system objects with all the issue fixed.
#'
#' @examples
#'
Check_Data = function(obj) {

  x <- obj$data
  Model_Spec <- obj$spec

  # check missing values
  chk_missing <- data.frame(apply(is.na(x),2,sum))
  names(chk_missing) <- "num_NAs"
  chk_missing$variables <- row.names(chk_missing)

  missing_vars <- chk_missing$variables[chk_missing$num_NAs > 0]
  if(length(missing_vars) > 0) {     # zero out missing cells if any
    for (i in 1:length(missing_vars) ){
      x[[missing_vars[i]]][is.na(x[[missing_vars[i]]])] <- 0
    }
  } else {
    print("There is no missing values in _ModelData.csv")
  }

  # check missing variables
  DepVar <- Model_Spec$Orig_Variable[Model_Spec$Variable_Type == "Dependent"]
  IV <- Model_Spec$Orig_Variable[Model_Spec$Variable_Type != "Dependent"]

  if(length(DepVar) == 0) {
    stop("Please define/include dependent variable in _Variables.csv. ")
  } else if(length(DepVar) > 1) {
    stop("You have listed more than 1 dependent variables in the _Variables.csv. ")
  } else {
    if(!(DepVar %in% names(x) ) )
      stop("The dependent variable ", DepVar, " is not in the model dataset. ")
  }

  if("Intercept" %in% (Model_Spec$Orig_Variable)) {
    x$Intercept = 1
  }

# needs at least 1 right hand side variable
  if(length(IV) == 0) {
    stop("Please define independent variables in the _Variables.csv.")
  }

# are all the IVs in the dataset?
  if(FALSE %in% (IV %in% names(x))) {
    stop("These independent variables are not in _ModelData.csv: \n",
#         setdiff(IV ,names(x)[IV %in% names(x)]),
          paste(IV[!(IV %in% names(x))], collapse = ", ") )
  } else {
    print("The model dataset contains all the model variables. ")
  }

# is there a time variable?
  if(!(obj$Time %in% names(x))) {
    stop("The time variable", obj$Time, "is not in the _ModelData.csv.")
  }
  

  if(toupper(obj$Panel == "Y")) {
    if(!is.null(obj$CS)) {
      if(!(obj$CS %in% names(x))) {
        stop("The cross section variable", obj$CS, "is not in the _ModelData.csv.")
      } else {
        x <- x[order(x[[obj$CS]], x[[obj$Time]]), ]   # sort by cs and time
      }
    }
  }
  
  obj$data <- x
  return(obj)
}

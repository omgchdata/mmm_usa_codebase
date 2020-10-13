

##################################
# this function checks data issues:
# - missing value (zero out the missing value if any)
# - check the model spec file against the model dataset,
#   and see whether there are any missing variables.
# - sort data by DMA by time
# - check any time gap (future)
# It returns the system objects with all the issue fixed.
# Update notes:
# 2020-09-14 Julia Liu : checks if the Orig_Variable and Trans_Variable names are the same or not:
#                        if Transform == Y: the Orig and Transformed name shouldn't be the same
#                        if Transform == N: they should be the same
#                        if the above doesn't satisfy, the R code stop and print out a message.
##################################

library(reshape2)
library(ggplot2)
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
  IV_t <- Model_Spec$Trans_Variable[Model_Spec$Variable_Type != "Dependent"]
  
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
  
  # check if Orig!=Trans_Variable when Transform==Y.
  ind <- which((Model_Spec$Orig_Variable != Model_Spec$Trans_Variable) != (toupper(Model_Spec$Transform) == "Y"))
  if(length(ind) >0) {
    stop("In the _Variables.csv file, this/these variable(s) ", paste(Model_Spec$Orig_Variable[ind], collapse = " , "), 
         " need attention. Please make sure Trans_Variable has differrent names from Orig_Variable when Tranform==Y and different when Transform==N. \n")
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
  
  data <- obj$data[, obj$spec$Orig_Variable[obj$spec$Include == 1]]
  #obj$cor_heatmap <- heatmap_cor(sel_data)
  
  return(obj)
}

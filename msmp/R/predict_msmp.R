
######################################################################
# predict_msmp:
# Julia Liu 2020-04-27 : created version 1
# this function scores/predicts given a msmp model object and new data.
# obj : the model object (a list object) that was generated when a MMM was 
#       ran using the msmp framework. It should at least contains the 
#       model specification (spec) and model coefficients.
# data :the new data in a dataframe. It should has the same format as the
#       _ModelData.csv, and it should contains all the original/raw varilablesin the 
#       model. It does not need to contain the transformed varialbles 
#       as the predict_msmp function will transform the variable acording
#       to the model specification.
# decomp : a logic (TRUE or FALSE). It is default to FALSE.
#          when it is set to T, the function returns "contribution" by each variable
#          in addition to the forecast. 
# Update notes:
# Julia Liu 2020-11-12: added decomp (a logic) argument. 
# Julia Liu 2022-05-16: added the predict_panel_msmp function
#                       reverse mc (mean-centering) if the dep var is mean centered. 
######################################################################
predict_msmp <- function(obj, data, decomp=F) {

  spec <- obj$spec[obj$spec$Include == 1, ]
  depvar <- spec$Trans_Variable[tolower(spec$Variable_Type) == "dependent"]

  coef <- obj$Model$coefficients
  b <- coef$Estimate
  iv <- coef$Variables
  
  # make sure the data contains all the variables that are needed to score the model
  for (i in 1:length(iv)) {
    if( !(iv[i] %in% spec$Trans_Variable)) {
      stop(iv[i], " is not in the spec/_Variable.csv")
    }
    orig_iv <- spec$Orig_Variable[spec$Trans_Variable == iv[i]]
    if( !(orig_iv %in% names(data))) {
      stop(orig_iv, " is the original variable of ", iv[i], " and it is not in the new dataset you have provided.")
    }
  }
  
  # transform the new data according to the original model specification
  if( !(obj$Time %in% names(data)) ) {
    stop("The time column", obj$Time, " is not in the dataset.")
  }
  
  # sort the data by time variable to prepare for transformation
  data <- data[order(data[[obj$Time]]), ]
  orig_data <- obj$data               # put aside the original model dataset 
  obj$data <- data
  obj <- Transform(obj, print=F)  # transform  the variables
  # create a new data dataframe 
  obj$new_data <- obj$data[, unique(c(obj$Time, depvar, spec$Orig_Variable, spec$Trans_Variable))]
  obj$data <- orig_data   # put back the original dataset


  X = as.matrix(obj$new_data[,iv])
  predict = X %*% b
  result <- data.frame(obj$new_data[[obj$Time]], predict)
  names(result) <- c(obj$Time, "predict")

  if(obj$spec$Transform[tolower(obj$spec$Variable_Type)=="dependent"] == "Y" &
     tolower(obj$spec$TransformType[tolower(obj$spec$Variable_Type)=="dependent"]) == "mc" ) {
    result$predict <- result$predict*obj$data$scl[1] + obj$data$cen[1]
  }
    

  if(decomp) {
    d_var = paste("d", iv, sep="_")
    for(i in 1:length(d_var)) {
      result[[d_var[i] ]] <- coef$Estimate[i] * obj$new_data[[ iv[i]]]
    }
  }
  
  return(result)
}


predict_panel_msmp <- function(obj, data, decomp=F) {
  cs <- unique(obj$Model$coefficients[[obj$CS]])
  
  # loop through panels and do forecasting one panel at a time
  pred <- list()
  for (i in 1:length(cs)) {
    cat("forecasting ", cs[i], "\n")
    obj2 <- obj
    obj2$data <- obj2$data[obj2$data[[obj2$CS]]== cs[i],]
    obj2$Model$coefficients <- obj2$Model$coefficients[obj2$Model$coefficients[[obj2$CS]]==cs[i],]
    data_tmp <- data[data[[obj2$CS]] == cs[i],]
    pred[[i]] <- predict_msmp(obj = obj2, data=data_tmp )
    pred[[i]][[obj2$CS]] <- cs[i]
  }
  
  
  pred_panel <- do.call("rbind", pred)
  return(pred_panel)
}

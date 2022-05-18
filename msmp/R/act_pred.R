
#############################################################
# this function creates actual vs predicted in a dataframe given the model object
# input : obj - the model object that has coefficients and data
# this function returns a dataframe that contains the acctual vs predicted
# update notes:
# Julia Liu 2021/10/11 : handles market share model
#############################################################
act_pred <- function( obj, share = NULL ) {
  spec <- obj$spec
  x = obj$data
  full_b <- obj$Model$coefficients
  full_b$Error <- NULL
  full_b$Tvalue <- NULL
  full_b$Variables <- paste0("beta_", full_b$Variables)
  full_b <- spread(full_b, Variables, Estimate)

  if(nrow(full_b) == 1) {
    x <- cbind(x, full_b)
  } else {
    x <- left_join(x, full_b)
  }
  x$predicted <- 0
  IV <- spec$Trans_Variable[spec$Include == 1 & tolower(spec$Variable_Type) != "dependent"]
  betaIV <- paste0("beta_", IV)
  depvar <- spec$Trans_Variable[tolower(spec$Variable_Type) =="dependent"]
  depvar_scale <- spec$Scale[tolower(spec$Variable_Type) =="dependent"]
  depvar_orig <- spec$Orig_Variable[tolower(spec$Variable_Type) =="dependent"]
  for (i in 1:length(IV)) {
    tmp <- x[[betaIV[i] ]] * x[[ IV[i] ]]
    if(tolower(spec$Transform[tolower(spec$Variable_Type) == "dependent"]) == "y" & tolower(spec$TransformType[tolower(spec$Variable_Type) == "dependent"]) == "mc") {
      tmp <- tmp * x$scl

    } else {    # not mean centered
      tmp <- x[[betaIV[i] ]] * x[[ IV[i] ]]
    }
    x$predicted <- x$predicted + tmp
  }
  if(tolower(spec$Transform[tolower(spec$Variable_Type) == "dependent"]) == "y" & tolower(spec$TransformType[tolower(spec$Variable_Type) == "dependent"]) == "mc") {
    x$predicted <- x$predicted + x$cen
  }
  
  if(tolower(spec$Transform[tolower(spec$Variable_Type) == "dependent"]) == "y" & tolower(spec$TransformType[tolower(spec$Variable_Type) == "dependent"]) == "log") {
    x$predicted <- (exp(x$predicted)-1)/depvar_scale
  }
  

  act_pred_df <- x[, c(obj$CS, obj$Time, depvar_orig, "predicted")]
  if( is.null(share)) {
    act_pred_df <- x[, c(obj$CS, obj$Time, depvar_orig, "predicted")]
    
  } else {
    act_pred_df <- x[, c(obj$CS, obj$Time, depvar_orig, "predicted", share)]
    act_pred_df[[depvar_orig]] <- act_pred_df[[depvar_orig]] * act_pred_df[[share]]
    act_pred_df$predicted <- act_pred_df$predicted * act_pred_df[[share]]
  }
  act_pred_df$residual <- act_pred_df[[depvar_orig]] - act_pred_df$predicted
  return(act_pred_df)
}
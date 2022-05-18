###############################
# update notes:
# 2020-05-04 Julia Liu : added actual vs predicted chart. It is store in mod_obj$act_pred_chart
###############################

Run_Model <- function(obj, Method="Bayes", loop = FALSE) {


  spec <- obj$spec
  x <- obj$data

  DepVar <- spec$Trans_Variable[spec$Variable_Type == "Dependent"]
  IV <- spec$Trans_Variable[spec$Variable_Type != "Dependent"]
  spec <- spec[spec$Variable_Type != "Dependent", ]
  priors <- dplyr::select(spec, Orig_Variable, Trans_Variable, Prior_Mean, Prior_SD)

  # do some checking and make sure the model variables (DepVar and IV) are in data
  if(DepVar %in% names(x)) {
    y = x[[DepVar]]
  } else {
    stop("The dependent variable", DepVar, "is not in the model dataset. ")
  }

  if("Intercept" %in% (spec$Trans_Variable)) {
    x$Intercept = 1
  }

  if(FALSE %in% (IV %in% names(x))) {
    stop("Some of the independent variables ",
         setdiff(IV ,names(x)[IV %in% names(x)]),
         " are not in the model x set. Please check _Variables.csv file. ")
  }

  eq <- (paste(IV, collapse =" + "))
  eq <- paste(eq, " -1")
  eq <- paste(DepVar, " ~ ", eq)
  eq <- as.formula(eq)

  eq_lm <- paste(IV[!IV %in% "Intercept"], collapse =" + ")
  eq_lm <- as.formula(paste(DepVar, " ~ ", eq_lm))


  print("calling my_bayes()...")
  bayes_obj <- my_bayes(formula=eq, data=x, priors=priors)
#  return(bayes_obj)
  obj$Model <- bayes_obj
  obj$Model$act_pred <- x[, c(obj$Time, DepVar)]
  obj$Model$act_pred$predicted <- obj$Model$fitted_value
  obj$Model$act_pred$residual <- obj$Model$residuals
  obj$Model$MAPE <- MAPE(obj$Model$act_pred[[DepVar]], obj$Model$act_pred$predicted)
  obj$lmModel <- lm(eq_lm, data=x)
  
  if(TRUE %in% unique(is.na(coef(obj$lmModel)))) {
    obj$Model$VIF <- data.frame(obj$Model$coefficients[, c("Variables")])
    names(obj$Model$VIF) <- "Variables"
    obj$Model$VIF$VIF <- NA
    obj$Model$VIF$Variables <- as.character(obj$Model$VIF$Variables)
  } else if( length(IV) <= 2 ) {
    obj$Model$VIF <- data.frame(obj$Model$coefficients[, c("Variables")])
    names(obj$Model$VIF) <- "Variables"
    obj$Model$VIF$VIF <- NA
    obj$Model$VIF$Variables <- as.character(obj$Model$VIF$Variables)
  }else {
    obj$Model$VIF <- data.frame(vif(obj$lmModel))
    obj$Model$VIF$variable <- row.names(obj$Model$VIF)
    names(obj$Model$VIF) <- c("VIF", "Variables")
  }
  obj$Model$result_all <- full_join(obj$Model$VIF, obj$Model$coefficients)
  obj$Model$result_all <- obj$Model$result_all[, c("Variables", "VIF", "Estimate","Error", "Tvalue")]
  names(obj$Model$result_all)[names(obj$Model$result_all)=="Variables"] <- "Trans_Variable"
  obj$Model$result_all <- full_join(spec, obj$Model$result_all)
 
  obj$Model$DW <- durbinWatsonTest(obj$lmModel)
  obj$Model$result_all$R2 <- rep(obj$Model$R2, nrow(obj$Model$result_all))
  obj$Model$result_all$DW <- rep(obj$Model$DW$dw, nrow(obj$Model$result_all))
#  obj$Model$DWtest <- (dwtest(obj$Model$formula, data=obj$data))

  
  if(!loop) {
  ap <- obj$Model$act_pred
  ap$KPI <- ap[[DepVar]]
  obj$Model$act_pred_chart <- 
    ggplot(data = ap, aes(x = ap[[obj$Time]])) + 
    geom_point(aes(y = KPI, colour = "KPI"), size = 0.8) + 
    geom_line(aes(y = KPI, colour = "KPI"), size = 0.8) + 
    geom_line(aes(y = predicted, colour = "predicted"), size = 0.8) +
    geom_bar (aes(y = residual, fill = "residual"), stat = "identity") +
    labs(title = DepVar,
         subtitle = paste("From", min(ap[[obj$Time]]), "to", max(ap[[obj$Time]]), sep = " "),
         x = obj$Time, y = "actual vs predicted and residual") +
    scale_fill_manual(name="Residual", 
                      values = c("residual" = "grey50"), guide = guide_legend(order = 2))
  }
  
  return(obj)
}
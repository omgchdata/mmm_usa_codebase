#################################################
# This function compares 2 models. 
# Inputs : the 2 msmp model objects
# output : comparison of the 2 model's coefficients
#          actual vs predicted 
#          decomposition summary
################################################
compare_models = function(obj1, obj2) {
  
  DepVar <- obj1$spec$Trans_Variable[obj1$spec$Variable_Type == "Dependent"]
  names(obj1$Model$coefficients)[-1] = paste0(names(obj1$Model$coefficients)[-1], "1")
  names(obj2$Model$coefficients)[-1] = paste0(names(obj2$Model$coefficients)[-1], "2")
  comp_coef <- full_join(obj1$Model$coefficients[, c("Variables", "Estimate1")], 
                         obj2$Model$coefficients[, c("Variables", "Estimate2")])
  
  obj1$Model$act_pred <- dplyr::rename(obj1$Model$act_pred, predicted1=predicted, residual1 = residual)
  obj2$Model$act_pred <- dplyr::rename(obj2$Model$act_pred, predicted2=predicted, residual2 = residual)
  comp_actpred <- full_join(obj1$Model$act_pred, obj2$Model$act_pred)
  
  comp_actpred$KPI <- comp_actpred[[DepVar]]
  act_pred_chart <- 
    ggplot(data = comp_actpred, aes(x = comp_actpred[[obj1$Time]])) + 
    geom_point(aes(y = KPI, colour = "KPI"), size = 0.8) + 
    geom_line(aes(y = KPI, colour = "KPI"), size = 0.8) + 
    geom_line(aes(y = predicted1, colour = "predicted1"), size = 0.8) +
    #geom_bar (aes(y = residual1, fill = "residual1"), stat = "identity") +
    geom_line(aes(y = predicted2, colour = "predicted2"), size = 0.8) +
    #geom_bar (aes(y = residual2, fill = "residual2"), stat = "identity") +
    labs(title = DepVar,
         #subtitle = paste("From", min(comp_actpred[[obj1$Time]]), "to", max(comp_actpred[[obj1$Time]]), sep = " "),
         x = obj1$Time, y = "actual vs predicted and residual") +
    scale_fill_manual(name="Residual", 
                      values = c("residual" = "grey50"), guide = guide_legend(order = 2))
  
  comp_actpred$KPI <- NULL
  comp_obj <- list()
  comp_obj$coefficients <- comp_coef
  comp_obj$act_pred <- comp_actpred
  comp_obj$act_pred_chart <- act_pred_chart
  
  
  d1 = decomp_summary(obj1$Decomposition, obj1$SimStart, obj1$SimEnd, dependentVar=DepVar)
  d2 = decomp_summary(obj2$Decomposition, obj2$SimStart, obj1$SimEnd, dependentVar=DepVar)
  names(d1)[2:3] <- paste0(names(d1)[2:3], "1")
  names(d2)[2:3] <- paste0(names(d2)[2:3], "2")
  comp_obj$Decomposition <- full_join(d1, d2)
  
  return(comp_obj)
  
}
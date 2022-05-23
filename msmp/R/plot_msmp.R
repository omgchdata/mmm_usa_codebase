
######################################
# plot_msmp : This R file contains all the R functions that creates plots
#             during all phases of MMM workflow
# 2020-05-05 : version 1
# Julia Liu 2022-05-23: plot_act_pred() create actual vs predicted plot.
#                       output is list object that contains ggplot. 
#                       If panel model, each list component contains up to 6 plots
#                       df: dataframe that contains the actual and predicted by crosssections (if any) and time
#                       dv : dependent variable name
#                       cs : cross section name. Null if national model
#                       time : time name ("Week" or "Month")
#             example : out_plot <- plot_act_pred(mod_obj$pred_outsample, "Y", cs="CS", "Date")
#                       print(out_plot[[1]])
#
# obj : the model object (a list object) that was generated when a MMM was 
#       ran using the msmp framework. It should contain the actual vs predicted graph 
#       and decomp stacked area graph. 
# 
############################################### 
plot_act_pred <- function(df, dv, cs=NULL, time) {
  if(!is.null(cs)) {
    df$cs <- df[[cs]]
  }
  df$time <- df[[time]]
  df$KPI <- df[[dv]]
  names(df)[names(df)=="predict"] = "predicted"
  
  df <- df[order(df$cs, df$time),]
  
  # create actual vs predicted charts by DMA. It is stored in mod_obj$Model$act_pred_cs
  p <- list()
  for (i in 1:ceiling(length(unique(df[[cs]]))/6)) {
    p[[i]] <- ggplot(df) + 
      geom_point(aes(time, KPI), size=0.5) + 
      geom_line(aes(time, predicted), colour = "red") + facet_wrap_paginate(~ cs, ncol=2, nrow = 3,page=i, scales="free") + theme_light()
  }
  
  return(p)
}

plot_msmp = function(obj) {
  if(is.null( obj$Model$act_pred_chart ) ) {
    print("there is no actual vs predicted graph in the model object.")
  } else {
    print(obj$Model$act_pred_chart)
  }
  readline("hit enter for the next graph \n")
  if(is.null( obj$decomp_chart ) ) {
    print("There is no decomp stacked area graph in the model object")
  } else {
    print(obj$decomp_chart)
  }

}


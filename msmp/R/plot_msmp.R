
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
# Julia Liu 2022-09-09: waterfall_chart() creates water fall chart.
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

waterfall_chart <- function(Variable, Value, percent = T) {
  df <- data.frame(Variable=Variable, Value = Value)
  df <- df[order(df$Value, decreasing = T),]
  
  #df$Variables <- factor(df$Variables, levels = df$Variables)
  df$Sign <- "positive"
  df$Sign[df$Value<0] <- "negative"
  
  df$end = cumsum(df$Value)
  #df$end <- c(head(df$end, -1), 0)
  df$start <- c(0, head(df$end, -1))
  #df$id <- seq(1:4)
  df <- df[order(df$Value, decreasing = F),]
  df$Variable <- factor(df$Variable, levels = df$Variable)
  df$id <- seq(1:nrow(df))
  p <- ggplot(df, aes(Variable, fill = Sign)) + 
    geom_rect(aes(x = Variable, xmin = id - 0.45, 
                  xmax = id + 0.45, ymin = end,
                  ymax = start)) + 
    labs(
      title = "Decomposition Waterfall",
      x = NULL, y = NULL, fill = "Sign"
    ) +
    coord_flip()
  
  if(percent) {
    p <- p + geom_text(mapping=aes(label = paste0(round(Value*100,1), "%"), 
                                   y = (start+Value)))
  } else {
    p <- p + geom_text(mapping=aes(label = (round(Value,1)), 
                                   y = (start+Value)))
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


# Function to set input data

set_input <- function(obj,newdata) {
  obj$input <- newdata[,intersect(names(newdata),names(obj$data))]
  obj$data[,names(obj$input)] -> 
    obj$input[(obj$input$Week >= min(obj$data$Week) & obj$input$Week <= historical_end_week),]
  return(obj)
}

set_input_wk <- function(obj,newdata,historical_end_week) {
  obj$input <- newdata[,intersect(names(newdata),names(obj$data))]
  obj$data[,names(obj$input)] -> 
    obj$input[(obj$input$Week >= min(obj$data$Week) & obj$input$Week <= historical_end_week),]
  return(obj)
}

# Function to predict

set_prediction <- function(obj,transform = T) {
  obj$pred <- predict_msmp(obj, obj$input, decomp=T, transform = transform)
  obj$pred <- full_join(obj$input[, c("Week", 
                                      as.character(obj$spec$Orig_Variable[obj$spec$Variable_Type=="Dependent"]))], 
                        obj$pred)
  return(obj)
}

# Function to save model object

save_prediction <- function(obj_item, modelfile) {
  obj <- obj_item
  temp_pred <- obj$pred[obj$pred$Week >= min(obj$Model$act_pred$Week),]
  for(i in c(1:(nrow(temp_pred)-nrow(obj$Model$act_pred)))) {
    add_row(obj$Model$act_pred) -> obj$Model$act_pred
  }
  
  temp_pred$Week -> obj$Model$act_pred$Week
  temp_pred$predict -> obj$Model$act_pred$predicted
  obj$Model$act_pred$predicted[is.na(obj$Model$act_pred[[2]])] -> obj$Model$act_pred[is.na(obj$Model$act_pred[[2]]),2]
  names(temp_pred)[2] <- names(obj$Decomposition)[2]
  temp_pred[temp_pred$Week  <= historical_end_week,2] <- obj$Decomposition[obj$Decomposition$Week <= historical_end_week,2]
  
  temp_pred[temp_pred$Week >= min(obj$Decomposition$Week),] -> obj$Decomposition
  
  obj$data <- obj$input
  obj -> mod_obj
  save(mod_obj, file=modelfile)
}

save_prediction_wk <- function(obj_item, modelfile,historical_end_week) {
  obj <- obj_item
  temp_pred <- obj$pred[obj$pred$Week >= min(obj$Model$act_pred$Week),]
  
  if(nrow(temp_pred)-nrow(obj$Model$act_pred)>0) {
  for(i in c(1:(nrow(temp_pred)-nrow(obj$Model$act_pred)))) {
    add_row(obj$Model$act_pred) -> obj$Model$act_pred
  }}
  
  temp_pred$Week -> obj$Model$act_pred$Week
  temp_pred$predict -> obj$Model$act_pred$predicted
  obj$Model$act_pred$predicted[is.na(obj$Model$act_pred[[2]])] -> obj$Model$act_pred[is.na(obj$Model$act_pred[[2]]),2]
  names(temp_pred)[2] <- names(obj$Decomposition)[2]
  temp_pred[temp_pred$Week  <= historical_end_week,2] <- obj$Decomposition[obj$Decomposition$Week <= historical_end_week,2]
  
  temp_pred[temp_pred$Week >= min(obj$Decomposition$Week),] -> obj$Decomposition
  
  obj$data <- obj$input
  obj -> mod_obj
  save(mod_obj, file=modelfile)
}

# Function to move columns

moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}

getlongdata <- function(obj) {
  if(!is.null(obj$Decomposition_Orig)){
    obj$Decomposition <- obj$Decomposition_Orig
  }
  if(is.null(obj$Decomposition$predict)){
    obj$Decomposition$predict <- rowSums(obj$Decomposition[,c(3:ncol(obj$Decomposition))])
  }
  obj$Decomposition_Long <- obj$Decomposition
  names(obj$Decomposition_Long)[2] <- "Actual"
  obj$Decomposition_Long <- obj$Decomposition_Long[moveme(names(obj$Decomposition_Long), "predict last")]
  obj$Decomposition_Long <- gather(obj$Decomposition_Long, Variable, Value, Actual:predict, factor_key=FALSE)
  return(obj)
}

paneldecomplong <- function(decomppaneltable) {
  gather(decomppaneltable,
         key = "Variable",
         value = "Value",
         3:ncol(decomppaneltable),
         na.rm = FALSE,
         convert = FALSE,
         factor_key = FALSE
  )
}



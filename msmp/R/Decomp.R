################################################
# update notes:
# 2020-05-12 Julia Liu : added mod_obj$decomp_stackedarea_chart
# 2020-05-20 Julia Liu : added another stacked area chart (mod_obj$decomp_stackedarea_chart_origvar), this time at the Orig Variable level,
#                        instead of the AggregateVariable level. 
# Julia Liu 2021/03/02 : added reference point calculation. 
#                        You will need to add a column called "decomp_ref" in the _Variable.csv
#                        The function first check to see if this field exists. 
# Julia Liu 2021/09/14 : added a coefficient check at the beginning. 
# Julia Liu 2021/09/20 : multiplitic model decomp
# Julia Liu 2021/10/11 : handles market share model
#######################################################
Decomp <- function(obj, incl_spent = FALSE, loop = FALSE, share=NULL) {

  x <- obj$data
  spec <- obj$spec
  if(is.null(obj$Model$coefficients)) {
    stop("The model object does not have coefficient. ")
  }
  b <- obj$Model$coefficients
  depvar <- obj$spec$Orig_Variable[tolower(obj$spec$Variable_Type)=="dependent"]
  depvar_scale <- spec$Scale[tolower(spec$Variable_Type) =="dependent"]
  decomp <- x[, c(obj$CS, obj$Time, depvar)]
  if("sales" %in% names(obj$Model$act_pred) & "sales_predicted" %in% names(obj$Model$act_pred))  {
    decomp <- left_join(decomp, obj$Model$act_pred[, c(obj$CS, obj$Time, "predicted", "sales","sales_predicted")])
  } else {
    decomp <- left_join(decomp, obj$Model$act_pred[,c(obj$CS, obj$Time, "predicted")])
  }
  
  calc_var <- spec$Trans_Variable[spec$Variable_Type!="Dependent"]
  dcmp_var <- paste("d", calc_var, sep="_")
  #print(dcmp_var)

  if(length(calc_var) <= 0) {
    stop("Please specify which variables you would like to decomp. ")
  }
  
  #######################
  # let's do decompostion
  # multiplitic model
  #######################
  if (tolower(obj$spec$Transform[tolower(obj$spec$Variable_Type) == "dependent"]) == "y" &
    tolower(obj$spec$TransformType[tolower(obj$spec$Variable_Type) == "dependent"]) == "log") {
    print("This is a multiplitic model. ")
    #decomp <- left_join(decomp, obj$Model$act_pred[, c(obj$Time, "predicted")])
    calc_var <- setdiff(calc_var, "Intercept")
    dcmp_var <- paste("d", calc_var, sep="_")
    for(i in 1:length(calc_var)) {
      cat("Calculate decomposition of ", calc_var[i], "\n")
      decomp[[dcmp_var[i] ]] <- (decomp$predicted - 
        decomp$predicted/exp(b$Estimate[b$Variables == calc_var[i]] * x[[calc_var[i] ]]))/depvar_scale
      
      if(! (is.null(share)) ) {
        if( share %in% names(x)) {
          decomp[[dcmp_var[i] ]] <- decomp[[dcmp_var[i]]] * x[[share]]
        } else {
          cat(share, "field does not exist in the data file. \n")
        }
      }
      
      if("decomp_ref" %in% tolower(names(spec))) {
        ref <- spec$decomp_ref[spec$Trans_Variable == calc_var[i]]
        cat("reference point =", ref, "\n")
        decomp[[dcmp_var[i] ]] <- decomp[[dcmp_var[i]]] - ref
      }
    }
    decomp$d_multi_base <- decomp$predicted-apply(decomp[,dcmp_var], 1, sum)
  } else if (tolower(obj$spec$Transform[tolower(obj$spec$Variable_Type) == "dependent"]) == "y" &
             tolower(obj$spec$TransformType[tolower(obj$spec$Variable_Type) == "dependent"]) == "mc") {
    for(i in 1:length(calc_var)) {
      cat("Calculate decomposition of ", calc_var[i], "\n")
      decomp[[dcmp_var[i] ]] <- b$Estimate[b$Variables == calc_var[i]] * x[[calc_var[i] ]]
      #if(toupper(spec$TransformType[spec$Trans_Variable == depvar]) == "MC") {
        if(tolower(dcmp_var[i]) == "d_intercept") {
          decomp[[dcmp_var[i] ]] <- decomp[[dcmp_var[i]]] * x$scl + x$cen
        } else {
          decomp[[dcmp_var[i] ]] <- decomp[[dcmp_var[i]]] * x$scl
        }
        if( tolower(obj$ModelForm) == "tiv") {
          cat("decomp a market share model. multiplying", obj$ModelForm, "\n")
          if( obj$ModelForm %in% names(x)) {
            decomp$tiv <- x[[obj$ModelForm]]
            cat(" multiply by", obj$ModelForm,"to get to sale decomp. \n")
            decomp[[dcmp_var[i] ]] <-  decomp[[dcmp_var[i] ]] * decomp$tiv/100
            
          } else {
            cat(obj$ModelForm, "field does not exist in the data file. \n")
          }
        }
      if("decomp_ref" %in% tolower(names(spec))) {
        ref <- spec$decomp_ref[spec$Trans_Variable == calc_var[i]]
        cat("reference point =", ref, "\n")
        decomp[[dcmp_var[i]]] <- decomp[[dcmp_var[i]]] - ref
      }  
      #}
    }
  } else if( tolower(obj$ModelForm) == "tiv") {
    cat("decomp a market share model. multiplying", obj$ModelForm, "\n")
    if( obj$ModelForm %in% names(x)) {
      decomp$tiv <- x[[obj$ModelForm]]
      cat(" multiply by", obj$ModelForm,"to get to sale decomp. \n")
      for(i in 1:length(calc_var)) {
        cat("Calculate decomposition of ", calc_var[i], "\n")
        decomp[[dcmp_var[i] ]] <- b$Estimate[b$Variables == calc_var[i]] * x[[calc_var[i] ]] * decomp$tiv/100
      }
      

    } else {
      cat(obj$ModelForm, "field does not exist in the data file. \n")
    }
  } else {
    for(i in 1:length(calc_var)) {
      cat("Calculate decomposition of ", calc_var[i], "\n")
      #decomp[[dcmp_var[i] ]] <- b$Estimate[b$Variables == calc_var[i]] * x[[calc_var[i] ]]
      
      if(is.null(share)) {
        decomp[[dcmp_var[i] ]] <- b$Estimate[b$Variables == calc_var[i]] * x[[calc_var[i] ]]
      } else {
        if( share %in% names(x)) {
          print(" multiply by TIV to get to sale decomp. ")
          decomp[[dcmp_var[i] ]] <- b$Estimate[b$Variables == calc_var[i]] * x[[calc_var[i] ]] * x[[share]]
        } else {
          cat(share, "field does not exist in the data file. \n")
        }
      }
      
      if("decomp_ref" %in% tolower(names(spec))) {
        ref <- spec$decomp_ref[spec$Trans_Variable == calc_var[i]]
        cat("reference point =", ref, "\n")
        decomp[[dcmp_var[i]]] <- decomp[[dcmp_var[i]]] - ref
      }
    }
  }

# include spent variable
  if(incl_spent) {
    for (i in 1:length(calc_var)) {
      spend_var <- spec$Spent_Variable[spec$Trans_Variable == calc_var[i]]
      if( !is.na(spend_var) ) {
        decomp[[spend_var]] <- x[[ spend_var ]]
      }
    }
  }
  
  obj$Decomposition <- decomp
  
  if(!loop) {
  d <- obj$Decomposition
  if(!("time" %in% names(d))) {
    d$time <- d[[obj$Time]]
    d[[obj$Time]] <- NULL
  }
  d <- gather(d, key = "d_var", value = "decomp", -time)
  
  spec <- obj$spec
  spec$d_var = paste("d", spec$Trans_Variable, sep="_")
  d <- left_join(d, spec[, c("Orig_Variable", "AggregateVariable", "d_var")])
  #dd <- d %>% dplyr::group_by(AggregateVariable, time) %>% dplyr::summarise(decomp = sum(decomp))
  #dd$AggregateVariable <- tolower(dd$AggregateVariable)
  # put the "base" last in the factor order.
  #dd$AggregateVariable <- factor(dd$AggregateVariable,levels=c(setdiff(unique(dd$AggregateVariable), "base"), "base"))
  
  #p <- ggplot(dd[!is.na(dd$AggregateVariable),], aes(x=time, y=decomp, fill=AggregateVariable)) + 
  #  geom_area()
  
  #obj$decomp_chart_stackedarea <- p
  
  #obj$decomp_chart_stackedarea_origvar <- 
  #  ggplot(d[!is.na(d$Orig_Variable),], aes(x=time, y=decomp, fill=Orig_Variable)) + 
  #  geom_area()
  }
  
  return(obj)
}


Decomp_panel <- function(obj, incl_spent = FALSE, loop = FALSE) {
  
  cs <- unique(obj$Model$coefficients[[obj$CS]])
  
  # loop through panels and do decomp one panel at a time
  dcmp <- list()
  for (i in 1:length(cs)) {
    obj2 <- obj
    obj2$data <- obj2$data[obj2$data[[obj2$CS]]== cs[i],]
    obj2$Model$coefficients <- obj2$Model$coefficients[obj2$Model$coefficients[[obj2$CS]]==cs[i],]
    obj2 <- Decomp(obj2, incl_spent = incl_spent, loop = loop )
    dcmp[[i]] <- obj2$Decomposition
  }
  
  
  obj$Decomposition_panel <- do.call("rbind", dcmp)
  
  
  # create national level decomposition 
  obj$Decomposition_panel$time <- obj$Decomposition_panel[[obj$Time]]
  d_nat <- obj$Decomposition_panel %>% group_by(time) %>% summarise_if(is.numeric, sum, na.rm = TRUE)
  names(d_nat)[1] <- obj$Time
  #d_nat[[obj$spec$Orig_Variable[tolower(obj$spec$Variable_Type)=="dependent"] ]]<- NULL
  #d_nat$predicted <- NULL
  d_nat$time <- NULL
  obj$Decomposition_panel$time <- NULL
  obj$Decomposition <- d_nat
  
  return(obj)
}

decomp_stack_chart <- function(d, time = "Date") {
  if(!("time" %in% names(d))) {
    d$time <- d[[time]]
    d[[time]] <- NULL
  }
  d$Nameplate <- NULL
  d$predicted <- NULL
  d$FAM_CMA3 <- NULL
  d <- gather(d, key = "d_var", value = "decomp", -time)
  p = ggplot(d, aes(x=time, y=decomp, fill=d_var)) + 
    geom_area()
  return(p)
}


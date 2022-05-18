#######################################
# this function run random effect model 
#
# update notes:
Run_Model_Panel <- function(obj, priors = NULL, Method="Bayes") {
  
  library(ggforce)
  Method="Bayes"
  big_number <- 100000           # for difuse priors
  spec <- obj$spec
  x <- obj$data

  DepVar <- spec$Trans_Variable[spec$Variable_Type == "Dependent"]
  IV <- spec$Trans_Variable[spec$Variable_Type != "Dependent"]
  spec <- spec[spec$Variable_Type != "Dependent", ]
  if(is.null(priors)) {
    priors <- dplyr::select(spec, Orig_Variable, Trans_Variable, Prior_Mean, Prior_SD)
  }
  
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

  eq <- ifelse(spec$VaryBy[1] == "None", spec$Trans_Variable[1], paste(spec$VaryBy[1], spec$Trans_Variable[1], sep=":"))
  if(nrow(spec) > 1) {
  for (k in 2:nrow(spec)) {
    tmp <- ifelse(spec$VaryBy[k] == "None", spec$Trans_Variable[k], paste(spec$VaryBy[k],spec$Trans_Variable[k],  sep=":"))
        eq <- paste(eq, tmp, sep="+")
  }
  }
  
  eq <- paste(eq, " -1")
  eq <- paste(DepVar, " ~ ", eq)
  eq <- as.formula(eq)

  eq_lm <- paste(IV[!IV %in% "Intercept"], collapse =" + ")
  #eq_lm <- paste(eq_lm, "-1", sep = " ")
  eq_lm <- (paste(DepVar, " ~ ", eq_lm))
  obj$eq_lm <- as.formula(eq_lm)

  # create lmer syntax of equation. This gives modelers a choice to run lmer or stan_lmer model.
  rand_iv <- spec$Trans_Variable[  tolower(spec$VaryBy)!="none"]
  rand_iv[tolower(rand_iv) == "intercept"] = "1"
  rand_eq <- paste(rand_iv, collapse = "+")  
  rand_eq <- paste(rand_eq, "|", obj$CS)
  rand_eq <- paste("(", rand_eq, ")")
  obj$eq_lmer <- as.formula(paste(eq_lm, rand_eq, sep="+"))

  # if prior is not specified
  if(is.null(priors)) {
    
  print("calling my_bayes()...")
  bayes_obj_inter <- my_bayes(formula=eq, data=x)
  mod_matrix <- model.matrix(eq, x)

  
  b <- bayes_obj_inter$coefficients
  b$Variables <- row.names(t(mod_matrix))
  priors <- b
  priors$Prior_Mean <- 0
  priors$Prior_SD <- big_number
  # first populate the priors with user defined priors in the Variables.csv
  for (i in 1:nrow(spec)) {
    v <- spec$Trans_Variable[i]
    priors$Prior_Mean[grep(v, priors$Variables)] <- spec$Prior_Mean[i]
    priors$Prior_SD[grep(v, priors$Variables)] <- spec$Prior_SD[i]
  }
  
  hb_var <- spec$Trans_Variable[spec$VaryBy != "None" & spec$Orig_Variable != "Intercept"]
#  hb_var <- spec$Trans_Variable[spec$Orig_Variable != "Intercept"]

  if("PriorSD_Adj" %in% names(spec)) {
    spec$PriorSD_Adj <- abs(spec$PriorSD_Adj)
  } else {
    print("PriorSD_Adj is not in the _Variable.csv file. Set it to the default value 1.")
    spec$PriorSD_Adj <- 1
  }
  # 2020-09-20 Julia Liu : added eq_lmer equation. It is constructed to reflect the _Variable.csv.
#                        using this equation, modeler can easiy run lmer or stan_lmer function 
#                        as an alternative
# 2021-04-09 Julia Liu : calculates number of pages needed to store the AVP Geo level charts. 
#                        should have used ceiling() instead of round()
# 2021-04-21 Julia Liu : added "priors" to Run_Model_Panel function. It defaults to NULL.
#                        - When it is set to NULL, the Run_Model_Panel will first determine the empirical priors
#                          then run the final random effect model using the priors.
#                        - User first runs the Gen_EB_Panel() function to "learn" priors, 
#                          then set the "priors" in Run_Model_Panel equal to the result of the Gen_EB_Panel.  
#                          In this case, the Run_model_Panel will skip the 1st step of "learning" priors, and 
#                          just ran the final model using the priors. 
#######################################

  if(length(hb_var) >= 1) {
  for (j in 1:length(hb_var)) {
    # if override == "N", calculate emperical prior and populate the priors dataframe
    if(toupper(spec$Override[spec$Trans_Variable == hb_var[j]]) == "N") {
      tmp <- priors$Estimate[grep(hb_var[j], priors$Variables)]
      if(spec$Sign[spec$Trans_Variable == hb_var[j]] > 0 ) {
        if(is.nan(mean(tmp[tmp>0]))) {
          cat("The code is not able to find a emperical prior for", hb_var[j], "\n")
          cat("A diffuse prior is used. If you want an informative prior, you can override.\n")
        } else {
          priors$Prior_Mean[grep(hb_var[j], priors$Variables)] = mean(tmp[tmp>0])
        }
      } else if(spec$Sign[spec$Trans_Variable == hb_var[j]] < 0 ) {
        if(is.nan(mean(tmp[tmp<0]))) {
          cat("The code is not able to find a emperical prior for", hb_var[j], "\n")
          cat("A diffuse prior is used. If you want an informative prior, you can override.\n")
        } else {
          priors$Prior_Mean[grep(hb_var[j], priors$Variables)] = mean(tmp[tmp<0])
        }
      } else {
        priors$Prior_Mean[grep(hb_var[j], priors$Variables)] = mean(tmp)
      }
# for now let's set the prior_sd = prior_mean*PriorSD_Adj. The PriorSD_Adj defauls to 1 in the _Variables.csv

      priors$Prior_SD[grep(hb_var[j], priors$Variables)] = 
        abs(priors$Prior_Mean[grep(hb_var[j], priors$Variables)] * spec$PriorSD_Adj[spec$Trans_Variable == hb_var[j]])
      if(sum(priors$Prior_SD[grep(hb_var[j], priors$Variables)]) == 0 ) {
        priors$Prior_SD[grep(hb_var[j], priors$Variables)] <- big_number
      }
    }
  }
  }
  
  order_of_var <- row.names(t(mod_matrix))

  a <- priors
  a$Variables <- factor(a$Variables, levels=order_of_var)
  a <- arrange(a, Variables)
  a$Variables <- as.character(a$Variables)
  priors <- a
  }  # if(is.null(priors))
  
  # run the final bayesian model with the priors "learned" from the 1st step.
  bayes_obj <- my_bayes(formula=eq, data=x, priors=priors)

  # now create the coefficient data frame that has results by variable by panel
  varyby <- unique(obj$spec$VaryBy)[unique(obj$spec$VaryBy) != "None"]
  if(length(varyby) >0 ) {
    b <- bayes_obj$coefficients %>% 
      separate(Variables, into = c(varyby, "Variables"), sep = "\\.", fill="left")
    b[[varyby]] <- gsub(varyby, "", b[[varyby]])

    tmp <- data.frame(varyby=unique(obj$data[[varyby]]))
    names(tmp) <- varyby
  
    full_b <- expand.grid(varyby = tmp[[varyby]],
                        Variables = obj$spec$Trans_Variable[obj$spec$Variable_Type != "Dependent"])
    names(full_b)[1] <- varyby
    
    full_b$Variables <- as.character(full_b$Variables)
    full_b[[varyby]] <- as.character(full_b[[varyby]])
    full_b <- left_join(full_b,b)
    v <- unique(full_b$Variables[is.na(full_b$Estimate)])
    if(length(v) >0 ) {
      for (j in 1:length(v)) {
        full_b$Estimate[full_b$Variable == v[j]] <- b$Estimate[b$Variables == v[j]]
        full_b$Error[full_b$Variable == v[j]] <- b$Error[b$Variables == v[j]]
      }
    }
    full_b$Tvalue <- full_b$Estimate/full_b$Error
  
  } else {
    varyby = obj$CS
    tmp <- data.frame(varyby=unique(obj$data[[varyby]]))
    names(tmp) <- varyby
    full_b <- expand.grid(varyby = tmp[[varyby]],
                          Variables = obj$spec$Trans_Variable[toupper(obj$spec$Variable_Type) != "DEPENDENT"])
    full_b$varyby <- as.character(full_b$varyby)
    full_b$Variables <- as.character(full_b$Variables)
    names(full_b)[1] = varyby
    
    full_b <- left_join(full_b, bayes_obj$coefficients)
  }
  bayes_obj$coefficients <- full_b
  obj$Model <- bayes_obj
  
  # calculate actual vs predicted
  #obj$Model$act_pred_modeled <- mod_obj$Model$act_pred
  obj$Model$act_pred <- act_pred(obj)
  obj$Model$act_pred_orginal <- act_pred(obj)
  
  #obj$Model_interLM <- bayes_obj_inter
  obj$lmModel <- lm(obj$eq_lm, data=x)
  
  # calculate MAPE at DMA level
  tmp <- obj$Model$act_pred
  dv <- obj$spec$Orig_Variable[obj$spec$Variable_Type == "Dependent"]
  tmp$time <- tmp[[obj$Time]]
  tmp$KPI <- tmp[[dv]]
  tmp$cs <- tmp[[obj$CS]]
  obj$Model$MAPE <- tmp %>% group_by(cs) %>% summarise(MAPE = MAPE(KPI, predicted))
  names(obj$Model$MAPE) <- c(obj$CS, "MAPE") 
  

  
  # create actual vs predicted charts by DMA. It is stored in mod_obj$Model$act_pred_cs
  p <- list()
  for (i in 1:ceiling(length(unique(tmp[[mod_obj$CS]]))/6)) {
    p[[i]] <- ggplot(tmp) + 
      geom_point(aes(time, KPI), size=0.5) + 
      geom_line(aes(time, predicted), colour = "red") + facet_wrap_paginate(~ cs, ncol=2, nrow = 3,page=i, scales="free") + theme_light()
  }
  obj$Model$act_pred_dma_chart <- p
  
  tmp <- tmp %>% group_by(time) %>% summarise(KPI=sum(KPI), predicted = sum(predicted))
  tmp$residual <- tmp$KPI - tmp$predicted

  
  # calculate R2 at the aggregated/national level
  SS_tot <- sum((tmp$KPI-mean(tmp$KPI))^2)
  #  SS_res <- sum((residuals)^2)
  SS_res <- t(tmp$residual) %*% tmp$residual
  obj$Model$R2_Nat <- 1-(SS_res/SS_tot)
  obj$Model$R2 <- NULL
  
  obj$Model$act_pred_chart <- 
    ggplot(data = tmp, aes(x = time)) + 
    geom_point(aes(y = KPI, colour = "KPI"), size = 0.8) + 
    geom_line(aes(y = KPI, colour = "KPI"), size = 0.8) + 
    geom_line(aes(y = predicted, colour = "predicted"), size = 0.8) +
    geom_bar (aes(y = residual, fill = "residual"), stat = "identity") +
    labs(title = dv,
         subtitle = paste("From", min(tmp$time), "to", max(tmp$time), sep = " "),
         x = obj$Time, y = "actual vs predicted and residual") +
    scale_fill_manual(name="Residual", 
                      values = c("residual" = "grey50"), guide = guide_legend(order = 2))
  names(tmp)[grep("time", names(tmp))] <- obj$Time
  obj$Model$act_pred_national <- tmp
  
  # create a result_all data frame that contains model specifications, estimates, VIF, and etc.
  obj$Model$DW <- durbinWatsonTest(obj$lmModel)
  obj$Model$VIF <- data.frame(vif(obj$lmModel))
  obj$Model$VIF$variable <- row.names(obj$Model$VIF)
  obj$Model$Priors <- priors
  names(obj$Model$VIF) <- c("VIF", "Variables")
  obj$Model$VIF <- left_join(obj$Model$coefficients,obj$Model$VIF, by="Variables")
  obj$Model$VIF <- obj$Model$VIF[,c("VIF", "Variables", names(obj$Model$coefficients)[1])]
  # 
  obj$Model$result_all <- full_join(obj$Model$VIF, obj$Model$coefficients)
  obj$Model$result_all <- obj$Model$result_all[, c("Variables", names(obj$Model$coefficients)[1], "VIF", "Estimate","Error", "Tvalue")]
  names(obj$Model$result_all)[names(obj$Model$result_all)=="Variables"] <- "Trans_Variable"
  obj$Model$result_all <- full_join(spec, obj$Model$result_all)
  
  obj$Model$result_all$R2 <- rep(obj$Model$R2_Nat, nrow(obj$Model$result_all))
  obj$Model$result_all$DW <- rep(obj$Model$DW$dw, nrow(obj$Model$result_all))

  
  a <- obj$Model$Priors %>% 
    separate(Variables, into = c(obj$CS, "Variables"), sep = "\\:", fill="left")
  a[[obj$CS]] <- gsub(obj$CS, "", a[[obj$CS]])
  a$Error <- NULL
  a$Tvalue <- NULL
  names(a)[names(a)=="Estimate"] <- "estimate_freq"
  #a <- a[, c(obj$CS, "Variables", "Estimate")]
  #a <- spread(a, Variables, Estimate)
  
  b <- obj$Model$coefficients[, c(obj$CS, "Variables", "Estimate")]
  names(b)[names(b)=="Estimate"] <- "estimate_bayes"
  obj$Model$freq_priors_bayes <- full_join(a, b)
  obj$Model$freq_priors_bayes <- 
    obj$Model$freq_priors_bayes[obj$Model$freq_priors_bayes$Variables %in% obj$spec$Trans_Variable[tolower(obj$spec$VaryBy) != "none"],]
  return(obj)
}
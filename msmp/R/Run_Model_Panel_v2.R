#' Execute Model Fitting
#'
#' @param obj
#' @param Method
#'
#' @return
#' @export
#'
#' @description
#'
#' @examples
#'
Run_Model_Panel <- function(obj, Method="Bayes") {

  Method="Bayes"
  big_number <- 100000           # for difuse priors
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

  eq <- ifelse(spec$VaryBy[1] == "None", spec$Trans_Variable[1], paste(spec$VaryBy[1], spec$Trans_Variable[1], sep=":"))
  for (k in 2:nrow(spec)) {
    tmp <- ifelse(spec$VaryBy[k] == "None", spec$Trans_Variable[k], paste(spec$VaryBy[k],spec$Trans_Variable[k],  sep=":"))
        eq <- paste(eq, tmp, sep="+")
  }

  eq <- paste(eq, " -1")
  eq <- paste(DepVar, " ~ ", eq)
  eq <- as.formula(eq)

  eq_lm <- paste(IV[!IV %in% "Intercept"], collapse =" + ")
  eq_lm <- as.formula(paste(DepVar, " ~ ", eq_lm))


  print("calling my_bayes()...")
  bayes_obj_inter <- my_bayes(formula=eq, data=x)

  b <- bayes_obj_inter$coefficients
  priors <- b
  priors$Prior_Mean <- 0
  priors$Prior_SD <- big_number
  hb_var <- spec$Trans_Variable[spec$VaryBy != "None" & spec$Orig_Variable != "Intercept"]
#  hb_var <- spec$Trans_Variable[spec$Orig_Variable != "Intercept"]
  
  if(length(hb_var) >= 1) {
  for (j in 1:length(hb_var)) {
    tmp <- priors$Estimate[grep(hb_var[j], priors$Variables)]
    if(spec$Sign[spec$Trans_Variable == hb_var[j]] < 0 ) {
      priors$Prior_Mean[grep(hb_var[j], priors$Variables)] = mean(tmp[tmp<0])
    } else {
      priors$Prior_Mean[grep(hb_var[j], priors$Variables)] = mean(tmp[tmp>=0])
    }
# for now let's set the prior_sd = prior_mean*PriorSD_Adj. The PriorSD_Adj defauls to 1 in the _Variables.csv
#    priors$Prior_SD[grep(hb_var[j], priors$Variables)] = 
#      abs(mean(tmp))*spec$PriorSD_Adj[spec$Trans_Variable == hb_var[j]]
    priors$Prior_SD[grep(hb_var[j], priors$Variables)] = 
      priors$Prior_Mean[grep(hb_var[j], priors$Variables)] * spec$PriorSD_Adj[spec$Trans_Variable == hb_var[j]]
  }
  }
  
  if(0) {
  #J. Liu 2018-10-10
  v <- unique(priors$Variables)
  for (i in 1:length(v)) {
    if(v[i] %in% priors0$Trans_Variable) {
      priors$Prior_Mean[priors$Variables == v[i]] <- 
        priors0$Prior_Mean[priors0$Trans_Variable == v[i]]
      priors$Prior_SD[priors$Variables == v[i]] <- 
        priors0$Prior_SD[priors0$Trans_Variable == v[i]]
    }
  }
  #J. Liu 2018-10-10
  }
  
  bayes_obj <- my_bayes(formula=eq, data=x, priors=priors) 

  varyby <- unique(obj$spec$VaryBy)[unique(obj$spec$VaryBy) != "None"]  
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
  
  bayes_obj$coefficients <- full_b
  
  #####################
  # actual vs predicted
  #####################
  full_b$Error <- NULL
  full_b$Tvalue <- NULL
  full_b$Variables <- paste0("beta_", full_b$Variables)
  full_b <- spread(full_b, Variables, Estimate)
  x[[varyby]] <- as.character(x[[varyby]])
  x <- left_join(x, full_b)
  x$predicted <- 0
  betaIV <- paste0("beta_", IV)
  for (i in 1:length(IV)) {
    x$predicted <- x$predicted + x[[betaIV[i] ]] * x[[ IV[i] ]]
  }
    
  obj$Model <- bayes_obj
  obj$Model$act_pred <- x[, c(varyby, obj$Time, DepVar, "predicted")]
  obj$Model$act_pred$residual <- x[[DepVar]] - x$predicted

  obj$Model_interLM <- bayes_obj_inter
  obj$lmModel <- lm(eq_lm, data=x)
  obj$Model$DW <- durbinWatsonTest(obj$lmModel)
  #obj$Model$VIF <- data.frame(vif(obj$lmModel))
  #obj$Model$VIF$variable <- row.names(obj$Model$VIF)
  obj$Model$Priors <- priors
  #names(obj$Model$VIF) <- c("VIF", "Variables")
  
  return(obj)
}
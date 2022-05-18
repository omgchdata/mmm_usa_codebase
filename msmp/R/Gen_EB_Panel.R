
Gen_EB_Panel <- function(obj) {

  library(ggforce)
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
  
  return(priors)
} 
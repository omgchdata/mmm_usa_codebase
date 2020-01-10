needs(tidyverse)

contributions <- function(obj, start_date, end_date) {
  dep_var <- obj$spec[obj$spec$Variable_Type == "Dependent",]$Trans_Variable
  decomp_df <- obj$Decomposition
  
  total <- sum(decomp_df[decomp_df[1] >= start_date & decomp_df[1] <= end_date,][dep_var])
  
  names(decomp_df)[1] <- "Week"
  
  contrib <- decomp_df %>% 
    filter(Week >= start_date & Week <= end_date) %>% 
    select(-Week) %>% 
    gather(key = Variable, value = Contributions) %>% 
    group_by(Variable) %>% 
    summarise(Contributions = sum(Contributions, na.rm = TRUE)) %>% 
    mutate(Pct_Total = Contributions/total) %>% 
    arrange(desc(Pct_Total))
  
  contrib
}


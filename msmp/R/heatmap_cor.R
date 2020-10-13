

################################################
# created on 2020-05-20
# this function creates heatmap given a dataset
# note, you might want to filter the dataset to only the variables you like to see
# correlation information. Otherwise the heatmap might look overwhelming.
################################################
library(reshape2)
library(ggplot2)

heatmap_cor <- function(data) {
  
  #v <- spec$Orig_Variable[tolower(spec$Variable_Type) != "dependent" & tolower(spec$Orig_Variable) != "intercept"]
  if("intercept" %in% tolower(names(data)) ) {
    data[["Intercept"]] <- NULL
  }
  cormat <- round(cor(data), 2)

  cormat[upper.tri(cormat)] <- NA
  melted_cormat <- melt(cormat, na.rm = TRUE)
  names(melted_cormat) <- c("Var1", "Var2", "value")
  
  # Heatmap
  heatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
  
  return(heatmap)
}




####################################################
# this function creates stack area ggplot chart object for decompositon.
# It takes msmp model object, start and end date (which are optional),
# and returns a ggplot object.
# In order to run this function, the model object must have $Decomposition data frame.
# Julia Liu 2020-05-04 : now the "Base"/"base" should always come at the bottom of the stack chart. 
####################################################
decomp_chart_stackedarea <- function(obj, start_date = "1980-01-01", end_date = "2100-01-01") {

  if(is.null(obj$Decomposition)) {
    stop("there is no decomposition ($Decomposition) in the model object.")
  }
  
  d <- obj$Decomposition
  d <- d[d[[obj$Time]] >= start_date & d[[obj$Time]] <= end_date, ]
  if(nrow(d) == 0) {
    stop("There is no decomp left after the date range is applied. Please check start and end date.")
  }
  
  if(!("time" %in% names(d))) {
    d$time <- d[[obj$Time]]
    d[[obj$Time]] <- NULL
  }
  d <- gather(d, key = "d_var", value = "decomp", -time)

  spec <- obj$spec
  spec$d_var = paste("d", spec$Trans_Variable, sep="_")
  d <- left_join(d, spec[, c("AggregateVariable", "d_var")])
  dd <- d %>% group_by(AggregateVariable, time) %>% summarise(decomp = sum(decomp))
  dd$AggregateVariable <- tolower(dd$AggregateVariable)
  # put the "base" last in the factor order.
  dd$AggregateVariable <- factor(dd$AggregateVariable,levels=c(setdiff(unique(dd$AggregateVariable), "base"), "base"))
  
  p <- ggplot(dd[!is.na(dd$AggregateVariable),], aes(x=time, y=decomp, fill=AggregateVariable)) + 
       geom_area()
  return(p)
}

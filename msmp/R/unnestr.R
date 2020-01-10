rename <- function(x) {
  if (typeof(x) != "character") stop("input is not a character")
  if (str_sub(x, 1, 2) == "d_") {x <- str_sub(x, 3)}
  return(x)
}


unnestr <- function(child, parent) {
  for (objects in c("child", "parent")) {
    eval(parse(text = gsub("x", objects, "names(x) <- str_to_upper(names(x))")))
  }
  if (!("DECOMP" %in% names(child)) | !("VARIABLES" %in% names(child))) {stop("child object needs to have dataframes named Decomp and Variables")}
  if (!("DECOMP" %in% names(parent)) | !("VARIABLES" %in% names(parent))) {stop("parent object needs to have dataframes named Decomp and Variables")}
  if (!("Submodel_Link" %in% (parent$MODELSETUP$Parameter))) {stop("In parent model set up file; missing 'Submodel_Link'")}
  join <- filter(parent$MODELSETUP, Parameter == "Submodel_Link")$Value 
  if (!(join %in% parent$VARIABLES$Trans_Variable)) {stop("parent Submodel_Link does not correspond to a parent Trans_Variable")}
  
  name <- names(child$DECOMP)
  name2 <- sapply(name, rename)
  
  #Get Dependent Variable name
  dep_var_child <- child$VARIABLES[child$VARIABLES$Include > 0 & str_to_upper(child$VARIABLES$Variable_Type) == "DEPENDENT", ]$Trans_Variable
  
  
  #Create a data set of the non-base variables (Advertising/Trend Variables)
  child_base <- child$VARIABLES[str_to_upper(child$VARIABLES$Variable_Type) %in% c("BASE", "DEPENDENT"), ]$Trans_Variable
  child_non_base <- names(name2[!name2 %in% child_base])
  child_non_base_val <- child$DECOMP[child_non_base]
  child_non_base_agg <- rowSums(child_non_base_val[-1])
  
  
  #Get an organized list of the fieldnames from the parent
  names_parent <- names(parent$DECOMP)
  names_parent2 <- sapply(names_parent, rename)
  
  
  #Calculate the share of the parent explained by the child
  ratio <- parent$DECOMP[names_parent2 %in% join] / child$DECOMP[dep_var_child]
  ratio[ratio == Inf] <- 0
  
  #Unnest everything
  new_parent <- parent$DECOMP
  new_parent["Tally"] <- 0
  #In this section we could define how to roll it up
  #Add in new columns 
  for (i in names(child_non_base_val[-1])) {
    if(length(grep(i, names_parent)) == 0) {new_parent[i] <- 0}
  }
  
  #Actually do the unnesting
  for (i in names(child_non_base_val[-1])) {
    unnest_var <- (child$DECOMP[i] * ratio)[[1]]
    new_parent[i] <- new_parent[i] + unnest_var
    new_parent$Tally <- new_parent$Tally + unnest_var
  }
  new_parent[grep(join, names_parent)] <- new_parent[grep(join, names_parent)] - new_parent$Tally
  new_parent$Tally <- NULL
  return(new_parent)
}
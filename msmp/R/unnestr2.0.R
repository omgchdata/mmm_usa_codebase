
load_nest <- function(submodel_name, root_dir) {
  # Check that the directory exists
  if(dir.exists(paste0(root_dir, submodel_name)) == FALSE) {
    stop(gsub("path", paste0(root_dir, submodel_name), "path does not exist"))
  }
  # Check that it's output directory exists
  else if (dir.exists(paste0(root_dir, submodel_name, "/output")) == FALSE) {
    stop(gsub("path", paste0(root_dir, submodel_name, "/output"), "path does not exist"))
  }
  # Check that the model object exists
  else if (file.exists(paste0(root_dir, submodel_name, "/output/", submodel_name, ".Rdata")) == FALSE) {
    stop(gsub("path", paste0(root_dir, submodel_name, "/output/", submodel_name, ".Rdata"), "path does not exist"))
  }
  
  #Set up new environment and load in the Rdata
  rdata <- paste0(root_dir, submodel_name, "/output/", submodel_name, ".Rdata")
  temp_env <- new.env()
  load(rdata, temp_env)
  
  for (item in c("decomposition", "spec")){
    if(!(str_to_lower(item) %in% str_to_lower(names(temp_env$mod_obj)))) {
      warning(gsub("item", item, "'item' is not in the model object; it will be needed to unnest"))
    }
  }
  return(temp_env)
}


rename <- function(x) {
  if (typeof(x) != "character") stop("input is not a character")
  if (str_sub(x, 1, 2) == "d_") {x <- str_sub(x, 3)}
  return(x)
}



unnestr <- function(child, parent, submodel_number) {
  num <- as.character(submodel_number)
  for (objects in c("child", "parent")) {
    eval(parse(text = gsub("x", objects, "names(x) <- str_to_lower(names(x))")))
  }
  if (!("decomposition" %in% names(child)) | !("spec" %in% names(child))) {stop("child object needs to have dataframes named decomposition and spec")}
  if (!("decomposition" %in% names(parent)) | !("spec" %in% names(parent))) {stop("parent object needs to have dataframes named decomposition and spec")}
  if (!(paste0("Submodel_Link", num) %in% (parent$setup$Parameter))) {stop(gsub("num", num, "In parent model set up file; missing 'Submodel_Linknum'"))}
  join <- filter(parent$setup, Parameter == paste0("Submodel_Link", num))$Value 
  if (!(join %in% parent$spec$Trans_Variable)) {stop(gsub("num", num, "parent Submodel_Linknum does not correspond to a parent Trans_Variable"))}
  
  name <- names(child$decomposition)
  name2 <- sapply(name, rename)
  
  #Get Dependent Variable name
  dep_var_child <- child$spec[child$spec$Include > 0 & str_to_upper(child$spec$Variable_Type) == "DEPENDENT", ]$Trans_Variable
  
  
  #Create a data set of the non-base variables (Advertising/Trend Variables)
  child_base <- child$spec[str_to_upper(child$spec$Variable_Type) %in% c("BASE", "DEPENDENT"), ]$Trans_Variable
  child_non_base <- names(name2[!name2 %in% child_base])
  child_non_base_val <- child$decomposition[child_non_base]
  child_non_base_agg <- rowSums(child_non_base_val[-1])
  
  
  #Get an organized list of the fieldnames from the parent
  names_parent <- names(parent$decomposition)
  names_parent2 <- sapply(names_parent, rename)
  
  
  #Calculate the share of the parent explained by the child
  ratio <- parent$decomposition[names_parent2 %in% join] / child$decomposition[dep_var_child]
  ratio[ratio == Inf] <- 0
  
  #Unnest everything
  new_parent <- parent$decomposition
  new_parent["Tally"] <- 0
  #In this section we could define how to roll it up
  #Add in new columns 
  for (i in names(child_non_base_val[-1])) {
    if(length(grep(i, names_parent)) == 0) {new_parent[i] <- 0}
  }
  
  #Actually do the unnesting
  for (i in names(child_non_base_val[-1])) {
    unnest_var <- (child$decomposition[i] * ratio)[[1]]
    new_parent[i] <- new_parent[i] + unnest_var
    new_parent$Tally <- new_parent$Tally + unnest_var
  }
  new_parent[grep(join, names_parent)] <- new_parent[grep(join, names_parent)] - new_parent$Tally
  new_parent$Tally <- NULL
  parent$Decomposition <- new_parent
  parent$decomposition <- NULL
  return(parent)
}
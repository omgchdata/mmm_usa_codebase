
library(tidyverse)
code_dir <- "C:/Users/julia.liu/OneDrive - OneWorkplace/Documents/MyWork/an_ms_modelplatform/msmp/R/"
source(paste(code_dir, "unnestr3.0.R", sep = ""))
source(paste(code_dir, "post_unnesting.R", sep = ""))
RootDirectory <- "C:/Users/julia.liu/OneDrive - OneWorkplace/Documents/MyWork/an_ms_modelplatform/mmm_unnest_example/"
##### UNNESTING SECTION 1: LOADING MODELS #####
## Load environments (one for each model). They will each contain the .Rdata model object within each folder
main_mod <- load_nest(submodel_name = "main_model", root_dir = RootDirectory)
sub_mod <- load_nest(submodel_name = "sub_model", root_dir = RootDirectory)

#check the setup and make sure there are sub model information
print(main_mod$mod_obj$setup)

##### UNNESTING SECTION 2: UNNEST #####
main_mod$mod_obj <- unnestr(child = sub_mod$mod_obj, parent = main_mod$mod_obj, submodel_number = 1)

##### UNNESTING SECTION 3: POST-UNNEST #####
main_mod$mod_obj <- post_unnesting(main_mod$mod_obj)

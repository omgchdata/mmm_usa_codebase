#######################################################################
# This is an example R code to run panel/multilevel/mixed-effect model in msmp framework
#######################################################################
library(car)
library(lmtest)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(plotly)
#needs(data.table)
#Define the server : pc or mac
if (Sys.info()['sysname'] == "Darwin") {server <- "/Volumes"} else {server <- "//nyccentral"}
code_dir <- paste0(server, "/annalect/BrandScience/msmp/R/")
source(paste(code_dir, "msmp_setup.R", sep = ""))
source(paste(code_dir, "Check_Data.R", sep = ""))
source(paste(code_dir, "Transform.R", sep = ""))
source(paste(code_dir, "Run_Model.R", sep = ""))
source(paste(code_dir, "Gen_EB_Panel.R", sep = ""))
source(paste(code_dir, "Run_Model_Panel.R", sep = ""))
source(paste(code_dir, "my_bayes.R", sep = ""))
source(paste(code_dir, "MAPE.R", sep = ""))
source(paste(code_dir, "act_pred.R", sep = ""))
source(paste(code_dir, "scatterhist.R", sep = ""))
source(paste(code_dir, "Decomp.R", sep = ""))
source(paste(code_dir, "unscale.R", sep = ""))
source(paste(code_dir, "DueToChart.R", sep = ""))
source(paste(code_dir, "responsecurve.R", sep = ""))
source(paste(code_dir, "decomp_summary.R", sep = ""))

#######  define project directories ##############
# please edit these lines to define the path to the project folder.
ProjectName <-  "msmp_panel_example"            # the name of the subfolder that contains the model project
OutDir <- "output"
RootDirectory <- paste0(server, "/Annalect/BrandScience/msmp/doc/")
ProjectDirectory <- paste(RootDirectory, ProjectName, "/", sep="")   # this is the full path of the project

###################
# setup
###################
# define input file names
ModelDataFile <- paste(ProjectDirectory, ProjectName, "_ModelData.csv", sep="")
ModelSetupFile <- paste(ProjectDirectory, ProjectName, "_ModelSetup.csv", sep="")
ModelSpecFile <- paste(ProjectDirectory, ProjectName, "_Variables.csv", sep="")

# output file names
output_folder <- paste(ProjectDirectory, OutDir, sep="")
if(!file.exists(output_folder)) {
  dir.create(output_folder)
}

PriorFile <- paste(output_folder,  "/", ProjectName, "_Priors.csv", sep="")
ModObjectFile <- paste(output_folder, "/", ProjectName, ".RData", sep="")
ModelAllResultFile <- paste(output_folder, "/", ProjectName, "_result_workbook.xlsx", sep="")

# read input files
x <- read_csv(ModelDataFile, col_types = cols())
# sometime the .csv file has the date in a mm/dd/yy format, this line changed it to the date formate the code requires which is yyyy-mm-dd
#x$Week<- mdy(x$Week)
# you can add some variables that are not in the _ModelData.csv here.
# For example dummy/indicator variables and etc. 

# define model object
mod_obj <- list()
mod_obj$data <- x
mod_obj$spec <- read.csv(ModelSpecFile,stringsAsFactors = F) %>% 
  dplyr::filter(Include == 1) # use Include to include (1)/exclude (0) variables
mod_obj$setup <- read.csv(ModelSetupFile, stringsAsFactors = F)

mod_obj <- msmp_setup(mod_obj)

#######################################
# data check
#######################################
mod_obj <- Check_Data(mod_obj)

#########################
# variable transformation
#########################
mod_obj <- Transform_panel(mod_obj)
mod_obj$data <- mod_obj$data[mod_obj$data[[mod_obj$Time]] >= mod_obj$BeginDate & mod_obj$data[[mod_obj$Time]] <= mod_obj$EndDate,]

########################
# Run model
########################
needs(ggforce)
print("Calculate Empirical Bayesian Priors:")
eb_priors <- Gen_EB_Panel(mod_obj)


print("Run hierarchical bayesian model using the emperical priors:")
mod_obj <- Run_Model_Panel(mod_obj, priors=eb_priors)

# actual vs predicted by regions
print(mod_obj$Model$act_pred_dma_chart[[1]])
print(mod_obj$Model$act_pred_dma_chart[[2]])

# actual vs predicted at the aggregated level
print(mod_obj$Model$act_pred_chart)

######################
# Decomp calculation
######################
mod_obj <- Decomp_panel(obj = mod_obj, incl_spent = F, loop=T)
decomp_sum <- decomp_summary_panel(df= mod_obj$Decomposition_panel, 
                                   startDate = mod_obj$SimStart, 
                                   endDate = mod_obj$EndDate,
                                   dependentVar=mod_obj$spec$Orig_Variable[tolower(mod_obj$spec$Variable_Type) == "dependent"], 
                                   cs = mod_obj$CS)

decomp_sum_predicted <- decomp_summary_panel(df= mod_obj$Decomposition_panel, 
                                   startDate = mod_obj$SimStart, 
                                   endDate = mod_obj$EndDate,
                                   dependentVar="predicted", 
                                   cs = mod_obj$CS)


####################
# Simulation to get response curve
####################
mod_obj <- responsecurve_panel(obj = mod_obj, showPlot=FALSE )

# let's save the model object to a RData file. This object can be loaded later at post-model calculation:
# unnest, combine response curves and calculate abc.
save(mod_obj, file=ModObjectFile)

allresultlist <- list(mod_obj$Model$Priors, mod_obj$Model$coefficients,mod_obj$Model$act_pred, mod_obj$Model$result_all, mod_obj$Decomposition_panel, decomp_sum_predicted, mod_obj$Decomposition)
write.xlsx(allresultlist, ModelAllResultFile, asTable = FALSE, sheetName=c("Priors", "Coefficients","Act vs Pred","Diagnostics","Decomps_panel", "contriubtion panel", "Decomps_national"), overwrite = T)
print("All done.")


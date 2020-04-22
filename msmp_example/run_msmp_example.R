#############################################
# Julia Liu : This R code runs msmp example
#############################################
library(car)
library(lmtest)
library(tidyverse)
library(lubridate)

#needs(car, lmtest, tidyverse, lubridate, stringr, onls)
#Defie the server : pc or mac
if (Sys.info()['sysname'] == "Darwin") {server <- "/Volumes"} else {server <- "//nyccentral"}
#code_dir <- "C:/Users/julia.liu/Documents/MyWork/Projects/Julia/msmp/R/"
code_dir <- "C:/Users/julia.liu/OneDrive - Omnicom Media Group/Documents/MyWork/an_ms_modelplatform/msmp/R/"

source(paste(code_dir, "Run_Model.R", sep = ""))
source(paste(code_dir, "my_bayes_v2.R", sep = ""))
source(paste(code_dir, "Check_Data.R", sep = ""))
source(paste(code_dir, "Transform.R", sep = ""))
source(paste(code_dir, "resid_var_cor.R", sep = ""))
source(paste(code_dir, "Decomp.R", sep = ""))
source(paste(code_dir, "decomp_chart_stackedarea.R", sep = ""))
source(paste(code_dir, "MAPE.R", sep = ""))
source(paste(code_dir, "responsecurve.R", sep = ""))
source(paste(code_dir, "unnestr2.0.R", sep = ""))
source(paste(code_dir, "abc.R", sep = ""))
source(paste(code_dir, "abc_onls.R", sep = ""))
source(paste(code_dir, "fitABC.R", sep = ""))

#######  define project directories ##############
# please edit these lines to define the path to the project folder.
ProjectName <-  "msmp_example"            # the name of the subfolder that contains the model project
OutDir <- "output"
RootDirectory <- "C:/Users/julia.liu/OneDrive - Omnicom Media Group/Documents/MyWork/an_ms_modelplatform/"
ProjectDirectory <- paste(RootDirectory, ProjectName, "/", sep="")   # this is the full path of the project

###################
# setup
###################
# define input file names
ModelDataFile <- paste(ProjectDirectory, ProjectName, "_ModelData.csv", sep="")
ModelSetupFile <- paste(ProjectDirectory, ProjectName, "_ModelSetup.csv", sep="")
ModelSpecFile <- paste(ProjectDirectory, ProjectName, "_Variables.csv", sep="")
ModelFitCurves <- paste(ProjectDirectory, ProjectName, "_Fit_Curves.csv", sep="")
# output file names
output_folder <- paste(ProjectDirectory, OutDir, sep="")
if(!file.exists(output_folder)) {
  dir.create(output_folder)
}

CoefficientsFile <- paste(output_folder,  "/", ProjectName, "_Coefficients.csv", sep="")
ActualPredictedFile <- paste(output_folder, "/", ProjectName, "_ActPred.csv", sep="")
DecompFile <- paste(output_folder,  "/", ProjectName, "_Decomp.csv", sep="")
raw_transFile <-  paste(output_folder, "/", ProjectName, "_TransVariables.csv", sep="")
spec_coef_vifFile <- paste(output_folder,  "/", ProjectName, "_spec_coef_vif.csv", sep="")
RCFile <- paste(output_folder,"/", ProjectName, "_ResponseCurve.csv", sep="")
kpi_spentFile <- paste(output_folder,"/", ProjectName, "_kpi_spent.csv", sep="")
ModObjectFile <- paste(output_folder, "/", ProjectName, "_ModObj.RData", sep="")

# read input files
x <- read_csv(ModelDataFile, col_types = cols())
x$Week <- mdy(x$Week)

# create some variables that are not in the raw data file
# you can create dummy variables here.
x$Year <- year(x$Week)
x$Year <- paste("Y", x$Year, sep="")
x$disp_olv_imp <- x$ONLV_IMP + x$DIS_IMP


Model_Spec <- read_csv(ModelSpecFile, col_types=cols())
Model_Spec <- Model_Spec %>% filter(Include == 1) # use Include to include (1)/exclude (0) variables
Model_setup <- read_csv(ModelSetupFile, col_types = cols())
fit_curves <- read_csv(ModelFitCurves, col_types = cols())

# define model object
mod_obj <- list()
mod_obj$ModelForm <- Model_setup$Value[Model_setup$Parameter=="ModelForm"]
mod_obj$Panel <- Model_setup$Value[Model_setup$Parameter=="Panel"]
mod_obj$Time <- Model_setup$Value[Model_setup$Parameter=="Time"]
mod_obj$BeginDate <- mdy(Model_setup$Value[Model_setup$Parameter=="BeginDate"])
mod_obj$EndDate <- mdy(Model_setup$Value[Model_setup$Parameter=="EndDate"])

mod_obj$data <- x
mod_obj$spec <- Model_Spec
mod_obj$fit_curves <- fit_curves
mod_obj$SimStart <- mdy(Model_setup$Value[Model_setup$Parameter=="SimStart"])
mod_obj$SimEnd <- mdy(Model_setup$Value[Model_setup$Parameter=="SimEnd"])
mod_obj$mroi_step <- as.numeric(Model_setup$Value[Model_setup$Parameter == "Mroi"])

#######################################
# data check
#######################################
mod_obj <- Check_Data(mod_obj)

#########################
# variable transformation
#########################
mod_obj <- Transform(mod_obj)
mod_obj$data <- mod_obj$data[mod_obj$data[[mod_obj$Time]] >= mod_obj$BeginDate & mod_obj$data[[mod_obj$Time]] <= mod_obj$EndDate,]

########################
# Run model
########################
print("Run model...")
mod_obj <- Run_Model(obj = mod_obj)
#mod_obj <- Run_Model_Time(obj = mod_obj)
write_csv(mod_obj$Model$coefficients, CoefficientsFile)
write_csv(mod_obj$Model$act_pred, ActualPredictedFile)
write_csv(mod_obj$Model$result_all, spec_coef_vifFile)

######################
# Decomp calculation
######################

mod_obj <- Decomp(obj = mod_obj, incl_spent = F)
mod_obj$chart_stackedarea <- decomp_chart_stackedarea(mod_obj)
write_csv(mod_obj$Decomposition, DecompFile)

####################
# Simulation to get response curve
####################
#mod_obj <- responsecurve(obj = mod_obj, showPlot=FALSE )
#write_csv(mod_obj$ResponseCurve, RCFile)

##################
# ABCs
##################
#mod_obj <- fitABC(mod_obj, showPlot=T)

# let's save the model object to a RData file. This object will be loaded later at post-model calculation:
# unnest, combine response curves and calculate abc.
save(mod_obj, file=ModObjectFile)
print("All done.")


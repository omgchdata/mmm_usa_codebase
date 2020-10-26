#############################################
# Julia Liu : This R code runs msmp example
#############################################
library(car)
library(lmtest)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(ggplot2)

#Define the server : pc or mac
if (Sys.info()['sysname'] == "Darwin") {server <- "/Volumes"} else {server <- "//nyccentral"}
code_dir <- "C:/Users/julia.liu/OneDrive - OneWorkplace/Documents/MyWork/an_ms_modelplatform/msmp/R/"

source(paste(code_dir, "msmp_setup.R", sep = ""))
source(paste(code_dir, "Check_Data.R", sep = ""))
source(paste(code_dir, "Transform.R", sep = ""))
source(paste(code_dir, "Run_Model.R", sep = ""))
source(paste(code_dir, "my_bayes_v2.R", sep = ""))
source(paste(code_dir, "Decomp.R", sep = ""))
source(paste(code_dir, "decomp_summary.R", sep = ""))
source(paste(code_dir, "DueToChart.R", sep = ""))
source(paste(code_dir, "MAPE.R", sep = ""))
source(paste(code_dir, "responsecurve.R", sep = ""))
source(paste(code_dir, "unnestr3.0.R", sep = ""))
source(paste(code_dir, "abc.R", sep = ""))
source(paste(code_dir, "abc_onls.R", sep = ""))
source(paste(code_dir, "fitABC.R", sep = ""))

#######  define project directories ##############
# please edit these lines to specify the path to the project folder. 
ProjectName <-  "main_model"            # the name of the sub-folder that contains the model project
OutDir <- "output"
RootDirectory <- "C:/Users/julia.liu/OneDrive - OneWorkplace/Documents/MyWork/an_ms_modelplatform/mmm_unnest_example/"
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

RCFile <- paste(output_folder,"/", ProjectName, "_ResponseCurve.csv", sep="")
kpi_spentFile <- paste(output_folder,"/", ProjectName, "_kpi_spent.csv", sep="")
ModObjectFile <- paste(output_folder, "/", ProjectName, ".RData", sep="")
ModelAllResultFile <- paste(output_folder, "/", ProjectName, "_result_workbook.xlsx", sep="")

# read input files
x <- read_csv(ModelDataFile, col_types = cols())
Model_Spec <- read_csv(ModelSpecFile, col_types=cols())
Model_Spec <- Model_Spec %>% filter(Include == 1) # use Include to include (1)/exclude (0) variables
Model_setup <- read_csv(ModelSetupFile, col_types = cols())
# the FitCurves is an optional file.
fit_curves <- read_csv(ModelFitCurves, col_types = cols())

# define model object
mod_obj <- list()    # an empty list 
mod_obj$data <- x    # ModelData file
# Variables.csv file, defines the model variables specifications
mod_obj$spec <- read_csv(ModelSpecFile, col_types=cols()) %>% 
  dplyr::filter(Include == 1) # use Include to include (1)/exclude (0) variables
# ModelSetup.csv : defines the model-level specifications
mod_obj$setup <- read_csv(ModelSetupFile, col_types = cols())
mod_obj <- msmp_setup(mod_obj)    # msmp_setup unpack the ModelSetup
# fit_curve for adr transformation. 
mod_obj$fit_curves <- fit_curves

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

######################
# Decomp calculation
######################
mod_obj <- Decomp(obj = mod_obj, incl_spent = F)
# contribution summary (at the model variable level) for a user specified time period
d1 <- decomp_summary(mod_obj$Decomposition, mod_obj$SimStart, mod_obj$SimEnd, "sales")
d2 <- decomp_summary(mod_obj$Decomposition, mod_obj$SimStart-365, mod_obj$SimEnd-365, "sales")
d <- full_join(d1, d2)
#print(d)
# generate due to chart
dueto <- DueToChart(df=mod_obj$Decomposition, 
                    spec = mod_obj$spec, 
                    startdate_current = mod_obj$SimStart, 
                    enddate_current = mod_obj$SimEnd, 
                    startdate_previous = mod_obj$SimStart-365,
                    enddate_previous = mod_obj$SimEnd - 365)
#print(dueto$pct)  
#print(dueto$chart)

####################
# Simulation to get response curve
####################
mod_obj <- responsecurve(obj = mod_obj, showPlot=T)
write_csv(mod_obj$ResponseCurve, RCFile)

##################
# ABCs
##################
#mod_obj <- fitABC(mod_obj, showPlot=T)

# let's save the model object to a RData file. This object will be loaded later at post-model calculation:
# unnest, combine response curves and calculate abc.
save(mod_obj, file=ModObjectFile)
# let's also save some of the model results to an excel workbook.
allresultlist <- list(mod_obj$Model$coefficients,
                      mod_obj$Model$act_pred, 
                      mod_obj$Model$result_all, 
                      mod_obj$Decomposition,
                      d)
write.xlsx(allresultlist, ModelAllResultFile, asTable = FALSE, sheetName=c("Coefficients","Act vs Pred","Diagnostics", "Decomps", "Decomp_compare"))
print("All done.")


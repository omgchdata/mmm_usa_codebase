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
code_dir <- paste(server, "/Annalect/BrandScience/msmp/R/", sep="")

source(paste(code_dir, "msmp_setup.R", sep = ""))
source(paste(code_dir, "Check_Data.R", sep = ""))
source(paste(code_dir, "Transform.R", sep = ""))
source(paste(code_dir, "Run_Model.R", sep = ""))
source(paste(code_dir, "Run_Model_Panel.R", sep = ""))
source(paste(code_dir, "my_bayes.R", sep = ""))
source(paste(code_dir, "MAPE.R", sep = ""))
source(paste(code_dir, "act_pred.R", sep = ""))
source(paste(code_dir, "scatterhist.R", sep = ""))
source(paste(code_dir, "Decomp_v2.R", sep = ""))
source(paste(code_dir, "DueToChart.R", sep = ""))
source(paste(code_dir, "responsecurve.R", sep = ""))
source(paste(code_dir, "decomp_summary_panel.R", sep = ""))
source(paste(code_dir, "decomp_summary_temp.R", sep = ""))

#######  define project directories ##############
# please edit these lines to define the path to the project folder.
ProjectName <-  "msmp_panel_example"            # the name of the subfolder that contains the model project
OutDir <- "output"
RootDirectory <- "C:/Users/julia.liu/OneDrive - OneWorkplace/Documents/MyWork/msmp/doc/"
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
CoefficientsFile <- paste(output_folder,  "/", ProjectName, "_Coefficients.csv", sep="")
#ActualPredictedFile <- paste(output_folder, "/", ProjectName, "_ActPred.csv", sep="")
DecompFile <- paste(output_folder,  "/", ProjectName, "_Decomp.csv", sep="")
RCFile <- paste(output_folder,"/", ProjectName, "_ResponseCurve.csv", sep="")
kpi_spentFile <- paste(output_folder,"/", ProjectName, "_kpi_spent.csv", sep="")
ModObjectFile <- paste(output_folder, "/", ProjectName, "_ModObj.RData", sep="")
#DecompSumFile <- paste(output_folder, "/", ProjectName, "_DecompSummary.csv",  sep="")
ModelAllResultFile <- paste(output_folder, "/", ProjectName, "_result_workbook.xlsx", sep="")

# read input files
x <- read_csv(ModelDataFile, col_types = cols())
# sometime the .csv file has the date in a mm/dd/yy format, this line changed it to the date formate the code requires which is yyyy-mm-dd
#x$Week<- mdy(x$Week)
# you can add some variables that are not in the _ModelData.csv here.
# For example dummy variable and etc. 

# define model object
mod_obj <- list()
mod_obj$data <- x
mod_obj$spec <- read_csv(ModelSpecFile, col_types=cols()) %>% 
  dplyr::filter(Include == 1) # use Include to include (1)/exclude (0) variables
mod_obj$setup <- read_csv(ModelSetupFile, col_types = cols())

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
print("Run model...")
mod_obj <- Run_Model_Panel(mod_obj)
#with(mod_obj$Model$freq_priors_bayes[mod_obj$Model$freq_priors_bayes$Variable=="search_t", ], scatterhist(estimate_freq, estimate_bayes, xlab="frequentist", ylab="HB"))

write_csv(mod_obj$Model$Priors, PriorFile)
write_csv(mod_obj$Model$coefficients, CoefficientsFile)
######################
# Decomp calculation
######################

mod_obj <- Decomp(obj = mod_obj, incl_spent = F)
write_csv(mod_obj$Decomposition_panel, DecompFile)

#dueto <- DueToChart(mod_obj$Decomposition, mod_obj$spec)
#ggplotly(dueto$chart)

#decomp_sum <- panel_decomp(mod_obj)
#write_csv(decomp_sum, DecompSumFile)
####################
# Simulation to get response curve
####################
#mod_obj <- responsecurve_panel(obj = mod_obj, showPlot=FALSE )
#write_csv(mod_obj$ResponseCurve, RCFile)

# let's save the model object to a RData file. This object can be loaded later at post-model calculation:
# unnest, combine response curves and calculate abc.
save(mod_obj, file=ModObjectFile)

allresultlist <- list(mod_obj$Model$Priors, mod_obj$Model$coefficients,mod_obj$Model$act_pred, mod_obj$Model$result_all, mod_obj$Decomposition_panel, mod_obj$Decomposition)
write.xlsx(allresultlist, ModelAllResultFile, asTable = FALSE, sheetName=c("Priors", "Coefficients","Act vs Pred","Diagnostics","Decomps_panel", "Decomps_national"))
print("All done.")


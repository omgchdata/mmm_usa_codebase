#############################################
# Julia Liu : This R code runs msmp example
#############################################
library(car)
library(lmtest)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)
library(onls)
library(plotly)
#needs(car, lmtest, tidyverse, lubridate, stringr, onls)
#Define the server : pc or mac
if (Sys.info()['sysname'] == "Darwin") {server <- "/Volumes"} else {server <- "//nyccentral"}

code_dir <- paste0(server, "/Annalect/BrandScience/msmp/R/")

source(paste(code_dir, "Run_Model.R", sep = ""))
source(paste(code_dir, "my_bayes_v2.R", sep = ""))
source(paste(code_dir, "Check_Data.R", sep = ""))
source(paste(code_dir, "adrnew.R", sep = ""))
source(paste(code_dir, "Transform.R", sep = ""))
source(paste(code_dir, "act_pred.R", sep = ""))
source(paste(code_dir, "Decomp.R", sep = ""))
source(paste(code_dir, "decomp_summary.R", sep = ""))
source(paste(code_dir, "contributions.R", sep = ""))
source(paste(code_dir, "MAPE.R", sep = ""))
source(paste(code_dir, "responsecurve_wip.R", sep = ""))
source(paste(code_dir, "DueToChart.R", sep = ""))
source(paste(code_dir, "unnestr3.0.R", sep = ""))
source(paste(code_dir, "abc.R", sep = ""))
source(paste(code_dir, "fitABC.R", sep = ""))
source(paste(code_dir, "predict_msmp.R", sep = ""))
source(paste(code_dir, "compare_models.R", sep = ""))
source(paste(code_dir, "plot_msmp.R", sep = ""))

#######  define project directories ##############
# please edit these lines to define the path to the project folder.
ProjectName <-  "main_model"            # the name of the subfolder that contains the model project
OutDir <- "output"
RootDirectory <- "//nyccentral/Annalect/BrandScience/msmp/doc/"
ProjectDirectory <- paste(RootDirectory, ProjectName, "/", sep="")   # this is the full path of the project

###################
# setup
###################
# define input file names
ModelDataFile <- paste(ProjectDirectory, ProjectName, "_ModelData.csv", sep="")
ModelSetupFile <- paste(ProjectDirectory, ProjectName, "_ModelSetup.csv", sep="")
ModelSpecFile <- paste(ProjectDirectory, ProjectName, "_Variables.csv", sep="")
ModelFitCurves <- paste(ProjectDirectory, ProjectName, "_Fit_Curves.xlsx", sep="")
# output file names
output_folder <- paste(ProjectDirectory, OutDir, sep="")
if(!file.exists(output_folder)) {
  dir.create(output_folder)
}

ModObjectFile <- paste(output_folder, "/", ProjectName, ".RData", sep="")
allresultFile <- paste(output_folder, "/", ProjectName, "_Model_all_results_", gsub(":","_",Sys.time()), ".xlsx", sep="")
resultPlotsFile <- paste(output_folder, "/", ProjectName, "_plots", ".xlsx", sep="")


# read input files
x <- read_csv(ModelDataFile, col_types = cols())
#x$Week <- mdy(x$Week)

Model_Spec <- read_csv(ModelSpecFile, col_types=cols())
Model_Spec <- Model_Spec %>% filter(Include == 1) # use Include to include (1)/exclude (0) variables
Model_setup <- read_csv(ModelSetupFile, col_types = cols())

sheets <- excel_sheets(ModelFitCurves)
fit_curves <- list()
for (i in 1:length(sheets)) {
  fit_curves[[i]] <- read_excel(ModelFitCurves, sheet = i)
}
names(fit_curves) = sheets

# define model object
mod_obj <- list()
mod_obj$ModelForm <- Model_setup$Value[Model_setup$Parameter=="ModelForm"]
mod_obj$Panel <- Model_setup$Value[Model_setup$Parameter=="Panel"]
mod_obj$Time <- Model_setup$Value[Model_setup$Parameter=="Time"]
mod_obj$BeginDate <- mdy(Model_setup$Value[Model_setup$Parameter=="BeginDate"])
mod_obj$EndDate <- mdy(Model_setup$Value[Model_setup$Parameter=="EndDate"])
mod_obj$Setup <- Model_setup

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
mod_obj$Model$act_pred <- act_pred(obj = mod_obj)
ggplotly(mod_obj$Model$act_pred_chart)

######################
# Decomp calculation
######################

mod_obj <- Decomp(obj = mod_obj, incl_spent = F)

# now just to show what you can do with the decomposition output,
# here are several different ways to summarize the decomp table 
# calculate decomp contribution for 2 time period against predicted.
decomp_sum_cy <- decomp_summary(mod_obj$Decomposition, mod_obj$SimStart, mod_obj$SimEnd, "predicted")
decomp_sum_yago <- decomp_summary(mod_obj$Decomposition, mod_obj$SimStart-52*7, mod_obj$SimEnd-52*7, "predicted")

names(decomp_sum_cy) = c("Variables", "Sum_cy", "Percent_cy")
names(decomp_sum_yago) = c("Variables", "Sum_yago", "Percent_yago")
decomp_cy_yago = left_join(decomp_sum_cy, decomp_sum_yago)

# calculate decomp contribution against actual dependent variable
decomp_sum_actual <- decomp_summary(mod_obj$Decomposition, mod_obj$SimStart, mod_obj$SimEnd, mod_obj$spec$Orig_Variable[tolower(mod_obj$spec$Variable_Type) == "dependent"])

# create due to result (table and chart)
mod_obj$dueto = DueToChart(mod_obj$Decomposition, mod_obj$spec, mod_obj$SimStart, mod_obj$SimEnd, mod_obj$SimStart-52*7, mod_obj$SimEnd-52*7)
print(mod_obj$dueto$pct)
print(mod_obj$dueto$chart)

mod_obj$contrib <- contributions(mod_obj, mod_obj$SimStart, mod_obj$SimEnd, by="aggregatevariable")
wf1 <- (waterfall_chart(Variable = mod_obj$contrib$AggregateVariable, Value = mod_obj$contrib$decomp, percent=F))
wf2 <- (waterfall_chart(Variable = mod_obj$contrib$AggregateVariable, Value = mod_obj$contrib$Percent, percent=T))

####################
# Simulation to get response curve
####################
mod_obj <- responsecurve(obj = mod_obj, showPlot=T )

##################
# fit abc curves on the response curves (that have spend information) to get a, b, and c
##################
mod_obj <- fitABC(mod_obj, newABC = F,showPlot=T)
print(mod_obj$abc)

########################################################
# save the results to excel workbook and Rdata object
########################################################
allresultlist <- list(mod_obj$Model$coefficients,mod_obj$Model$act_pred, mod_obj$Model$result_all, mod_obj$Decomposition, decomp_sum, mod_obj$kpi_spent)
write.xlsx(allresultlist,  allresultFile, asTable = FALSE, sheetName=c("Coefficients","Act vs Pred","Diagnostics","Decomps","% contrib", "mroi and roi ratio"))

plot_wb <- createWorkbook() 
name <- addWorksheet(plot_wb, "act vs predict") 
plot(mod_obj$Model$act_pred_chart)
insertPlot(plot_wb, sheet=name)

name <- addWorksheet(plot_wb, "decomp waterfall")
plot(wf2)
insertPlot(plot_wb, sheet=name)

name <- addWorksheet(plot_wb, "due to")
plot(mod_obj$dueto$chart)
insertPlot(plot_wb, sheet=name)
name <- addWorksheet(plot_wb, "TV response curve fit") 
plot(mod_obj$rc_fit_plots)
insertPlot(plot_wb, sheet=name) 

saveWorkbook(plot_wb,resultPlotsFile, overwrite = TRUE) 


# let's save the model object to a RData file. This object will be loaded later at post-model calculation:
# unnest, combine response curves and calculate abc.
save(mod_obj, file=ModObjectFile)
print("All done.")


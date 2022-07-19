#############################################
# Julia Liu : This R code runs an msmp example in the loop run mode
#############################################
needs(car, lmtest, tidyverse, lubridate, reshape, stringr, RcppRoll, onls, svDialogs, utils, taRifx ,openxlsx, pbapply)

#Defie the server : pc or mac
if (Sys.info()['sysname'] == "Darwin") {server <- "/Volumes"} else {server <- "//nyccentral"}
code_dir <- "//nyccentral/annalect/BrandScience/msmp/R/"

source(paste(code_dir, "Run_Model.R", sep = ""))
source(paste(code_dir, "my_bayes_v2.R", sep = ""))
source(paste(code_dir, "Check_Data.R", sep = ""))
source(paste(code_dir, "adrnew.R", sep = ""))
source(paste(code_dir, "Transform.R", sep = ""))
source(paste(code_dir, "resid_var_cor.R", sep = ""))
source(paste(code_dir, "Decomp_working.R", sep = ""))
source(paste(code_dir, "decomp_chart_stackedarea.R", sep = ""))
source(paste(code_dir, "MAPE.R", sep = ""))
source(paste(code_dir, "responsecurve.R", sep = ""))
source(paste(code_dir, "unnestr3.0.R", sep = ""))
source(paste(code_dir, "abc.R", sep = ""))
source(paste(code_dir, "abc_onls.R", sep = ""))
source(paste(code_dir, "decomp_summary.R", sep = ""))
source(paste(code_dir, "create_loop.R", sep = ""))
source(paste(code_dir, "loop_model.R", sep = ""))
source(paste(code_dir, "simple_loop_pull.R", sep = ""))
source(paste(code_dir, "write_loop.R", sep = ""))

#######  define project directories ##############
# please edit these lines to define the path to the project folder.
ProjectName <-  "main_model"            # the name of the subfolder that contains the model project
OutDir <- "output"
RootDirectory <- "//nyccentral/annalect/BrandScience/msmp/doc/"
#RootDirectory <- "C:/Users/julia.liu/OneDrive - OneWorkplace/Documents/MyWork/msmp/doc/"
ProjectDirectory <- paste(RootDirectory, ProjectName, "/", sep="")   # this is the full path of the project

# Set the working directory for convenience
setwd(ProjectDirectory)

# Generates all model scenarios

# Reads the csv file with options separated by semicolons

originalcsv <- read.csv(paste(ProjectDirectory, ProjectName, "_Variables_massrun.csv", sep=""),stringsAsFactors = FALSE)

originalcsv <- originalcsv[originalcsv$Include!=0,]

# create the loop folders if they do not yet exist

dir.create("Loop_variables", showWarnings = FALSE)
dir.create("Loop_output", showWarnings = FALSE)
dir.create("temp", showWarnings = FALSE)


count_loop(originalcsv) -> count_loop_num

# asks if you want to proceed if there is a lot of models

proceedmodeliterations <- askYesNo(paste(count_loop_num, "models need to be run, proceed?"), default = TRUE, 
                                   prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel")))
)
if(is.na(proceedmodeliterations)) {proceedmodeliterations=FALSE}

# if yes, go ahead and run models


if(proceedmodeliterations) {
  # record email for error messages (this function not useful yet)
  # user_email <- dlgInput("What is your email address to send error message during loop run?", Sys.info()["user"])$res
  
  # Write csv files for loop scenarios - this can be skipped if you use R to view them alone
  
  csv_list <- create_loop(originalcsv)
  op <- pboptions(type="win")
  
  for(i in c(1:length(csv_list))){
    write.csv(csv_list[[i]],paste(ProjectDirectory, "Loop_variables//", ProjectName, "_Variables",i,".csv", sep=""),row.names = FALSE)
  }
  
  # define input file names (other than model specs)
  
###################
# setup
###################
# define input file names
ModelDataFile <- paste(ProjectDirectory, ProjectName, "_ModelData.csv", sep="")
ModelSetupFile <- paste(ProjectDirectory, ProjectName, "_ModelSetup.csv", sep="")
ModelFitCurves <- paste(ProjectDirectory, ProjectName, "_Fit_Curves.xlsx", sep="")
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
#x$Week <- mdy(x$Week)

# create some variables that are not in the raw data file
# you can create dummy variables here.
x$Year <- year(x$Week)
x$Year <- paste("Y", x$Year, sep="")


Model_setup <- read_csv(ModelSetupFile, col_types = cols())
sheets <- excel_sheets(ModelFitCurves)
fit_curves <- list()
for (i in 1:length(sheets)) {
  fit_curves[[i]] <- read_excel(ModelFitCurves, sheet = i)
}
names(fit_curves) = sheets


###########################################################################################################


# Name of the result tables

list("coefficientsheet","vifsheet","tstatsheet","decompsumsheet", "decompsumtotalsheet", "r2sheet") -> allresultnames

# Run loop at once if there are fewer than 1000 models

# This part actually runs the models (under 1000)  
if(length(csv_list) < 1000) {
  sink("File")
  suppressMessages(pblapply(csv_list, run_a_loop_model, modsetup = Model_setup, moddata=x, ft_curves=fit_curves)) -> modeldatabase
  sink()
  
  # pulling numerical results from models into tables
  
  coefficientdatabase <- pblapply(modeldatabase, pullmodel)
  decompsumlist <- pblapply(modeldatabase, pulldecomp)
  decompsumtotallist <- pblapply(modeldatabase, pulldecomptotal)
  
  coefficientsheet <- simple_loop_pull(originalcsv$Orig_Variable[originalcsv$Orig_Variable != ""],coefficientdatabase, "Estimate",originalcsv$Trans_Variable[originalcsv$Trans_Variable != ""])
  vifsheet <- simple_loop_pull(originalcsv$Orig_Variable[originalcsv$Orig_Variable != ""],coefficientdatabase, "VIF",originalcsv$Trans_Variable[originalcsv$Trans_Variable != ""]) 
  tstatsheet <- simple_loop_pull(originalcsv$Orig_Variable[originalcsv$Orig_Variable != ""],coefficientdatabase, "Tvalue",originalcsv$Trans_Variable[originalcsv$Trans_Variable != ""]) 
  decompsumsheet <- decomp_loop_pull(originalcsv$Orig_Variable[originalcsv$Orig_Variable != ""],decompsumlist, "Percent",originalcsv$Trans_Variable[originalcsv$Trans_Variable != ""])
  decompsumtotalsheet <-  decomp_loop_pull(originalcsv$Orig_Variable[originalcsv$Orig_Variable != ""],decompsumtotallist, "Percent",originalcsv$Trans_Variable[originalcsv$Trans_Variable != ""])
  r2sheet <- single_loop_pull(c("R2"),coefficientdatabase, "R2")
  
  # write these tables of model results to csv
  
  lapply(X=lapply(X = allresultnames,FUN = as.name),eval) -> allresultlist
  lapply(X=allresultnames,write_loop)
  
  # and an Excel file (if possible)
  
  tryCatch({write.xlsx(allresultlist,  paste0(ProjectDirectory, "\\Loop_output\\Browse_all_results.xlsx"), asTable = FALSE, sheetName=c("Coefficients","VIF","T-stats","Decomps","total contrib", "R2"),rowNames=TRUE)},error=function(e){})
  
}

# if there are more than 1000 models, split the scenarios into 1000s to save memory space

if(length(csv_list) >= 1000) {
  
  # splitting the models    
  allmodelnum_split <- split(c(1:length(csv_list)), ceiling(seq_along(c(1:length(csv_list)))/1000))
  
  # run the separate 1000s of models    
  
  for (i in c(1:length(allmodelnum_split))) {
    sink("File")
    suppressMessages(pblapply(csv_list[allmodelnum_split[[i]]], run_a_loop_model, modsetup = Model_setup, moddata=x)) -> partialdatabase
    sink()
    
    # saving models to hard drive    
    
    saveRDS(partialdatabase, file = paste0(ProjectDirectory, "\\temp\\partialmodels_",i,".rds"))
    rm(partialdatabase)
  }
  
  # reading from hard drive to pull model results      
  
  for (i in c(1:length(allmodelnum_split))) {
    modeldatabase <- readRDS(file = paste0(ProjectDirectory, "\\temp\\partialmodels_",i,".rds"))
    
    coefficientdatabase <- pblapply(modeldatabase, pullmodel)
    decompsumlist <- pblapply(modeldatabase, pulldecomp)
    decompsumtotallist <- pblapply(modeldatabase, pulldecomptotal)
    
    # prepare the result tables
    
    coefficientsheet <- simple_loop_pull(originalcsv$Orig_Variable[originalcsv$Orig_Variable != ""],coefficientdatabase, "Estimate",originalcsv$Trans_Variable[originalcsv$Trans_Variable != ""])
    vifsheet <- simple_loop_pull(originalcsv$Orig_Variable[originalcsv$Orig_Variable != ""],coefficientdatabase, "VIF",originalcsv$Trans_Variable[originalcsv$Trans_Variable != ""]) 
    tstatsheet <- simple_loop_pull(originalcsv$Orig_Variable[originalcsv$Orig_Variable != ""],coefficientdatabase, "Tvalue",originalcsv$Trans_Variable[originalcsv$Trans_Variable != ""]) 
    decompsumsheet <- decomp_loop_pull(originalcsv$Orig_Variable[originalcsv$Orig_Variable != ""],decompsumlist, "Percent",originalcsv$Trans_Variable[originalcsv$Trans_Variable != ""])
    decompsumtotalsheet <-  decomp_loop_pull(originalcsv$Orig_Variable[originalcsv$Orig_Variable != ""],decompsumtotallist, "Percent",originalcsv$Trans_Variable[originalcsv$Trans_Variable != ""])
    r2sheet <- single_loop_pull(c("R2"),coefficientdatabase, "R2")
    
    # write tables of model results (in 1000s) to temporary csv files to combine later
    lapply(X=allresultnames,write_temp_loop)
    
    # remove temporary model objects from memeory
    
    rm(modeldatabase)
    rm(coefficientdatabase)
    rm(decompsumlist)
    rm(decompsumtotallist) 
    rm(coefficientsheet)
    rm(vifsheet)
    rm(tstatsheet)
    rm(decompsumsheet)
    rm(decompsumtotalsheet)
    rm(r2sheet)
    
  }
  
  # Reard from the hard drive to combine the loop results of 1000s
  allresultlist <- vector(mode = "list", length = length(allresultnames))
  for (b in c(1:length(allresultnames))) {
    for (i in c(2:length(allmodelnum_split))) {
      combine_loop(allresultnames[[b]], i, allresultlist[[b]]) -> allresultlist[[b]]
    }
  }
  
  # Write these into csv (actual csv files with all results this time!)
  
  for (i in c(1:length(allresultlist))) {
    write.csv(allresultlist[[i]], paste0(ProjectDirectory, "\\Loop_output\\",allresultnames[[i]],".csv"))
  }
  
  tryCatch({write.xlsx(allresultlist,  paste0(ProjectDirectory,
                                              "\\Loop_output\\Browse_all_results.xlsx"), 
                       asTable = FALSE,
                       sheetName=c("Coefficients","VIF","T-stats","Decomps","total contrib", "R2"),
                       rowNames=TRUE)},error=function(e){})
}

}

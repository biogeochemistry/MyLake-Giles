### Run MyLake model Matlab script via R,
### including changing parameters in parameter file
### for manual calibration
### RMP (last modified 2019-MAR-27)

## NOTE:  In Matlab script ("RMC_modelGILES_v12_rmp.m"), local directory paths
## will need to be updated with the location of the model components and associated
## files. There are EIGHT path names in the Matlab script that require this updating
## (they start with "G:\My Drive\" ...)


## load necessary packages

library(matlabr)
library(xlsx)


## function to run MyLake Matlab script via R,
## with arguments of folder directory (path),
## parameter value to update for the model run (param.value),
## and row location in the parameter file for this value (row.loc)

run_MyLake_model <- function(path, param.value, row.loc) {
  # set folder directory where all necessary files are located;
  # this is also were the MODELLED WATER TEMPERATURE & SNOW/ICE OUTPUT will be saved
  # as a .csv file (you can change the output variable of interest
  # directly, at the end of the Matlab script file)
  setwd(path)
  
  # load in parameter file sheet
  workbook = loadWorkbook("GILES_para_v12.xls")
  sheet = getSheets(workbook)[[1]]
  
  # update parameter file sheet with specified parameter
  addDataFrame(
    param.value,
    sheet = sheet,
    startRow = row.loc,
    startColumn = 2,
    col.names = FALSE,
    row.names = FALSE
  )
  
  # save over with the new parameter value in the file
  saveWorkbook(workbook, "GILES_para_v12.xls")
  
  # run the Matlab script
  # (a new window of Matlab Commander will pop up during this;
  # you do NOT need to interact with this window, it will
  # automatically close upon completion of model run)
  run_matlab_script(fname = "RMC_modelGILES_v12_rmp.m")
}


## example run of function
## (can be used in for-loops to test multiple parameter values,
## or to manually adjust multiple parameters)

run_MyLake_model(path = "G:/My Drive/rachel-PC/Miami-OH/Sentinel North - Laval/MyLake_public/v12/Giles_application_GitHub",
                 param.value = 41.3765,   # this would change the latitude value
                 row.loc = 8)             # location of latitude parameter is row 8 of parameter file


## option to rename output csv files:

# file.rename(from = "ModelledTemp-Giles_modelOutput.csv",
#             to = "NewFileName-Giles-ModelledTemp.csv")

# file.rename(from = "ModelledHis-Giles_modelOutput.csv",
#             to = "NewFileName-Giles-ModelledHis.csv")

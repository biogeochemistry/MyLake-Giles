### AUTO-CALIBRATION OF MyLake USING NELDER-MEAD METHOD
### REVISED BY RMP
### 2019 MAR 28

### NOTE:  This script was originally written by Robert Ladwig for GLM,
### and has since been modified to fit MyLake by RMP. Credit and additional
### commments throughout from Robert.

## NOTE:  Because this calls Matlab externally and needs lots of interations to converge,
## it may take a long time to run (e.g., 8 parameters required ~600 runs can take up to ~6-8 hours).
## So, it can be ideal to run overnight or in the background without issue if R is left open and undisturbed.



# author: Robert Ladwig
# date: 03/11/2019
# project: automatic calibration routine for ISIMIP-GLM project

# CMA-ES theory: The CMA-ES implements a stochastic variable-metric method. In the very particular case of a 
# convex-quadratic objective function the covariance matrix adapts to the inverse of the Hessian matrix, up to a scalar 
# factor and small random fluctuations. The update equations for mean and covariance matrix maximize a likelihood while 
# resembling an expectation-maximization algorithm. (https://www.rdocumentation.org/packages/adagio/versions/0.7.1/topics/CMAES)

rm(list = ls())

# Load packages

library(rLakeAnalyzer)
library(zoo)
library(anytime)
library(optimx)
library(adagio)
library(ncdf4)
library(nloptr)
library(Hmisc)
library(matlabr)
library(xlsx)
library(tidyverse)
library(lubridate)
library(Metrics)
library(akima)
library(scales)


# Set working diretory
path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)


# bring in observed water temperature data, clean up the data, and subset a bit as needed

setwd(paste0(path,"/Observations"))
obs.temp=read.csv("GILEStemp_all.csv",header=T)
colnames(obs.temp)=c("Date","Time","Depth","Obs_Temp","Instrument")
obs.temp.long=obs.temp %>%
  select(Date,Depth,Obs_Temp,Instrument) %>%
  mutate(Date=ymd(Date),
         Depth=Depth/-100) %>%
  filter(Date>=ymd("2017-08-11"),
         Instrument=="miniDOT",
         !Depth==0.5) %>%
  arrange(Date,Depth)


## MAKE ADJUSTMENTS TO PARAMETERS, BOUNDARIES, STARTING VALUE, ETC. HERE:
## 1) parameter names (order of these must be the same as in the following vectors)
## 2) lower and upper bound constraints for each parameter ("lb" and "ub")
## 3) initial starting point/guess for each parameter ("values.optim")
## 4) row locations in the associated parameter file for each parameter value ("row.locs")

## WITHIN THE "mylakeFUN", UPDATE:
## 1) the parameters to be used, simply adding the name and the "p[X]"
## 2) ensure there is one "addDataFrame" command for each parameter,
##    where the parameter name is the first argument,
##    and the "row.locs[X]" number is the same number as is "p[X]"


# parameters for auto-calibration:
par.names.order=c("IscV",
                  "Kz_ak_ice",
                  "IscT",
                  "albedo_melt_ice",
                  "albedlo_melt_snow",
                  "C_shelter",
                  "swa_b0",
                  "swa_b1")


# constraints for all parameters (lb = lower bound, ub = upper bound)
lb <- c(0, 0.0001, 0, 0.1, 0.5, 0, 0.01, 0.01)
ub <- c(5, 0.016, 10, 0.7, 0.9, 1, 5, 5)


# initial guesses
values.optim <- c(2, 0.000898, 3, 0.3, 0.77, 0.1, 1, 0.5)


# row location in para_file for each p
row.locs=c(18, 5, 19, 10, 11, 7, 26, 27)


# main function

mylakeFUN <- function(p,row.locs){
  
  setwd(path)
  
  p <- wrapper_scales(p, lb, ub)
  
  
  ## read in parameter file, so R will automatically replace with parameter values based on optimization
  workbook <- loadWorkbook("GILES_para_v12.xls")
  para_file <- getSheets(workbook)[[1]]
  
  ## select parameters to optimize here:
  IscV <- p[1]
  Kz_ak_ice <- p[2]
  IscT <- p[3]
  albedo_melt_ice <- p[4]
  albedo_melt_snow <- p[5]
  C_shelter <- p[6]
  swa_b0 <- p[7]
  swa_b1 <- p[8]

  # save parameter estimates for posterior distribution in R global environment
  iter.val <<- iter.val+1
  est.param.vals[[iter.val]] <<- p
  
  ## locations of parameter values in the spreadsheet
  
  addDataFrame(IscV,sheet=para_file,startRow=row.locs[1],startColumn=2,col.names=FALSE,row.names=FALSE)
  addDataFrame(Kz_ak_ice,sheet=para_file,startRow=row.locs[2],startColumn=2,col.names=FALSE,row.names=FALSE)
  addDataFrame(IscT,sheet=para_file,startRow=row.locs[3],startColumn=2,col.names=FALSE,row.names=FALSE)
  addDataFrame(albedo_melt_ice,sheet=para_file,startRow=row.locs[4],startColumn=2,col.names=FALSE,row.names=FALSE)
  addDataFrame(albedo_melt_snow,sheet=para_file,startRow=row.locs[5],startColumn=2,col.names=FALSE,row.names=FALSE)
  addDataFrame(C_shelter,sheet=para_file,startRow=row.locs[6],startColumn=2,col.names=FALSE,row.names=FALSE)
  addDataFrame(swa_b0,sheet=para_file,startRow=row.locs[7],startColumn=2,col.names=FALSE,row.names=FALSE)
  addDataFrame(swa_b1,sheet=para_file,startRow=row.locs[8],startColumn=2,col.names=FALSE,row.names=FALSE)

  saveWorkbook(workbook,"GILES_para_v12.xls")
  
  
  # automatically run the MyLake Matlab script via R
  setwd(path)
  run_matlab_script(fname="RMC_modelGILES_v12_rmp.m")
  
  
  ## read in the model output, and clean up/extract the water temperature data
  file.name="ModelledTemp-Giles_modelOutput.csv"
  
  mod.temp=as.data.frame(t(read.csv(file.name,header=F)))
  colnames(mod.temp)=seq(0.5,24.5,by=1)
  mod.temp.all=mod.temp %>%
    mutate(Date=seq.Date(as.Date("2016-05-17"),as.Date("2018-12-31"),by=1),
           '4'=rowMeans(cbind(`3.5`,`4.5`)),
           '6'=rowMeans(cbind(`5.5`,`6.5`)),
           '8'=rowMeans(cbind(`7.5`,`8.5`)),
           '12'=rowMeans(cbind(`11.5`,`12.5`)),
           '14'=rowMeans(cbind(`13.5`,`14.5`)),
           '16'=rowMeans(cbind(`15.5`,`16.5`)),
           '18'=rowMeans(cbind(`17.5`,`18.5`)),
           '20'=rowMeans(cbind(`19.5`,`20.5`)),
           '22'=rowMeans(cbind(`21.5`,`22.5`))) %>%
    gather(key="Depth",value="Mod_Temp",`4`:`22`) %>%
    select(Date,Depth,Mod_Temp) %>%
    mutate(Depth=as.numeric(Depth)) %>%
    arrange(Date,Depth)
  
  
  ## compare model vs. observed water temperature and return RMSE value for set of parameter values
  diag.overall=mod.temp.all %>%
    full_join(obs.temp.long) %>%
    arrange(Date,Depth) %>%
    filter(!is.na(Mod_Temp),
           !is.na(Obs_Temp)) %>%
    summarize(RMSE=rmse(actual=Obs_Temp,predicted=Mod_Temp))
  
  print(paste("Water Temp. RMSE =",signif(diag.overall[1,1],3),"°C"))
  return(diag.overall[1,1])
}    


# some theory from http://cma.gforge.inria.fr/cmaes_sourcecode_page.html: The specific formulation of a (real) optimization problem has a tremendous impact 
#on the optimization performance. In particular, a reasonable parameter encoding is essential. All parameters should be rescaled such that they have presumably 
#similar sensitivity (this makes the identity as initial covariance matrix the right choice).

wrapper_scales <- function(x, lb, ub){
  y <-  lb+(ub-lb)/(10)*(x)
  return(y)
}


## run parameter optimization via Nelder-Mead method

## NOTE:  Because this calls Matlab externally and needs lots of interations to converge,
## it may take a long time to run (e.g., 8 parameters required ~600 runs can take up to ~6-8 hours).
## BUT this can be run in the background without issue if R is left open and undisturbed.

print("### NELDER-MEAD ###")
niter <- 1000
iter.val <- 0  
est.param.vals <- list()
t1 <- Sys.time()
mylakeOPT1 <- neldermead(values.optim, mylakeFUN, lower = rep(0,length(values.optim)), 
                         upper =  rep(10,length(values.optim)), nl.info = TRUE, 
                         control=list(xtol_rel = 1e-8, maxeval = niter),
                         row.locs=row.locs)
t2 <- Sys.time()
mylakeFUN(p=mylakeOPT1$par,row.locs=row.locs)
print(paste("RMSE",mylakeOPT1$value,"°C"))
print(data.frame("Parameter"=par.names.order,
                 "OptValue"=wrapper_scales(mylakeOPT1$par,lb,ub)))
print(t2-t1)



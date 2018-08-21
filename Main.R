rm(list = ls())
graphics.off()

library(deSolve)
library(ggplot2)
library(reshape)
library(gridExtra)
library(scales)
library(Matrix)
library(FME)

print('************* INITIALIZE INPUTS *************')
setwd('~/../Dropbox/HumanPopDynModel/Model/R/WorldPopulationModel - Difference')
print('COMPLETED.')

# ASSEMBLE INPUT MATRICES AND VECTORS

source('InitializeData.R')

# LOAD SUBSYSTEMS
print('------------- LOAD ECONOMY SUBMODEL')
source('Economy.R')

print('------------- LOAD RESOURCE SUBMODEL')
source('Resource.R')

print('------------- LOAD FOOD SUBMODEL')
source('Food.R')

print('------------- LOAD CLIMATE SUBMODEL')
source('Climate.R')

print('------------- LOAD POPULATION SUBMODEL')
source('Population.R')

print('------------- LOAD HEALTH & EDUCATION SUBMODEL')
source('HealthEducation.R')

print('------------- LOAD WATER SUBMODEL')
source('Water.R')
print('ALL COMPONENTS LOADED.')

# PRINT AND PLOT FUNCTION
printvar = function(x){
	if(!is.na(x)){
		print(paste(deparse(substitute(x))," = ",x))
	}
}
		
source('plotter.R')

# DEFINE MODEL
print('************* LOAD MAIN MODEL *************')

t0 = 1980
tf = 2080
delta_t = 1
delayyearlength = 1
source('WorldMod.R')
print('COMPLETED.')

########################### Initial Simulate

print('*************  SIMULATE MODEL   *************')
ptm = proc.time()
OutputData = WorldMod(t0,tf,delta_t,delayyearlength,InitValue,ParameterValue)
ptm = proc.time() - ptm
OutputData = data.frame(OutputData)
print('COMPLETED.')
print(ptm)
########################### Plot results

PlotFuncWithObs(OutputData)

########################### Run Calibration

print('*************  RUN CALIBRATION  *************')
# OptSolver = 'Newton'
# OptSolver = 'BFGS'
# OptSolver = 'CG'
OptSolver = 'Pseudo'
# OptSolver = 'Marq'
source('ModelCalibration.R')
print('COMPLETED.')

########################### Simulated Fitted Parameters

print('************* SIMULATE FITTED MODEL**********')
ptm = proc.time()
FitOutput = WorldMod(t0,tf,delta_t,delayyearlength,InitValue,FittedParameters)
ptm = proc.time() - ptm
print('COMPLETED.')
FitOutput = data.frame(FitOutput)
PlotFuncWithObs(FitOutput)
print(ptm)

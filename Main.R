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

# LOAD SUBMODELS
print('------------- LOAD ECONOMY SUBMODEL')
source('./Submodels/Economy.R')

print('------------- LOAD RESOURCE SUBMODEL')
source('./Submodels/Resource.R')

print('------------- LOAD FOOD SUBMODEL')
source('./Submodels/Food.R')

print('------------- LOAD CLIMATE SUBMODEL')
source('./Submodels/Climate.R')

print('------------- LOAD POPULATION SUBMODEL')
source('./Submodels/Population.R')

print('------------- LOAD HEALTH & EDUCATION SUBMODEL')
source('./Submodels/HealthEducation.R')

print('------------- LOAD WATER SUBMODEL')
source('./Submodels/Water.R')
print('ALL COMPONENTS LOADED.')

# PRINT AND PLOT FUNCTION
printvar = function(x){
	if(!is.na(x)){
		print(paste(deparse(substitute(x))," = ",x))
	}
}
		
source('plotter.R')

# DEFINE MODEL AND SET TIME STEPS
print('************* LOAD MAIN MODEL *************')
t0 = 1980
tf = 2080
delta_t = 1
delayyearlength = 1
source('WorldMod.R')
print('COMPLETED.')

########################### Run Local Calibration
print('*****RUN INITIAL PARAMETER ESTIMATION********')
print('------------- CALIBRATE ECONOMY SUBMODEL')
source('./Calibration/EconomyCalibration.R')

print('------------- CALIBRATE RESOURCE SUBMODEL')
source('./Calibration/ResourceCalibration.R')

print('------------- CALIBRATE FOOD SUBMODEL')
source('./Calibration/FoodCalibration.R')

print('------------- CALIBRATE CLIMATE SUBMODEL')
source('./Calibration/ClimateCalibration.R')

print('------------- CALIBRATE POPULATION SUBMODEL')
source('./Calibration/PopulationCalibration.R')

print('------------- CALIBRATE HEALTH & EDUCATION SUBMODEL')
source('./Calibration/HealthEducationCalibration.R')

print('------------- CALIBRATE WATER SUBMODEL')
source('./Calibration/WaterCalibration.R')
print('ALL COMPONENTS LOADED.')

OutputData = WorldMod(t0,tf,delta_t,delayyearlength,InitValue,ParameterValue)
PlotFuncWithObs(OutputData)

########################### Run Calibration

print('************RUN GLOBAL CALIBRATION***********')
# OptSolver = 'Newton'
# OptSolver = 'BFGS'
OptSolver = 'CG'
# OptSolver = 'Pseudo'
# OptSolver = 'Marq'
source('./Calibration/GlobalCalibration.R')
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

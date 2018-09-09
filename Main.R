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
# setwd('~/../Dropbox/HumanPopDynModel/Model/R/WorldPopulationModel - Difference')
# ASSEMBLE INPUT MATRICES AND VECTORS
source('InitializeData.R')
print('COMPLETED.')



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
source('Plotter.R')

# DEFINE MODEL AND SET TIME STEPS
print('************* LOAD MAIN MODEL *************')
t0 = 1980
tf = 2080
delta_t = 1
delayyearlength = 1
source('WorldMod.R')
print('COMPLETED.')

########################### Run Local Calibration

# PREDEFINE INITIAL GLOBAL PARAMETER VECTOR

print('*****LOAD SUBMODEL PARAMEATER ESTIMATION*****')
print('------------- CALIBRATE ECONOMY SUBMODEL')
load('./Calibration/CalibrationOutput/EconomyParmsEstimate.RData')

print('------------- CALIBRATE RESOURCE SUBMODEL')
load('./Calibration/CalibrationOutput/ResourceParmsEstimate.RData')

print('------------- CALIBRATE FOOD SUBMODEL')
load('./Calibration/CalibrationOutput/FoodParmsEstimate.RData')

print('------------- CALIBRATE CLIMATE SUBMODEL')
load('./Calibration/CalibrationOutput/ClimateParmsEstimate.RData')

print('------------- CALIBRATE POPULATION SUBMODEL')
load('./Calibration/CalibrationOutput/PopulationParmsEstimate.RData')

print('------------- CALIBRATE HEALTH & EDUCATION SUBMODEL')
load('./Calibration/CalibrationOutput/HealthEducationParmsEstimate.RData')

print('------------- CALIBRATE WATER SUBMODEL')
load('./Calibration/CalibrationOutput/WaterParmsEstimate.RData')
print('ALL COMPONENTS LOADED.')

# COMBINE PARAMETER ESTIMATION FROM ALL SUBMODELS

print('***** ASSEMBLE GLOBAL PARAMETER VALUES *****')
source('CalibDataFormat.R')


OutputData = WorldMod(t0,tf,delta_t,delayyearlength,LocalFitInitValue,
	LocalFitParmameterValue)
PlotFuncWithObs(OutputData)
write.csv(OutputData,file = 'OutputFiles/OutputData.csv')
save.image(file = 'OutputFiles/OutWorkspace.RData')


# ########################### Run Calibration

# print('************RUN GLOBAL CALIBRATION***********')
# # OptSolver = 'Newton'
# OptSolver = 'BFGS'
# # OptSolver = 'CG'
# # OptSolver = 'Pseudo'
# # OptSolver = 'Marq'
# source('./Calibration/GlobalCalibration.R')
# print('COMPLETED.')

# ########################### Simulated Fitted Parameters

# print('************* SIMULATE FITTED MODEL**********')
# ptm = proc.time()
# FitOutput = WorldMod(t0,tf,delta_t,delayyearlength,InitValue,FittedParameters)
# ptm = proc.time() - ptm
# print('COMPLETED.')
# FitOutput = data.frame(FitOutput)
# PlotFuncWithObs(FitOutput)
# print(ptm)


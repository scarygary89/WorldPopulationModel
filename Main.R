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

########################### Load Results from Local Calibration

# LOAD R IMAGE OBJECTS

print('*****LOAD SUBMODEL PARAMEATER ESTIMATION*****')
print('------------- LOAD ECONOMY SUBMODEL VARIABLES')
load('./Calibration/CalibrationOutput/EconomyParmsEstimate.RData')

print('------------- LOAD RESOURCE SUBMODEL VARIABLES')
load('./Calibration/CalibrationOutput/ResourceParmsEstimate.RData')

print('------------- LOAD FOOD SUBMODEL VARIABLES')
load('./Calibration/CalibrationOutput/FoodParmsEstimate.RData')

print('------------- LOAD CLIMATE SUBMODEL VARIABLES')
load('./Calibration/CalibrationOutput/ClimateParmsEstimate.RData')

print('------------- LOAD POPULATION SUBMODEL VARIABLES')
load('./Calibration/CalibrationOutput/PopulationParmsEstimate.RData')

print('------------- LOAD HEALTH & VARIABLES EDUCATION SUBMODEL')
load('./Calibration/CalibrationOutput/HealthEducationParmsEstimate.RData')

print('------------- LOAD WATER SUBMODEL VARIABLES')
load('./Calibration/CalibrationOutput/WaterParmsEstimate.RData')
print('ALL COMPONENTS LOADED.')

# COMBINE PARAMETER ESTIMATION FROM ALL SUBMODELS

LocalFitParmameterValue = ParameterValue
LocalFitInitValue = InitValue

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


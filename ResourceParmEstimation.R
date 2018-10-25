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
source('Plotter.R')

# LOAD SUBMODELS
print('------------- LOAD RESOURCE SUBMODEL')
source('./Submodels/Resource.R')


# DEFINE MODEL AND SET TIME STEPS
print('*******LOAD MAIN MODEL PARAMETERS**********')
t0 = 1980
tf = 2080
delta_t = 1
delayyearlength = 1
print('COMPLETED.')

########################### Run Local Calibration

print('******* RUN PARAMETER ESTIMATION **********')
print('------------- CALIBRATE RESOURCE SUBMODEL')
source('./Calibration/ResourceCalibration.R')

print('***********  SAVE RESULTS *****************')
save(list = c(
	'ResourceResults',
	'ResourceParStart',
	'ResourceFitData'),
	file = 'Calibration/CalibrationOutput/ResourceParmsEstimate.RData')

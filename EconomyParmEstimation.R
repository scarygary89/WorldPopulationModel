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
print('------------- LOAD ECONOMY SUBMODEL')
source('./Submodels/Economy.R')


# DEFINE MODEL AND SET TIME STEPS
print('*******LOAD MAIN MODEL PARAMETERS**********')
t0 = 1980
tf = 2080
delta_t = 1
delayyearlength = 1
print('COMPLETED.')

########################### Run Local Calibration

print('******* RUN PARAMETER ESTIMATION **********')
print('------------- CALIBRATE ECONOMY SUBMODEL')
source('./Calibration/EconomyCalibration.R')

print('***********  SAVE RESULTS *****************')
save(list = c(
	'EconResults_Low',
	'EconResults_Mid',
	'EconResults_High',
	'EconParStart_Low',
	'EconParStart_Mid',
	'EconParStart_High',
	'EconFitData_Low',
	'EconFitData_Mid',
	'EconFitData_High'),
	file = 'Calibration/CalibrationOutput/EconomyParmsEstimate.RData')

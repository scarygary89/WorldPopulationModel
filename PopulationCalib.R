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

print('------------- LOAD POPULATION SUBMODEL')
source('Population.R')



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
print('************* DEFINE MAIN MODEL *************')

t0 = 1980
tf = 2080
delta_t = 1
delayyearlength = 1

WorldMod = function(t0, tf, delta_t, delayyearlength, init, parms) {	
	with(as.list(c(parms)), {	
		tspan = seq(from=t0, to=tf, by=delta_t)
		aux_names = c(
			'LowPop',
			'MidPop',
			'HighPop',
			'FemaleHealthAccess_RichLow',
			'FemaleHealthAccess_RichMid',
			'FemaleHealthAccess_RichHigh',
			'FemaleHealthAccess_PoorLow',
			'FemaleHealthAccess_PoorMid',
			'FemaleHealthAccess_PoorHigh',
			'GeneralHealthAccess_RichLow',
			'GeneralHealthAccess_RichMid',
			'GeneralHealthAccess_RichHigh',
			'GeneralHealthAccess_PoorLow',
			'GeneralHealthAccess_PoorMid',
			'GeneralHealthAccess_PoorHigh',
			'DeathRateM1_Low',
			'DeathRateM2_Low',
			'DeathRateM3_Low',
			'DeathRateM4_Low',
			'DeathRateF1_Low',
			'DeathRateF2_Low',
			'DeathRateF3_Low',
			'DeathRateF4_Low',
			'DeathRateM1_Mid',
			'DeathRateM2_Mid',
			'DeathRateM3_Mid',
			'DeathRateM4_Mid',
			'DeathRateF1_Mid',
			'DeathRateF2_Mid',
			'DeathRateF3_Mid',
			'DeathRateF4_Mid',
			'DeathRateM1_High',
			'DeathRateM2_High',
			'DeathRateM3_High',
			'DeathRateM4_High',
			'DeathRateF1_High',
			'DeathRateF2_High',
			'DeathRateF3_High',
			'DeathRateF4_High',
			'TFR_Low',
			'TFR_Mid',
			'TFR_High',
			'GFR_Low',
			'GFR_Mid',
			'GFR_High'
			)
		AuxData = matrix(NA,
			nrow = (length(tspan)),
			ncol = length(aux_names)
			)		
		colnames(AuxData) = aux_names
		StockData = matrix(NA,
			nrow = (length(tspan) + 1),
			ncol = length(init)
			)
		colnames(StockData) = names(init)	
		stocks = init
		StockData[1,] = stocks
		for(i in 1:length(tspan)) {
  		# Assemble Lists and Vectors
			RegPop_ijkr = list( 
				Low = c(
					RM1 = as.numeric(stocks['Low_RM1']),
					RM2 = as.numeric(stocks['Low_RM2']),
					RM3 = as.numeric(stocks['Low_RM3']),
					RM4 = as.numeric(stocks['Low_RM4']),
					RF1 = as.numeric(stocks['Low_RF1']),
					RF2 = as.numeric(stocks['Low_RF2']),
					RF3 = as.numeric(stocks['Low_RF3']),
					RF4 = as.numeric(stocks['Low_RF4']), 
					PM1 = as.numeric(stocks['Low_PM1']),
					PM2 = as.numeric(stocks['Low_PM2']),
					PM3 = as.numeric(stocks['Low_PM3']),
					PM4 = as.numeric(stocks['Low_PM4']),
					PF1 = as.numeric(stocks['Low_PF1']),
					PF2 = as.numeric(stocks['Low_PF2']),
					PF3 = as.numeric(stocks['Low_PF3']),
					PF4 = as.numeric(stocks['Low_PF4'])),
				Mid = c(
					RM1 = as.numeric(stocks['Mid_RM1']),
					RM2 = as.numeric(stocks['Mid_RM2']),
					RM3 = as.numeric(stocks['Mid_RM3']),
					RM4 = as.numeric(stocks['Mid_RM4']),
					RF1 = as.numeric(stocks['Mid_RF1']),
					RF2 = as.numeric(stocks['Mid_RF2']),
					RF3 = as.numeric(stocks['Mid_RF3']),
					RF4 = as.numeric(stocks['Mid_RF4']), 
					PM1 = as.numeric(stocks['Mid_PM1']),
					PM2 = as.numeric(stocks['Mid_PM2']),
					PM3 = as.numeric(stocks['Mid_PM3']),
					PM4 = as.numeric(stocks['Mid_PM4']),
					PF1 = as.numeric(stocks['Mid_PF1']),
					PF2 = as.numeric(stocks['Mid_PF2']),
					PF3 = as.numeric(stocks['Mid_PF3']),
					PF4 = as.numeric(stocks['Mid_PF4'])),
				High = c(
					RM1 = as.numeric(stocks['High_RM1']),
					RM2 = as.numeric(stocks['High_RM2']),
					RM3 = as.numeric(stocks['High_RM3']),
					RM4 = as.numeric(stocks['High_RM4']),
					RF1 = as.numeric(stocks['High_RF1']),
					RF2 = as.numeric(stocks['High_RF2']),
					RF3 = as.numeric(stocks['High_RF3']),
					RF4 = as.numeric(stocks['High_RF4']), 
					PM1 = as.numeric(stocks['High_PM1']),
					PM2 = as.numeric(stocks['High_PM2']),
					PM3 = as.numeric(stocks['High_PM3']),
					PM4 = as.numeric(stocks['High_PM4']),
					PF1 = as.numeric(stocks['High_PF1']),
					PF2 = as.numeric(stocks['High_PF2']),
					PF3 = as.numeric(stocks['High_PF3']),
					PF4 = as.numeric(stocks['High_PF4']))
			)


			ChiEF1_kr = list(
				Low = c(Rich = ChiEF1_RichLow, Poor = ChiEF1_PoorLow),
				Mid = c(Rich = ChiEF1_RichMid, Poor = ChiEF1_PoorMid),
				High = c(Rich = ChiEF1_RichHigh, Poor = ChiEF1_PoorHigh))
			ChiEF2_kr = list(
				Low = c(Rich = ChiEF2_RichLow, Poor = ChiEF2_PoorLow),
				Mid = c(Rich = ChiEF2_RichMid, Poor = ChiEF2_PoorMid),
				High = c(Rich = ChiEF2_RichHigh, Poor = ChiEF2_PoorHigh))
			ChiEF3_kr = list(
				Low = c(Rich = ChiEF3_RichLow, Poor = ChiEF3_PoorLow),
				Mid = c(Rich = ChiEF3_RichMid, Poor = ChiEF3_PoorMid),
				High = c(Rich = ChiEF3_RichHigh, Poor = ChiEF3_PoorHigh))
			ChiHF1_kr = list(
				Low = c(Rich = ChiHF1_RichLow, Poor = ChiHF1_PoorLow),
				Mid = c(Rich = ChiHF1_RichMid, Poor = ChiHF1_PoorMid),
				High = c(Rich = ChiHF1_RichHigh, Poor = ChiHF1_PoorHigh))
			ChiHF2_kr = list(
				Low = c(Rich = ChiHF2_RichLow, Poor = ChiHF2_PoorLow),
				Mid = c(Rich = ChiHF2_RichMid, Poor = ChiHF2_PoorMid),
				High = c(Rich = ChiHF2_RichHigh, Poor = ChiHF2_PoorHigh))
			ChiHF3_kr = list(
				Low = c(Rich = ChiHF3_RichLow, Poor = ChiHF3_PoorLow),
				Mid = c(Rich = ChiHF3_RichMid, Poor = ChiHF3_PoorMid),
				High = c(Rich = ChiHF3_RichHigh, Poor = ChiHF3_PoorHigh))
			ChiHA1_kr = list(
				Low = c(Rich = ChiHA1_RichLow, Poor = ChiHA1_PoorLow),
				Mid = c(Rich = ChiHA1_RichMid, Poor = ChiHA1_PoorMid),
				High = c(Rich = ChiHA1_RichHigh, Poor = ChiHA1_PoorHigh))
			ChiHA2_kr = list(
				Low = c(Rich = ChiHA2_RichLow, Poor = ChiHA2_PoorLow),
				Mid = c(Rich = ChiHA2_RichMid, Poor = ChiHA2_PoorMid),
				High = c(Rich = ChiHA2_RichHigh, Poor = ChiHA2_PoorHigh))
			ChiHA3_kr = list(
				Low = c(Rich = ChiHA3_RichLow, Poor = ChiHA3_PoorLow),
				Mid = c(Rich = ChiHA3_RichMid, Poor = ChiHA3_PoorMid),
				High = c(Rich = ChiHA3_RichHigh, Poor = ChiHA3_PoorHigh))
			RegPop_r = c(
				Low = sum(RegPop_ijkr[['Low']]),
				Mid = sum(RegPop_ijkr[['Mid']]),
				High = sum(RegPop_ijkr[['High']]))

	# Combine Submodels

			# Regional Health and Education System

			TotalFemale_kr = list(
				Low = c(
					Rich = sum(RegPop_ijkr[['Low']][c('RF1','RF2','RF3','RF4')]),
					Poor = sum(RegPop_ijkr[['Low']][c('PF1','PF2','PF3','PF4')])),
				Mid = c(					
					Rich = sum(RegPop_ijkr[['Mid']][c('RF1','RF2','RF3','RF4')]),
					Poor = sum(RegPop_ijkr[['Mid']][c('PF1','PF2','PF3','PF4')])),
				High = c(
					Rich = sum(RegPop_ijkr[['High']][c('RF1','RF2','RF3','RF4')]),
					Poor = sum(RegPop_ijkr[['High']][c('PF1','PF2','PF3','PF4')]))
			)
			TotalPop_kr = list(
				Low = c(
					Rich = sum(RegPop_ijkr[['Low']][c('RM1','RM2','RM3','RM4','RF1','RF2','RF3','RF4')]),
					Poor = sum(RegPop_ijkr[['Low']][c('PM1','PM2','PM3','PM4','PF1','PF2','PF3','PF4')])),
				Mid = c(
					Rich = sum(RegPop_ijkr[['Mid']][c('RM1','RM2','RM3','RM4','RF1','RF2','RF3','RF4')]),
					Poor = sum(RegPop_ijkr[['Mid']][c('PM1','PM2','PM3','PM4','PF1','PF2','PF3','PF4')])),
				High = c(
					Rich = sum(RegPop_ijkr[['High']][c('RM1','RM2','RM3','RM4','RF1','RF2','RF3','RF4')]),
					Poor = sum(RegPop_ijkr[['High']][c('PM1','PM2','PM3','PM4','PF1','PF2','PF3','PF4')]))
			)


			HealthEduOut_Low[['dEducationServices']] = 0
			HealthEduOut_Low[['dHealthServices']] = 0
			HealthEduOut_Mid[['dEducationServices']] = 0
			HealthEduOut_Mid[['dHealthServices']] = 0
			HealthEduOut_High[['dEducationServices']] = 0
			HealthEduOut_High[['dHealthServices']] = 0


			# Regional Population System 
			PopOut_Low      = Population(
								RegPop_ijkr[['Low']],
								HealthEduOut_Low[['FemaleEduAttain_k']],
								HealthEduOut_Low[['FemaleHealthAccess_k']],
								HealthEduOut_Low[['GeneralHealthAccess_k']],
								FoodOut[['NutritionConsPC_kr']][,'Low'],
								parms)

			PopOut_Mid      = Population(
								RegPop_ijkr[['Mid']],
								HealthEduOut_Mid[['FemaleEduAttain_k']],
								HealthEduOut_Mid[['FemaleHealthAccess_k']],
								HealthEduOut_Mid[['GeneralHealthAccess_k']],
								FoodOut[['NutritionConsPC_kr']][,'Mid'],
								parms)

			PopOut_High      = Population(
								RegPop_ijkr[['High']],
								HealthEduOut_High[['FemaleEduAttain_k']],
								HealthEduOut_High[['FemaleHealthAccess_k']],
								HealthEduOut_High[['GeneralHealthAccess_k']],
								FoodOut[['NutritionConsPC_kr']][,'High'],
								parms)



			################ STORE OUTPUT

			# AUXILIARY VARIABLES
			aux = c(
				RegPop_r['Low'],
				RegPop_r['Mid'],
				RegPop_r['High'],
				PopOut_Low[['MortRate_ijk']][,'Rich'],
				PopOut_Mid[['MortRate_ijk']][,'Rich'],
				PopOut_High[['MortRate_ijk']][,'Rich'],
				PopOut_Low[['TFR_k']]['Rich'],
				PopOut_Mid[['TFR_k']]['Rich'],
				PopOut_High[['TFR_k']]['Rich'],
				PopOut_Low[['GFR_k']]['Rich'],
				PopOut_Mid[['GFR_k']]['Rich'],
				PopOut_High[['GFR_k']]['Rich']
			)

			AuxData[i,] = aux

			# STOCK VARIABLES
			dstocks = c(

				# Population Stocks (Regions)        
				PopOut_Low[["dPop_ijk"]],
				PopOut_Mid[["dPop_ijk"]],
				PopOut_High[["dPop_ijk"]],
			) 
			stocks = stocks + dstocks * delta_t
			StockData[i+1,] = stocks
		}
	Output = cbind(tspan, StockData[-length(tspan),],AuxData)
	colnames(Output)[1] = 'time'
	return(Output)
	})
}

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

# print('*************  RUN CALIBRATION  *************')
# # OptSolver = 'Newton'
# # OptSolver = 'BFGS'
# # OptSolver = 'CG'
# OptSolver = 'Pseudo'
# source('ModelCalibration.R')
# print('COMPLETED.')

########################### Simulated Fitted Parameters

# print('************* SIMULATE FITTED MODEL**********')
# ptm = proc.time()
# FitOutput = WorldMod(t0,tf,delta_t,delayyearlength,InitValue,FittedParameters)
# ptm = proc.time() - ptm
# print('COMPLETED.')
# FitOutput = data.frame(FitOutput)
# PlotFuncWithObs(FitOutput)
# print(ptm)

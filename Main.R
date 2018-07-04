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
print('************* DEFINE MAIN MODEL *************')

t0 = 1980
tf = 2080
delta_t = 1
delayyearlength = 1

WorldMod = function(t0, tf, delta_t, delayyearlength, init, parms) {
	# write.csv(unlist(parms),file='CurrentParms.csv')	
	with(as.list(c(parms)), {	
		tspan = seq(from=t0, to=tf, by=delta_t)
		aux_names = c(
			'EconOutput_Low',
			'EconOutput_Mid',
			'EconOutput_High',
			'LowPop',
			'MidPop',
			'HighPop',
			'EconOutputPC_Low',
			'EconOutputPC_Mid',
			'EconOutputPC_High',
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

			EmployedWorkRatio_ijkr = list(
				Low = c(
					RM1 = LowEmployedWorkRatio_RM1,
					RM2 = LowEmployedWorkRatio_RM2,
					RM3 = LowEmployedWorkRatio_RM3,
					RM4 = LowEmployedWorkRatio_RM4,
					RF1 = LowEmployedWorkRatio_RF1,
					RF2 = LowEmployedWorkRatio_RF2,
					RF3 = LowEmployedWorkRatio_RF3,
					RF4 = LowEmployedWorkRatio_RF4,
					PM1 = LowEmployedWorkRatio_PM1,
					PM2 = LowEmployedWorkRatio_PM2,
					PM3 = LowEmployedWorkRatio_PM3,
					PM4 = LowEmployedWorkRatio_PM4,
					PF1 = LowEmployedWorkRatio_PF1,
					PF2 = LowEmployedWorkRatio_PF2,
					PF3 = LowEmployedWorkRatio_PF3,
					PF4 = LowEmployedWorkRatio_PF4),
				Mid = c(
					RM1 = MidEmployedWorkRatio_RM1,
					RM2 = MidEmployedWorkRatio_RM2,
					RM3 = MidEmployedWorkRatio_RM3,
					RM4 = MidEmployedWorkRatio_RM4,
					RF1 = MidEmployedWorkRatio_RF1,
					RF2 = MidEmployedWorkRatio_RF2,
					RF3 = MidEmployedWorkRatio_RF3,
					RF4 = MidEmployedWorkRatio_RF4,
					PM1 = MidEmployedWorkRatio_PM1,
					PM2 = MidEmployedWorkRatio_PM2,
					PM3 = MidEmployedWorkRatio_PM3,
					PM4 = MidEmployedWorkRatio_PM4,
					PF1 = MidEmployedWorkRatio_PF1,
					PF2 = MidEmployedWorkRatio_PF2,
					PF3 = MidEmployedWorkRatio_PF3,
					PF4 = MidEmployedWorkRatio_PF4),
				High = c(
					RM1 = HighEmployedWorkRatio_RM1,
					RM2 = HighEmployedWorkRatio_RM2,
					RM3 = HighEmployedWorkRatio_RM3,
					RM4 = HighEmployedWorkRatio_RM4,
					RF1 = HighEmployedWorkRatio_RF1,
					RF2 = HighEmployedWorkRatio_RF2,
					RF3 = HighEmployedWorkRatio_RF3,
					RF4 = HighEmployedWorkRatio_RF4,
					PM1 = HighEmployedWorkRatio_PM1,
					PM2 = HighEmployedWorkRatio_PM2,
					PM3 = HighEmployedWorkRatio_PM3,
					PM4 = HighEmployedWorkRatio_PM4,
					PF1 = HighEmployedWorkRatio_PF1,
					PF2 = HighEmployedWorkRatio_PF2,
					PF3 = HighEmployedWorkRatio_PF3,
					PF4 = HighEmployedWorkRatio_PF4)
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
			FoodStock_l = c(
				Fishstock = as.numeric(stocks['Fishstock']),
				Livestock = as.numeric(stocks['Livestock']),
				Crops = as.numeric(stocks['Crops']))
			FoodDemandPC_r = c( 
				Low = as.numeric(stocks['FoodDemandPC_Low']),
				Mid = as.numeric(stocks['FoodDemandPC_Mid']),
				High = as.numeric(stocks['FoodDemandPC_High']))
			RegPop_r = c(
				Low = sum(RegPop_ijkr[['Low']]),
				Mid = sum(RegPop_ijkr[['Mid']]),
				High = sum(RegPop_ijkr[['High']]))

	# Combine Submodels

			# Regional Economies
			EconOut_Low    	= Economy(
								stocks['RenewableResources'],
								stocks['NonrenewableResources'],
								RegPop_r['Low'],
								stocks['Capital_Low'],
								RenewableAccess_Low,
								NonrenewableAccess_Low,
								TechMult_Low,
								LaborInputElast_Low,
								CapitalInputElast_Low,
								RenewableCapitalReturn_Low,
								NonrenewableCapitalReturn_Low,
								IneqMult_Low,
								SavingsRate_Low,
								DeprecRate_Low,
								EmployedWorkRatio_ijkr[['Low']],
								RegPop_ijkr[['Low']],
								parms)  

			EconOut_Mid    	= Economy(
								stocks['RenewableResources'],
								stocks['NonrenewableResources'],
								RegPop_r['Mid'],
								stocks['Capital_Mid'],
								RenewableAccess_Mid,
								NonrenewableAccess_Mid,
								TechMult_Mid,
								LaborInputElast_Mid,
								CapitalInputElast_Mid,
								RenewableCapitalReturn_Mid,
								NonrenewableCapitalReturn_Mid,
								IneqMult_Mid,
								SavingsRate_Mid,
								DeprecRate_Mid,
								EmployedWorkRatio_ijkr[['Mid']],
								RegPop_ijkr[['Mid']],
								parms)
			
			EconOut_High   	= Economy(
								stocks['RenewableResources'],
								stocks['NonrenewableResources'],
								RegPop_r['High'],
								stocks['Capital_High'],
								RenewableAccess_High,
								NonrenewableAccess_High,
								TechMult_High,
								LaborInputElast_High,
								CapitalInputElast_High,
								RenewableCapitalReturn_High,
								NonrenewableCapitalReturn_High,
								IneqMult_High,
								SavingsRate_High,
								DeprecRate_High,
								EmployedWorkRatio_ijkr[['High']],
								RegPop_ijkr[['High']],
								parms)  

			EconOutput_r = 	 c( Low = EconOut_Low[['EconOutput']],
								Mid = EconOut_Mid[['EconOutput']],
								High = EconOut_High[['EconOutput']])

			EconOutputPC_r = c( Low = EconOut_Low[['EconOutputPC']], 
								Mid = EconOut_Mid[['EconOutputPC']], 
								High = EconOut_High[['EconOutputPC']])
	# Extract Delayed Values
			if (i < (1 + delayyearlength / delta_t)) {
				PrevFoodDemandPC_Low = stocks['FoodDemandPC_Low'] 
				PrevFoodDemandPC_Mid = stocks['FoodDemandPC_Mid']
				PrevFoodDemandPC_High = stocks['FoodDemandPC_High']
				PrevEconOutput_Low = (1 - InitEconOutputGrowth_Low) * EconOut_Low[['EconOutput']]
				PrevEconOutput_Mid = (1 - InitEconOutputGrowth_Mid) * EconOut_Mid[['EconOutput']]
				PrevEconOutput_High = (1 - InitEconOutputGrowth_Low) * EconOut_High[['EconOutput']]
				PrevEconOutputPC_Low = PrevEconOutput_Low / RegPop_r['Low']
				PrevEconOutputPC_Mid = PrevEconOutput_Mid / RegPop_r['Mid']
				PrevEconOutputPC_High = PrevEconOutput_High / RegPop_r['High']
			}

			if (i >= (1 + delayyearlength / delta_t)) { 
				PrevFoodDemandPC_Low = StockData[i - delayyearlength / delta_t,"FoodDemandPC_Low"]
				PrevFoodDemandPC_Mid = StockData[i - delayyearlength / delta_t, "FoodDemandPC_Mid"]
				PrevFoodDemandPC_High = StockData[i - delayyearlength / delta_t, "FoodDemandPC_High"]
				PrevEconOutput_Low = AuxData[i - delayyearlength / delta_t, "EconOutput_Low"]
				PrevEconOutput_Mid = AuxData[i - delayyearlength / delta_t, "EconOutput_Mid"]
				PrevEconOutput_High = AuxData[i - delayyearlength / delta_t, "EconOutput_High"]
				PrevEconOutputPC_Low =  PrevEconOutput_Low / 
									(AuxData[i - delayyearlength / delta_t, "LowPop"]*1000)
				PrevEconOutputPC_Mid =  PrevEconOutput_Mid / 
									(AuxData[i - delayyearlength / delta_t, "MidPop"]*1000)
				PrevEconOutputPC_High =  PrevEconOutput_High / 
									(AuxData[i - delayyearlength / delta_t, "HighPop"]*1000)
			} 
			PrevFoodDemandPC_r = c( 	
				Low =  PrevFoodDemandPC_Low, 
				Mid =  PrevFoodDemandPC_Mid, 
				High = PrevFoodDemandPC_High)
			PrevEconOutputPC_r = c(		
				Low = 	PrevEconOutputPC_Low, 
				Mid = 	PrevEconOutputPC_Mid, 
				High =	PrevEconOutputPC_High)
			PrevEconOutput_r = c(		
				Low = 	PrevEconOutput_Low, 
				Mid = 	PrevEconOutput_Mid, 
				High =	PrevEconOutput_High)			

			ChangeEconOutput_r = EconOutput_r - PrevEconOutput_r
			names(ChangeEconOutput_r) = c('Low','Mid','High')

			# Global Resources
			ResourceOut  	= Resource(
								stocks['RenewableResources'],
								stocks['NonrenewableResources'],
								EconOutput_r,
								parms)
			
			# Global Climate
			ClimateOut    	= Climate(stocks['GlobalTemp'],
								stocks['CO2Conc'],
								EconOutputPC_r,
								RegPop_r,
								parms)

			# Global Food System 
			FoodOut       	= Food(
								FoodStock_l,
								stocks['Fisheries'],
								FoodDemandPC_r,
								stocks['GrazeLand'],
								stocks['CropLand'],
								stocks['GlobalTemp'],
								RegPop_ijkr,
								RegPop_r,
								EconOutputPC_r,
								PrevEconOutputPC_r,
								PrevFoodDemandPC_r,
								stocks['Freshwater'],
								parms)

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
			HealthEduOut_Low = HealthEducation(
								stocks['HealthServices_Low'],
								stocks['EducationServices_Low'],
								ChangeEconOutput_r['Low'],
								TotalFemale_kr[['Low']],
								TotalPop_kr[['Low']],
								EducationInvestFrac_Low,
								HealthInvestFrac_Low,
								ZetaE_Low,
								ZetaH_Low,
								LambdaE_Low,
								LambdaH_Low,
								ChiEF1_kr[['Low']],
								ChiHF1_kr[['Low']],
								ChiHA1_kr[['Low']],
								ChiEF2_kr[['Low']],
								ChiHF2_kr[['Low']],
								ChiHA2_kr[['Low']],
								ChiEF3_kr[['Low']],
								ChiHF3_kr[['Low']],
								ChiHA3_kr[['Low']],
								EconOut_Low[['Inequality']],
								parms)
			HealthEduOut_Mid = HealthEducation(
								stocks['HealthServices_Mid'],
								stocks['EducationServices_Mid'],
								ChangeEconOutput_r['Mid'],
								TotalFemale_kr[['Mid']],
								TotalPop_kr[['Mid']],
								EducationInvestFrac_Mid,
								HealthInvestFrac_Mid,
								ZetaE_Mid,
								ZetaH_Mid,
								LambdaE_Mid,
								LambdaH_Mid,
								ChiEF1_kr[['Mid']],
								ChiHF1_kr[['Mid']],
								ChiHA1_kr[['Mid']],
								ChiEF2_kr[['Mid']],
								ChiHF2_kr[['Mid']],
								ChiHA2_kr[['Mid']],
								ChiEF3_kr[['Mid']],
								ChiHF3_kr[['Mid']],
								ChiHA3_kr[['Mid']],
								EconOut_Mid[['Inequality']],
								parms)
			HealthEduOut_High = HealthEducation(
								stocks['HealthServices_High'],
								stocks['EducationServices_High'],
								ChangeEconOutput_r['High'],
								TotalFemale_kr[['High']],
								TotalPop_kr[['High']],
								EducationInvestFrac_High,
								HealthInvestFrac_High,
				 				ZetaE_High,
								ZetaH_High,
								LambdaE_High,
								LambdaH_High,
								ChiEF1_kr[['High']],
								ChiHF1_kr[['High']],
								ChiHA1_kr[['High']],
								ChiEF2_kr[['High']],
								ChiHF2_kr[['High']],
								ChiHA2_kr[['High']],
								ChiEF3_kr[['High']],
								ChiHF3_kr[['High']],
								ChiHA3_kr[['High']],
								EconOut_High[['Inequality']],
								parms)

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

			# Global Water Supply

			WaterOut      	= Water(stocks['Freshwater'],stocks['GlobalTemp'],EconOutput_r,
								FoodOut[['AgriWaterDemand']],RegPop_r,parms)

			################ STORE OUTPUT

			# AUXILIARY VARIABLES
			aux = c(
				EconOut_Low[['EconOutput']],
				EconOut_Mid[['EconOutput']],
				EconOut_High[['EconOutput']],
				RegPop_r['Low'],
				RegPop_r['Mid'],
				RegPop_r['High'],
				EconOut_Low[['EconOutputPC']],
				EconOut_Mid[['EconOutputPC']],
				EconOut_High[['EconOutputPC']],
				HealthEduOut_Low[['FemaleHealthAccess_k']]['Rich'],
				HealthEduOut_Mid[['FemaleHealthAccess_k']]['Rich'],
				HealthEduOut_High[['FemaleHealthAccess_k']]['Rich'],
				HealthEduOut_Low[['FemaleHealthAccess_k']]['Poor'],
				HealthEduOut_Mid[['FemaleHealthAccess_k']]['Poor'],
				HealthEduOut_High[['FemaleHealthAccess_k']]['Poor'],
				HealthEduOut_Low[['GeneralHealthAccess_k']]['Rich'],
				HealthEduOut_Mid[['GeneralHealthAccess_k']]['Rich'],
				HealthEduOut_High[['GeneralHealthAccess_k']]['Rich'],
				HealthEduOut_Low[['GeneralHealthAccess_k']]['Poor'],
				HealthEduOut_Mid[['GeneralHealthAccess_k']]['Poor'],
				HealthEduOut_High[['GeneralHealthAccess_k']]['Poor'],
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
				# Economic Stocks (Regional)
				EconOut_Low[["dCapital"]],
				EconOut_Mid[["dCapital"]],
				EconOut_High[["dCapital"]],
				
				# Resource Stocks (Global)		
				ResourceOut[["dRenewableResources"]],
				ResourceOut[["dNonrenewableResources"]],
				 
				# Climate Stocks (Global)
				ClimateOut[["dCO2Conc"]],
				ClimateOut[["dGlobalTemp"]],

				# Food Stocks (Global)
				FoodOut[["dFisheries"]], 
				FoodOut[["dFoodStock_l"]],
				FoodOut[["dFoodDemandPC_r"]],
				FoodOut[["dGrazeLand"]], 
				FoodOut[["dCropLand"]],

				# Health and Education Stocks (Regions)
				HealthEduOut_Low[["dEducationServices"]],
				HealthEduOut_Mid[["dEducationServices"]],
				HealthEduOut_High[["dEducationServices"]],
				HealthEduOut_Low[["dHealthServices"]],
				HealthEduOut_Mid[["dHealthServices"]],
				HealthEduOut_High[["dHealthServices"]],

				# Population Stocks (Regions)        
				PopOut_Low[["dPop_ijk"]],
				PopOut_Mid[["dPop_ijk"]],
				PopOut_High[["dPop_ijk"]],

				# Water Stocks (Global)
				WaterOut[["dFreshwater"]]
			) 
			stocks = stocks + dstocks * delta_t
			stocks = pmax(stocks,1e-16)
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

# PlotFuncWithObs(OutputData)

########################### Run Calibration

print('*************  RUN CALIBRATION  *************')
# OptSolver = 'Newton'
# OptSolver = 'BFGS'
# OptSolver = 'CG'
OptSolver = 'Pseudo'
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

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

# PRINT FUNCTION
printvar = function(x){
	if(!is.na(x)){
		print(paste(deparse(substitute(x))," = ",x))
	}
}
		

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
			'EconOutput_Low',
			'EconOutput_Mid',
			'EconOutput_High',
			'LowPop',
			'MidPop',
			'HighPop')
		AuxData = matrix(NA,
			nrow = (length(tspan) - 1),
			ncol = (length(aux_names) + 1)
			)		
		colnames(AuxData) = c('time',aux_names)
		StockData = matrix(NA,
			nrow = length(tspan),
			ncol = (length(init) + 1)
			)
		colnames(StockData) = c('time',names(init))	
		stocks = c(init)
		StockData[1,] = c(tspan[1],stocks)
		for(i in 1:(length(tspan) - 1)) {
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
			ChiEF_kr = list(
				Low = c(Rich = ChiEF_RichLow, Poor = ChiEF_PoorLow),
				Mid = c(Rich = ChiEF_RichMid, Poor = ChiEF_PoorMid),
				High = c(Rich = ChiEF_RichHigh, Poor = ChiEF_PoorHigh))
			ChiHF_kr = list(
				Low = c(Rich = ChiHF_RichLow, Poor = ChiHF_PoorLow),
				Mid = c(Rich = ChiHF_RichMid, Poor = ChiHF_PoorMid),
				High = c(Rich = ChiHF_RichHigh, Poor = ChiHF_PoorHigh))
			ChiHA_kr = list(
				Low = c(Rich = ChiHA_RichLow, Poor = ChiHA_PoorLow),
				Mid = c(Rich = ChiHA_RichMid, Poor = ChiHA_PoorMid),
				High = c(Rich = ChiHA_RichHigh, Poor = ChiHA_PoorHigh))
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
								ChiEF_kr[['Low']],
								ChiHF_kr[['Low']],
								ChiHA_kr[['Low']],
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
								ChiEF_kr[['Mid']],
								ChiHF_kr[['Mid']],
								ChiHA_kr[['Mid']],
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
								ChiEF_kr[['High']],
								ChiHF_kr[['High']],
								ChiHA_kr[['High']],
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

			aux = c(
				EconOut_Low[['EconOutput']],
				EconOut_Mid[['EconOutput']],
				EconOut_High[['EconOutput']],
				RegPop_r['Low'],
				RegPop_r['Mid'],
				RegPop_r['High']
			)
			AuxData[i,] = c(tspan[i],aux)

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
			StockData[i+1,] = c(tspan[i+1],stocks)
		}
	Output = list(Stocks = StockData, Auxiliary = AuxData)
	return(Output)
	})
}

print('COMPLETED.')

########################### Simulate

print('*************  SIMULATE MODEL   *************')
ptm = proc.time()
SimResults = WorldMod(t0,tf,delta_t,delayyearlength,InitValue,ParameterValue)
ptm = proc.time() - ptm
StockData = data.frame(SimResults[['Stocks']])
AuxData = data.frame(SimResults[['Auxiliary']])
print('COMPLETED.')
print(ptm)
########################### Plot results

source('plotter.R')

########################### Run Calibration

print('*************  RUN CALIBRATION  *************')
# source('ModelCalibration.R')
print('COMPLETED.')

rm(list = ls())

library(deSolve)
library(ggplot2)
library(reshape)
library(gridExtra)
library(scales)
library(simecol)
library(Matrix)
library(FME)

print('************* INITIALIZE INPUTS *************')
setwd('~/../Dropbox/HumanPopDynModel/Model/R/WorldPopulationModel')
print('COMPLETED.')

# ASSEMBLE INPUT MATRICES AND VECTORS

source('InitializeData.R')

# LOAD SUBSYSTEMS
print('------------- LOAD ECONOMY SUBSYSTEM')
source('Economy.R')
print('COMPLETED.')

print('------------- LOAD RESOURCE SUBSYSTEM')
source('Resource.R')
print('COMPLETED.')

print('------------- LOAD FOOD SUBSYSTEM')
source('Food.R')
print('COMPLETED.')

print('------------- LOAD CLIMATE SUBSYSTEM')
source('Climate.R')
print('COMPLETED.')

print('------------- LOAD POPULATION SUBSYSTEM')
source('Population.R')
print('COMPLETED.')

print('------------- LOAD HEALTH & EDUCATION SUBSYSTEM')
source('HealthEducation.R')
print('COMPLETED.')

print('------------- LOAD WATER SUBSYSTEM')
source('Water.R')
print('COMPLETED.')

# DEFINE MODEL
print('************* DEFINE MAIN MODEL *************')

t0 = 1980
tf = 2080
tstep = 1
delaylength = 1

WorldMod = new("odeModel",
	main = function(time, init, parms){
		with(as.list(c(init, parms)), {

      # Assemble Lists and Vectors
			RegPop_ijkr = list( 
				Low = c(
					RM1 = Low_RM1,
					RM2 = Low_RM2,
					RM3 = Low_RM3,
					RM4 = Low_RM4,
					RF1 = Low_RF1,
					RF2 = Low_RF2,
					RF3 = Low_RF3,
					RF4 = Low_RF4, 
					PM1 = Low_PM1,
					PM2 = Low_PM2,
					PM3 = Low_PM3,
					PM4 = Low_PM4,
					PF1 = Low_PF1,
					PF2 = Low_PF2,
					PF3 = Low_PF3,
					PF4 = Low_PF4),
				Mid = c(
					RM1 = Mid_RM1,
					RM2 = Mid_RM2,
					RM3 = Mid_RM3,
					RM4 = Mid_RM4,
					RF1 = Mid_RF1,
					RF2 = Mid_RF2,
					RF3 = Mid_RF3,
					RF4 = Mid_RF4, 
					PM1 = Mid_PM1,
					PM2 = Mid_PM2,
					PM3 = Mid_PM3,
					PM4 = Mid_PM4,
					PF1 = Mid_PF1,
					PF2 = Mid_PF2,
					PF3 = Mid_PF3,
					PF4 = Mid_PF4),
				High = c(
					RM1 = High_RM1,
					RM2 = High_RM2,
					RM3 = High_RM3,
					RM4 = High_RM4,
					RF1 = High_RF1,
					RF2 = High_RF2,
					RF3 = High_RF3,
					RF4 = High_RF4, 
					PM1 = High_PM1,
					PM2 = High_PM2,
					PM3 = High_PM3,
					PM4 = High_PM4,
					PF1 = High_PF1,
					PF2 = High_PF2,
					PF3 = High_PF3,
					PF4 = High_PF4)
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
				Fishstock = Fishstock,
				Livestock = Livestock,
				Crops = Crops)
			FoodDemandPC_r = c( 
				Low = FoodDemandPC_Low,
				Mid = FoodDemandPC_Mid,
				High = FoodDemandPC_High)
			RegPop_r = c( 	
				Low = LowPop, 
				Mid = MidPop, 
				High = HighPop) 

    # Extract Delayed Values
			if (time <= (t0 + delaylength )){
				PrevFoodDemandPC_Low = InitValue['FoodDemandPC_Low']
				PrevFoodDemandPC_Mid = InitValue['FoodDemandPC_Mid']
				PrevFoodDemandPC_High = InitValue['FoodDemandPC_High']
				PrevEconOutput_Low = InitValue['EconOutput_Low']
				PrevEconOutput_Mid = InitValue['EconOutput_Mid']
				PrevEconOutput_High = InitValue['EconOutput_High']
				PrevEconOutputPC_Low = InitValue['EconOutput_Low'] / InitValue['LowPop']
				PrevEconOutputPC_Mid = InitValue['EconOutput_Mid'] / InitValue['MidPop']
				PrevEconOutputPC_High = InitValue['EconOutput_High'] / InitValue['HighPop']
			}
			if (time > (t0 + delaylength)) { 
				LagStock = lagvalue(time - delaylength)
				PrevFoodDemandPC_Low = LagStock[which(names(InitValue) == "FoodDemandPC_Low")]
				PrevFoodDemandPC_Mid = LagStock[which(names(InitValue) == "FoodDemandPC_Mid")]
				PrevFoodDemandPC_High = LagStock[which(names(InitValue) == "FoodDemandPC_High")]
				PrevEconOutput_Low = LagStock[which(names(InitValue) == "EconOutput_Low")]
				PrevEconOutput_Mid = LagStock[which(names(InitValue) == "EconOutput_Mid")]
				PrevEconOutput_High = LagStock[which(names(InitValue) == "EconOutput_High")]
				PrevEconOutputPC_Low =  LagStock[which(names(InitValue) == "EconOutput_Low")] / 
									(LagStock[which(names(InitValue) == "LowPop")]*1000)
				PrevEconOutputPC_Mid =  LagStock[which(names(InitValue) == "EconOutput_Mid")] / 
									(LagStock[which(names(InitValue) == "MidPop")]*1000)
				PrevEconOutputPC_High =  LagStock[which(names(InitValue) == "EconOutput_High")] / 
									(LagStock[which(names(InitValue) == "HighPop")]*1000)
			}

			PrevFoodDemandPC_r = c( 	
				Low =  PrevFoodDemandPC_Low, 
				Mid =  PrevFoodDemandPC_Mid, 
				High = PrevFoodDemandPC_High)
		
			PrevEconOutputPC_r = c(		
				Low = 	PrevEconOutputPC_Low, 
				Mid = 	PrevEconOutputPC_Mid, 
				High =	PrevEconOutputPC_High)


        	
    # Combine Submodels

			# Regional Economies
			EconOut_Low    	= Economy(RenewableResources,NonrenewableResources,
								EconOutput_Low,PrevEconOutput_Low,LowPop,Capital_Low,
								RenewableAccess_Low,NonrenewableAccess_Low,TechMult_Low,
								LaborInputElast_Low,CapitalInputElast_Low,
								RenewableInputElast_Low,NonrenewableInputElast_Low,IneqMult_Low,SavingsRate_Low,
								DeprecRate_Low,EmployedWorkRatio_ijkr[['Low']],RegPop_ijkr[['Low']],
								parms)  
			EconOut_Mid    	= Economy(RenewableResources,NonrenewableResources,
								EconOutput_Mid,PrevEconOutput_Mid,MidPop,Capital_Mid,
								RenewableAccess_Mid,NonrenewableAccess_Mid,TechMult_Mid,
								LaborInputElast_Mid,CapitalInputElast_Mid,
								RenewableInputElast_Mid,NonrenewableInputElast_Mid,IneqMult_Mid,SavingsRate_Mid,
								DeprecRate_Mid,EmployedWorkRatio_ijkr[['Mid']],RegPop_ijkr[['Mid']],
								parms)
			EconOut_High   	= Economy(RenewableResources,NonrenewableResources,
								EconOutput_High,PrevEconOutput_High,HighPop,Capital_High,
								RenewableAccess_High,NonrenewableAccess_High,TechMult_High,
								LaborInputElast_High,CapitalInputElast_High,
								RenewableInputElast_High,NonrenewableInputElast_High,IneqMult_High,SavingsRate_High,
								DeprecRate_High,EmployedWorkRatio_ijkr[['High']],RegPop_ijkr[['High']],
								parms)  
			
			EconOutput_r = 	 c( Low = EconOutput_Low,
								Mid = EconOutput_Mid,
								High = EconOutput_High)

			EconOutputPC_r = c( Low = EconOut_Low[['EconOutputPC']], 
								Mid = EconOut_Mid[['EconOutputPC']], 
								High = EconOut_High[['EconOutputPC']])

			# Global Resources
			ResourceOut  	= Resource(RenewableResources,NonrenewableResources,EconOutput_r,
								parms)
			
			# Global Climate

			ClimateOut    	= Climate(GlobalTemp,CO2Conc,EconOutputPC_r,RegPop_r,parms)

			# Global Food System 
			
			FoodOut       	= Food(FoodStock_l,Fisheries,FoodDemandPC_r,GrazeLand,CropLand,
							    GlobalTemp,RegPop_ijkr,RegPop_r,EconOutputPC_r,PrevEconOutputPC_r,
							    PrevFoodDemandPC_r,Freshwater,
							    parms)

			# Regional Health and Education System 

			HealthEduOut_Low  = HealthEducation(HealthServices_Low,EducationServices_Low,EconOutput_Low,
				EducationInvestFrac_Low, HealthInvestFrac_Low,ZetaE_Low,ZetaH_Low,LambdaE_Low,LambdaH_Low,
				ChiEF_kr[['Low']],ChiHF_kr[['Low']],ChiHA_kr[['Low']],
				RegPop_ijkr[['Low']],EconOut_Low[['Inequality']],parms)

			HealthEduOut_Mid  = HealthEducation(HealthServices_Mid,EducationServices_Mid,EconOutput_Mid,
				EducationInvestFrac_Mid, HealthInvestFrac_Mid,ZetaE_Mid,ZetaH_Mid,LambdaE_Mid,LambdaH_Mid,
				ChiEF_kr[['Mid']],ChiHF_kr[['Mid']],ChiHA_kr[['Mid']],
				RegPop_ijkr[['Mid']],EconOut_Mid[['Inequality']],parms)

			HealthEduOut_High  = HealthEducation(HealthServices_High,EducationServices_High,EconOutput_High,
				EducationInvestFrac_High, HealthInvestFrac_High,ZetaE_High,ZetaH_High,LambdaE_High,LambdaH_High,
				ChiEF_kr[['High']],ChiHF_kr[['High']],ChiHA_kr[['High']],
				RegPop_ijkr[['High']],EconOut_High[['Inequality']],parms)

			# Regional Population System 

			PopOut_Low      = Population(RegPop_ijkr[['Low']],RegPop[['Low']],HealthEduOut_Low[['FemaleEduAttain_k']],
								HealthEduOut_Low[['FemaleHealthAccess_k']],HealthEduOut_Low[['GeneralHealthAccess_k']],
								FoodOut[['NutritionConsPC_kr']][,'Low'],parms)

			PopOut_Mid      = Population(RegPop_ijkr[['Mid']],RegPop[['Mid']],HealthEduOut_Mid[['FemaleEduAttain_k']],
								HealthEduOut_Mid[['FemaleHealthAccess_k']],HealthEduOut_Mid[['GeneralHealthAccess_k']],
								FoodOut[['NutritionConsPC_kr']][,'Mid'],parms)

			PopOut_High      = Population(RegPop_ijkr[['High']],RegPop[['High']],HealthEduOut_High[['FemaleEduAttain_k']],
								HealthEduOut_High[['FemaleHealthAccess_k']],HealthEduOut_High[['GeneralHealthAccess_k']],
								FoodOut[['NutritionConsPC_kr']][,'High'],parms)

			# Global Water Supply

			WaterOut      	= Water(Freshwater,GlobalTemp,EconOutput_r,FoodOut[['AgriWaterDemand']],
								RegPop_r,parms)

			stocks = list(c(
				# Economic Stocks (Regional)
				EconOut_Low[["dCapital"]],
				EconOut_Mid[["dCapital"]],
				EconOut_High[["dCapital"]],
				EconOut_Low[["dEconOutput"]],
				EconOut_Mid[["dEconOutput"]],
				EconOut_High[["dEconOutput"]],
			  	
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
				PopOut_Low[["dTotPop"]],
				PopOut_Mid[["dTotPop"]],
				PopOut_High[["dTotPop"]],

				# Water Stocks (Global)
				WaterOut[["dFreshwater"]]
			))
			stocks
		})
	},

	init = InitValue,

	parms = ParameterValue,

	equations = list(Economy,Water,HealthEducation,Population,Food,Climate),

	times = c(from=t0, to=tf, by=tstep)

)

########################## Specify Solver

nlag = ceiling(delaylength/tstep)
solver(WorldMod) = function(y,times,func,parms,...) {
	lsode(y,times,func,parms,lags = nlag,verbose = T,...)
}
print('COMPLETED.')

########################### Simulate

print('*************  SIMULATE MODEL   *************')
SimResults = sim(WorldMod)
OutputData = data.frame(out(SimResults))
print('COMPLETED.')

########################### Plot results

source('plotter.R')

########################### Run Calibration

print('*************  RUN CALIBRATION  *************')
source('ModelCalibration.R')
print('COMPLETED.')

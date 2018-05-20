rm(list = ls())

library(deSolve)
library(ggplot2)
library(reshape)
library(gridExtra)
library(scales)
library(simecol)
library(Matrix)
library(FME)

setwd('~/../Dropbox/HumanPopDynModel/Model/R/WorldPopulationModel')

# ASSEMBLE INPUT MATRICES AND VECTORS

source('InitializeData.R')

# LOAD SUBSYSTEMS

source('Economy.R')
source('Food.R')
source('Climate.R')
source('Population.R')
source('HealthEducation.R')
source('Water.R')

# DEFINE MODEL

t0 = 1980
tf = 2080
tstep = 1
delaylength = 1

WorldMod = new("odeModel",
	main = function(time, init, parms){
		with(as.list(c(init, parms)), {

      # Assemble Matrices
			Pop_ijk = c(
				RM1 = RM1,
				RM2 = RM2,
				RM3 = RM3,
				RM4 = RM4,
				RF1 = RF1,
				RF2 = RF2,
				RF3 = RF3,
				RF4 = RF4, 
				PM1 = PM1,
				PM2 = PM2,
				PM3 = PM3,
				PM4 = PM4,
				PF1 = PF1,
				PF2 = PF2,
				PF3 = PF3,
				PF4 = PF4)

      # Extract Delayed Values
			if (time <= (t0 + delaylength )){
				PrevFoodDemandPC = InitData['FoodDemandPC']
				PrevEconOutput = InitData['EconOutput']
				PrevEconOutputPC = InitData['EconOutput'] / InitData['TotalPop']
			}
			if (time > (t0 + delaylength)) { 
				LagStock = lagvalue(time - delaylength)
				PrevFoodDemandPC = LagStock[which(names(InitData) == "FoodDemandPC")]
				PrevEconOutput = LagStock[which(names(InitData) == "EconOutput")]
				PrevEconOutputPC =  LagStock[which(names(InitData) == "EconOutput")] / 
									(LagStock[which(names(InitData) == "TotalPop")]*1000)
			} 
			# print(paste(time,": PrevFoodDemandPC", PrevFoodDemandPC))


			# Combine Submodels
			EconOut       = Economy(Capital,RenewableResources,NonrenewableResources,
								EconOutput,Pop_ijk,TotalPop,PrevEconOutput,
								parms)  

			ClimateOut    = Climate(GlobalTemp,CO2Conc,EconOutput,Livestock,parms)

			FoodOut       = Food(Crops,Livestock,Fishstock,Fisheries,FoodDemandPC,AgriculturalLand,
								GlobalTemp,Pop_ijk,TotalPop,EconOut[["EconOutputPC"]],
								PrevEconOutputPC,PrevFoodDemandPC,Freshwater,parms)
			HealthEduOut  = HealthEducation(HealthServices,EducationServices,EconOutput,
								Pop_ijk,EconOut[['Inequality']],parms)

			PopOut        = Population(Pop_ijk,TotalPop,HealthEduOut[['FemaleEduAttain_k']],
								HealthEduOut[['FemaleHealthAccess_k']],HealthEduOut[['GeneralHealthAccess_k']],
								FoodOut[['NutritionConsPC_k']],parms)

			WaterOut      = Water(Freshwater,GlobalTemp,EconOutput,FoodOut[['AgriWaterDemand']],
								TotalPop,parms)


			list(c(
				EconOut[["dCapital"]],
				EconOut[["dRenewableResources"]],
				EconOut[["dNonrenewableResources"]],
				EconOut[["dEconOutput"]],
				ClimateOut[["dCO2Conc"]],
				ClimateOut[["dGlobalTemp"]],
				FoodOut[["dFisheries"]], 
				FoodOut[["dFishstock"]],
				FoodOut[["dLivestock"]], 
				FoodOut[["dCrops"]], 
				FoodOut[["dFoodDemandPC"]],
				FoodOut[["dAgriculturalLand"]], 
				HealthEduOut[["dEducationServices"]],
				HealthEduOut[["dHealthServices"]],
				PopOut[["dPop_ijk"]],
				PopOut[["dTotPop"]],
				WaterOut[["dFreshwater"]]
			))
		})
	},

	init = InitData,

	parms = ParameterData,

	equations = list(Economy,Water,HealthEducation,Population,Food,Climate),

	times = c(from=t0, to=tf, by=tstep)

)

########################## Specify Solver

nlag = ceiling(delaylength/tstep)
solver(WorldMod) = function(y,times,func,parms,...) {
	lsode(y,times,func,parms,lags = nlag,verbose = T,...)
}

########################### Run Calibration

# source('ModelCalibration.R')

########################### Simulate

SimResults = sim(WorldMod)
OutputData = data.frame(out(SimResults))

########################### Plot results

source('plotter.R')

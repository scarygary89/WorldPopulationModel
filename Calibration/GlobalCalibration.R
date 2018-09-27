print('----------------- CALIBRATE MODEL   ---------------------')

library(foreach)
library(doParallel)
library(FME)
library(GA)
numcore = detectCores()
cl<-makeCluster(numcore) 
print(cl)

GlobalCost = function(p,t0,tf,delta_t,delayyearlength,init,parms,yactual)
{
	whichpar = names(parms)[names(parms) %in% names(p)]
	whichinit = names(init)[names(init) %in% names(p)]
	parms[whichpar] = p[whichpar]
	init[whichinit] = p[whichinit]	
	ysim = WorldMod(t0,tf,delta_t,delayyearlength,init,parms)
	RunCost = modCost(ysim,yactual,x = 'time',y = 'value',err = 'error',
		scaleVar=T)	
	return(RunCost)
}

################# DEFINE INITIAL PARAMETER VALUES TO BE FITTED

GlobalParms = LocalFitParmameterValue
GlobalInit = LocalFitInitValue

GlobalActual = na.omit(melt(
	data.frame(cbind(
		time = CalibData$time,

		# Economy Submodel
		logEconOutput_Low = log(CalibData$EconOutput_Low),
		Inequality_Low = CalibData$Inequality_Low,
		Capital_Low = CalibData$Capital_Low,
		logEconOutput_Mid = log(CalibData$EconOutput_Mid),
		Inequality_Mid = CalibData$Inequality_Mid,
		Capital_Mid = CalibData$Capital_Mid,
		logEconOutput_High = log(CalibData$EconOutput_High),
		Inequality_High = CalibData$Inequality_High,
		Capital_High = CalibData$Capital_High,

		# Resource Submodel
		CoalReserves = CalibData$CoalReserves,
        OilReserves = CalibData$OilReserves,
        GasReserves = CalibData$GasReserves,

		# Climate Submodel
        CO2Conc = CalibData$CO2Conc,
        TempAnamoly = CalibData$TempAnamoly,
        CO2Emission_Low = CalibData$CO2Emission_Low,
        CO2Emission_Mid = CalibData$CO2Emission_Mid,
        CO2Emission_High = CalibData$CO2Emission_High,

		# Food Submodel
        FishProduction = CalibData$GlobalFishProduction,
		LivestockProduction = CalibData$GlobalMeatProduction,
		CropProduction = CalibData$GlobalCropProduction,
		Fishstock = CalibData$Fishstock,
		Livestock = CalibData$Livestock,
		Crops = CalibData$Crops,
		LivestockWaste = CalibData$GlobalMeatLoss,
		CropWaste = CalibData$GlobalCropLoss,
		FishConsumption = CalibData$FishConsumption_Low + 
			CalibData$FishConsumption_Mid + CalibData$FishConsumption_High,
		LivestockConsumption = CalibData$MeatConsumption_Low + 
			CalibData$MeatConsumption_Mid + CalibData$MeatConsumption_High,
		CropConsumption = CalibData$CropConsumption_Low + 
			CalibData$CropConsumption_Mid + CalibData$CropConsumption_High,
		NutritionConsPC_RichLow = CalibData$NutritionConsumptionPC_Low,
		NutritionConsPC_PoorLow = CalibData$NutritionConsumptionPC_Low,
		NutritionConsPC_RichMid = CalibData$NutritionConsumptionPC_Mid,
		NutritionConsPC_PoorMid = CalibData$NutritionConsumptionPC_Mid,
		NutritionConsPC_RichHigh = CalibData$NutritionConsumptionPC_High,
		NutritionConsPC_PoorHigh = CalibData$NutritionConsumptionPC_High,
		Fisheries = CalibData$Fisheries,
		CropLand = CalibData$CropLand,
		GrazeLand = CalibData$GrazeLand,
		AgriWaterDemand = CalibData$SmoothAgriculturalWaterConsumption,

		# Popoulation Submodel
		Low_M1 = CalibData$Low_M1,
		Low_M3 = CalibData$Low_M3,
		Low_M2 = CalibData$Low_M2,
		Low_M4 = CalibData$Low_M4,
		Low_F1 = CalibData$Low_F1,
		Low_F2 = CalibData$Low_F2,
		Low_F3 = CalibData$Low_F3,
		Low_F4 = CalibData$Low_F4,
		Mid_M1 = CalibData$Mid_M1,
		Mid_M3 = CalibData$Mid_M3,
		Mid_M2 = CalibData$Mid_M2,
		Mid_M4 = CalibData$Mid_M4,
		Mid_F1 = CalibData$Mid_F1,
		Mid_F2 = CalibData$Mid_F2,
		Mid_F3 = CalibData$Mid_F3,
		Mid_F4 = CalibData$Mid_F4,
		High_M1 = CalibData$High_M1,
		High_M3 = CalibData$High_M3,
		High_M2 = CalibData$High_M2,
		High_M4 = CalibData$High_M4,
		High_F1 = CalibData$High_F1,
		High_F2 = CalibData$High_F2,
		High_F3 = CalibData$High_F3,
		High_F4 = CalibData$High_F4,					
		GFR_Low = CalibData$GFR_Low,
		GFR_Mid = CalibData$GFR_Mid,
		GFR_High = CalibData$GFR_High,

		# Health and Education Submodel
		FemaleHealthAccess_Low = CalibData$FemaleHealthAccess_Low,
		GeneralHealthAccess_RichLow = CalibData$GeneralHealthAccess_RichLow,
		GeneralHealthAccess_PoorLow = CalibData$GeneralHealthAccess_PoorLow,
		EducationServices_Low = CalibData$EducationServices_Low, 
		HealthServices_Low = CalibData$HealthServices_Low,
		FemaleHealthAccess_Mid = CalibData$FemaleHealthAccess_Mid,
		GeneralHealthAccess_RichMid = CalibData$GeneralHealthAccess_RichMid,
		GeneralHealthAccess_PoorMid = CalibData$GeneralHealthAccess_PoorMid,
		EducationServices_Mid = CalibData$EducationServices_Mid, 
		HealthServices_Mid = CalibData$HealthServices_Mid,
		FemaleHealthAccess_High = CalibData$FemaleHealthAccess_High,
		GeneralHealthAccess_RichHigh = CalibData$GeneralHealthAccess_RichHigh,
		GeneralHealthAccess_PoorHigh = CalibData$GeneralHealthAccess_PoorHigh,
		EducationServices_High = CalibData$EducationServices_High, 
		HealthServices_High = CalibData$HealthServices_High,

		# Water Submodel
        MunWaterDemand = CalibData$SmoothMunicipalWaterConsumption,
        IndWaterDemand = CalibData$SmoothIndustrialWaterConsumption,
        Freshwater = CalibData$Freshwater
	)),
	id = 'time'))


GlobalActual$variable = as.character(GlobalActual$variable)
GlobalActual = GlobalActual[,c('variable','time','value')]
# Economy Submodel
GlobalActual$error[GlobalActual$variable == 'logEconOutput_Low'] = 
	mean(GlobalActual$value[GlobalActual$variable == 'logEconOutput_Low']) #/ 
	# (GlobalActual$time[GlobalActual$variable == 'logEconOutput_Low'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'Inequality_Low'] = 
	mean(GlobalActual$value[GlobalActual$variable == 'Inequality_Low']) #/ 
	# (GlobalActual$time[GlobalActual$variable == 'Inequality_Low'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'Capital_Low'] =  
	mean(GlobalActual$value[GlobalActual$variable == 'Capital_Low']) #/ 
	# (GlobalActual$time[GlobalActual$variable == 'Capital_Low'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'logEconOutput_Mid'] = 
	mean(GlobalActual$value[GlobalActual$variable == 'logEconOutput_Mid']) #/ 
	# (GlobalActual$time[GlobalActual$variable == 'logEconOutput_Mid'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'Inequality_Mid'] = 
	mean(GlobalActual$value[GlobalActual$variable == 'Inequality_Mid']) #/ 
	# (GlobalActual$time[GlobalActual$variable == 'Inequality_Mid'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'Capital_Mid'] =  
	mean(GlobalActual$value[GlobalActual$variable == 'Capital_Mid']) #/ 
	# (GlobalActual$time[GlobalActual$variable == 'Capital_Mid'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'logEconOutput_High'] = 
	mean(GlobalActual$value[GlobalActual$variable == 'logEconOutput_High']) #/ 
	# (GlobalActual$time[GlobalActual$variable == 'logEconOutput_High'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'Inequality_High'] = 
	mean(GlobalActual$value[GlobalActual$variable == 'Inequality_High']) #/ 
	# (GlobalActual$time[GlobalActual$variable == 'Inequality_High'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'Capital_High'] =  
	mean(GlobalActual$value[GlobalActual$variable == 'Capital_High']) #/ 
	# (GlobalActual$time[GlobalActual$variable == 'Capital_High'] - 1979) ^ 2

# Resource Submodel
GlobalActual$error[GlobalActual$variable == 'CoalReserves'] = 
    mean(GlobalActual$value[GlobalActual$variable == 'CoalReserves']) #/ 
    # (GlobalActual$time[GlobalActual$variable == 'CoalReserves'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'OilReserves'] = 
    mean(GlobalActual$value[GlobalActual$variable == 'OilReserves']) #/ 
    # (GlobalActual$time[GlobalActual$variable == 'OilReserves'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'GasReserves'] = 
    mean(GlobalActual$value[GlobalActual$variable == 'GasReserves']) #/ 
    # (GlobalActual$time[GlobalActual$variable == 'GasReserves'] - 1979) ^ 2

# Climate Submodel
GlobalActual$error[GlobalActual$variable == 'CO2Conc'] = 
    mean(GlobalActual$value[GlobalActual$variable == 'CO2Conc']) #/ 
    # (GlobalActual$time[GlobalActual$variable == 'CO2Conc'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'TempAnamoly'] = 
    mean(GlobalActual$value[GlobalActual$variable == 'TempAnamoly']) #/ 
    # (GlobalActual$time[GlobalActual$variable == 'TempAnamoly'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'CO2EmissionPC_Low'] = 
    mean(GlobalActual$value[GlobalActual$variable == 'CO2EmissionPC_Low']) #/ 
    # (GlobalActual$time[GlobalActual$variable == 'CO2EmissionPC_Low'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'CO2EmissionPC_Mid'] = 
    mean(GlobalActual$value[GlobalActual$variable == 'CO2EmissionPC_Mid']) #/ 
    # (GlobalActual$time[GlobalActual$variable == 'CO2EmissionPC_Mid'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'CO2EmissionPC_High'] = 
    mean(GlobalActual$value[GlobalActual$variable == 'CO2EmissionPC_High']) #/ 
    # (GlobalActual$time[GlobalActual$variable == 'CO2EmissionPC_High'] - 1979) ^ 2

# Food Submodel
GlobalActual$error[GlobalActual$variable == 'FishProduction'] =
	mean(GlobalActual$value[GlobalActual$variable == 'FishProduction']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'FishProduction'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'LivestockProduction'] =
	mean(GlobalActual$value[GlobalActual$variable == 'LivestockProduction']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'LivestockProduction'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'CropProduction'] =
	mean(GlobalActual$value[GlobalActual$variable == 'CropProduction']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'CropProduction'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'LivestockWaste'] =
	mean(GlobalActual$value[GlobalActual$variable == 'LivestockWaste']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'LivestockWaste'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'CropWaste'] =
	mean(GlobalActual$value[GlobalActual$variable == 'CropWaste']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'CropWaste'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'FishConsumption'] =
	mean(GlobalActual$value[GlobalActual$variable == 'FishConsumption']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'FishConsumption'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'LivestockConsumption'] =
	mean(GlobalActual$value[GlobalActual$variable == 'LivestockConsumption']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'LivestockConsumption'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'CropConsumption'] =
	mean(GlobalActual$value[GlobalActual$variable == 'CropConsumption']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'CropConsumption'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'NutritionConsPC_RichLow'] =
	mean(GlobalActual$value[GlobalActual$variable == 'NutritionConsPC_RichLow']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'NutritionConsPC_RichLow'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'NutritionConsPC_PoorLow'] =
	mean(GlobalActual$value[GlobalActual$variable == 'NutritionConsPC_PoorLow']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'NutritionConsPC_PoorLow'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'NutritionConsPC_RichMid'] =
	mean(GlobalActual$value[GlobalActual$variable == 'NutritionConsPC_RichMid']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'NutritionConsPC_RichMid'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'NutritionConsPC_PoorMid'] =
	mean(GlobalActual$value[GlobalActual$variable == 'NutritionConsPC_PoorMid']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'NutritionConsPC_PoorMid'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'NutritionConsPC_RichHigh'] =
	mean(GlobalActual$value[GlobalActual$variable == 'NutritionConsPC_RichHigh']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'NutritionConsPC_RichHigh'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'NutritionConsPC_PoorHigh'] =
	mean(GlobalActual$value[GlobalActual$variable == 'NutritionConsPC_PoorHigh']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'NutritionConsPC_PoorHigh'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'CropLand'] =
	mean(GlobalActual$value[GlobalActual$variable == 'CropLand']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'CropLand'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'GrazeLand'] =
	mean(GlobalActual$value[GlobalActual$variable == 'GrazeLand']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'GrazeLand'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Fisheries'] =
	mean(GlobalActual$value[GlobalActual$variable == 'Fisheries']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Fisheries'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'AgriWaterDemand'] =
	mean(GlobalActual$value[GlobalActual$variable == 'AgriWaterDemand']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'AgriWaterDemand'] - 1979) ^ 2 / 1297)

# Health and Education Submodel
GlobalActual$error[GlobalActual$variable == 'FemaleHealthAccess'] =
	mean(GlobalActual$value[GlobalActual$variable == 'FemaleHealthAccess']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'FemaleHealthAccess'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'FemaleHealthAccess'] =
	mean(GlobalActual$value[GlobalActual$variable == 'FemaleHealthAccess']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'FemaleHealthAccess'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'FemaleHealthAccess'] =
	mean(GlobalActual$value[GlobalActual$variable == 'FemaleHealthAccess']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'FemaleHealthAccess'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'GeneralHealthAccess_Rich'] =
	mean(GlobalActual$value[GlobalActual$variable == 'GeneralHealthAccess_Rich']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'GeneralHealthAccess_Rich'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'GeneralHealthAccess_Rich'] =
	mean(GlobalActual$value[GlobalActual$variable == 'GeneralHealthAccess_Rich']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'GeneralHealthAccess_Rich'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'GeneralHealthAccess_Rich'] =
	mean(GlobalActual$value[GlobalActual$variable == 'GeneralHealthAccess_Rich']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'GeneralHealthAccess_Rich'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'GeneralHealthAccess_Poor'] =
	mean(GlobalActual$value[GlobalActual$variable == 'GeneralHealthAccess_Poor']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'GeneralHealthAccess_Poor'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'GeneralHealthAccess_Poor'] =
	mean(GlobalActual$value[GlobalActual$variable == 'GeneralHealthAccess_Poor']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'GeneralHealthAccess_Poor'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'GeneralHealthAccess_Poor'] =
	mean(GlobalActual$value[GlobalActual$variable == 'GeneralHealthAccess_Poor']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'GeneralHealthAccess_Poor'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'EducationServices'] =
	mean(GlobalActual$value[GlobalActual$variable == 'EducationServices']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'EducationServices'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'EducationServices'] =
	mean(GlobalActual$value[GlobalActual$variable == 'EducationServices']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'EducationServices'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'EducationServices'] =
	mean(GlobalActual$value[GlobalActual$variable == 'EducationServices']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'EducationServices'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'HealthServices'] =
	mean(GlobalActual$value[GlobalActual$variable == 'HealthServices']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'HealthServices'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'HealthServices'] =
	mean(GlobalActual$value[GlobalActual$variable == 'HealthServices']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'HealthServices'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'HealthServices'] =
	mean(GlobalActual$value[GlobalActual$variable == 'HealthServices']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'FemaleHealthAccess'] - 1979) ^ 2 / 1297)

# Popoulation Submodel
GlobalActual$error[GlobalActual$variable == 'Low_M1'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Low_M1']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Low_M1'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Low_M2'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Low_M2']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Low_M2'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Low_M3'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Low_M3']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Low_M3'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Low_M4'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Low_M4']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Low_M4'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Low_F1'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Low_F1']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Low_F1'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Low_F2'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Low_F2']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Low_F2'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Low_F3'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Low_F3']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Low_F3'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Low_F4'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Low_F4']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Low_F4'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'GFR'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'GFR']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'GFR'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Mid_M1'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Mid_M1']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Mid_M1'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Mid_M2'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Mid_M2']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Mid_M2'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Mid_M3'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Mid_M3']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Mid_M3'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Mid_M4'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Mid_M4']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Mid_M4'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Mid_F1'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Mid_F1']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Mid_F1'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Mid_F2'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Mid_F2']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Mid_F2'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Mid_F3'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Mid_F3']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Mid_F3'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'Mid_F4'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'Mid_F4']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'Mid_F4'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'GFR_Mid'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'GFR_Mid']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'GFR_Mid'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'High_M1'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'High_M1']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'High_M1'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'High_M2'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'High_M2']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'High_M2'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'High_M3'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'High_M3']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'High_M3'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'High_M4'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'High_M4']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'High_M4'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'High_F1'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'High_F1']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'High_F1'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'High_F2'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'High_F2']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'High_F2'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'High_F3'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'High_F3']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'High_F3'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'High_F4'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'High_F4']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'High_F4'] - 1979) ^ 2 / 1297)
GlobalActual$error[GlobalActual$variable == 'GFR_High'] = 1
	# mean(GlobalActual$value[GlobalActual$variable == 'GFR_High']) #* 
	# (1 - (GlobalActual$time[GlobalActual$variable == 'GFR_High'] - 1979) ^ 2 / 1297)

# Water Submodel
GlobalActual$error[GlobalActual$variable == 'MunGlobalDemand'] = 
    mean(GlobalActual$value[GlobalActual$variable == 'MunGlobalDemand']) #/ 
    # (GlobalActual$time[GlobalActual$variable == 'MunGlobalDemand'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'IndGlobalDemand'] = 
    mean(GlobalActual$value[GlobalActual$variable == 'IndGlobalDemand']) #/ 
    # (GlobalActual$time[GlobalActual$variable == 'IndGlobalDemand'] - 1979) ^ 2
GlobalActual$error[GlobalActual$variable == 'Freshwater'] = 
    mean(GlobalActual$value[GlobalActual$variable == 'Freshwater']) #/ 
    # (GlobalActual$time[GlobalActual$variable == 'Freshwater'] - 1979) ^ 2	)

CalibPars =  rbind(
	# Economy Submodel
	TechMult_Low = as.numeric(InitialData['TechMult_Low',]),
	TechGrowth_Low = as.numeric(ParameterData['TechGrowth_Low',]),
	SavingsRate_Low = as.numeric(ParameterData['SavingsRate_Low',]),
	DeprecRate_Low = as.numeric(ParameterData['DeprecRate_Low',]),
	CoalInputElast_Low = as.numeric(ParameterData['CoalInputElast_Low',]),
	OilInputElast_Low = as.numeric(ParameterData['OilInputElast_Low',]),
	GasInputElast_Low = as.numeric(ParameterData['GasInputElast_Low',]),
	CapitalInputElast_Low = as.numeric(ParameterData['CapitalInputElast_Low',]),
	LaborInputElast_Low = as.numeric(ParameterData['LaborInputElast_Low',]),
	IneqMult_Low = as.numeric(ParameterData['IneqMult_Low',]),
	IneqInt_Low = as.numeric(ParameterData['IneqInt_Low',]),
	TechMult_Mid = as.numeric(InitialData['TechMult_Mid',]),
	TechGrowth_Mid = as.numeric(ParameterData['TechGrowth_Mid',]),
	SavingsRate_Mid = as.numeric(ParameterData['SavingsRate_Mid',]),
	DeprecRate_Mid = as.numeric(ParameterData['DeprecRate_Mid',]),
	CoalInputElast_Mid = as.numeric(ParameterData['CoalInputElast_Mid',]),
	OilInputElast_Mid = as.numeric(ParameterData['OilInputElast_Mid',]),
	GasInputElast_Mid = as.numeric(ParameterData['GasInputElast_Mid',]),
	CapitalInputElast_Mid = as.numeric(ParameterData['CapitalInputElast_Mid',]),
	LaborInputElast_Mid = as.numeric(ParameterData['LaborInputElast_Mid',]),
	IneqMult_Mid = as.numeric(ParameterData['IneqMult_Mid',]),
	IneqInt_Mid = as.numeric(ParameterData['IneqInt_Mid',]),
	TechMult_High = as.numeric(InitialData['TechMult_High',]),
	TechGrowth_High = as.numeric(ParameterData['TechGrowth_High',]),
	SavingsRate_High = as.numeric(ParameterData['SavingsRate_High',]),
	DeprecRate_High = as.numeric(ParameterData['DeprecRate_High',]),
	CoalInputElast_High = as.numeric(ParameterData['CoalInputElast_High',]),
	OilInputElast_High = as.numeric(ParameterData['OilInputElast_High',]),
	GasInputElast_High = as.numeric(ParameterData['GasInputElast_High',]),
	CapitalInputElast_High = as.numeric(ParameterData['CapitalInputElast_High',]),
	LaborInputElast_High = as.numeric(ParameterData['LaborInputElast_High',]),
	IneqMult_High = as.numeric(ParameterData['IneqMult_High',]),
	IneqInt_High = as.numeric(ParameterData['IneqInt_High',]),

	# Resource Submodel
	CoalConsIntensity_Low = as.numeric(ParameterData['CoalConsIntensity_Low',]),
	CoalConsIntensity_Mid = as.numeric(ParameterData['CoalConsIntensity_Mid',]),
	CoalConsIntensity_High = as.numeric(ParameterData['CoalConsIntensity_High',]),
	OilConsIntensity_Low = as.numeric(ParameterData['OilConsIntensity_Low',]),
	OilConsIntensity_Mid = as.numeric(ParameterData['OilConsIntensity_Mid',]),
	OilConsIntensity_High = as.numeric(ParameterData['OilConsIntensity_High',]),
	GasConsIntensity_Low = as.numeric(ParameterData['GasConsIntensity_Low',]),
	GasConsIntensity_Mid = as.numeric(ParameterData['GasConsIntensity_Mid',]),
	GasConsIntensity_High = as.numeric(ParameterData['GasConsIntensity_High',]),

	# Climate Submodel
	Lambda = as.numeric(ParameterData['Lambda',]),
	# RefTemp = as.numeric(ParameterData['RefTemp',]),
	PsiE1_Low = as.numeric(ParameterData['PsiE1_Low',]),
	PsiE1_Mid = as.numeric(ParameterData['PsiE1_Mid',]),
	PsiE1_High = as.numeric(ParameterData['PsiE1_High',]),
	PsiE2_Low = as.numeric(ParameterData['PsiE2_Low',]),
	PsiE2_Mid = as.numeric(ParameterData['PsiE2_Mid',]),
	PsiE2_High = as.numeric(ParameterData['PsiE2_High',]),
	PsiE3_Low = as.numeric(ParameterData['PsiE3_Low',]),
	PsiE3_Mid = as.numeric(ParameterData['PsiE3_Mid',]),
	PsiE3_High = as.numeric(ParameterData['PsiE3_High',]),
	Gamma = as.numeric(ParameterData['Gamma',]),
	# RefCO2Conc = as.numeric(ParameterData['RefCO2Conc',]),
	OtherRadForce = as.numeric(ParameterData['OtherRadForce',]),

	# Food Submodel
	ThetaU = as.numeric(ParameterData['ThetaU',]),
	ZetaU = as.numeric(ParameterData['ZetaU',]),
	FishingTech = as.numeric(ParameterData['FishingTech',]),
	CropLandGrowthRate = as.numeric(ParameterData['CropLandGrowthRate',]),
	# CropLandLossRate = as.numeric(ParameterData['CropLandLossRate',]),
	GrazeLandGrowthRate = as.numeric(ParameterData['GrazeLandGrowthRate',]),
	# GrazeLandLossRate = as.numeric(ParameterData['GrazeLandLossRate',]),
	LandProdElastLivestock = as.numeric(ParameterData['LandProdElastLivestock',]),
	WaterProdElastLivestock = as.numeric(ParameterData['WaterProdElastLivestock',]),
	LivestockTechMult = as.numeric(ParameterData['LivestockTechMult',]),
	WaterProdEastCrops = as.numeric(ParameterData['WaterProdEastCrops',]),
	LandProdElastCrops = as.numeric(ParameterData['LandProdElastCrops',]),
	CropsTechMult = as.numeric(ParameterData['CropsTechMult',]),
	FoodNutrConvMultiplier = as.numeric(ParameterData['FoodNutrConvMultiplier',]),
	FoodNutrConv_Fish = as.numeric(ParameterData['FoodNutrConv_Fish',]),
	FoodNutrConv_Livestock = as.numeric(ParameterData['FoodNutrConv_Livestock',]),
	FoodNutrConv_Crops = as.numeric(ParameterData['FoodNutrConv_Crops',]),
	FishProdDelay = as.numeric(ParameterData['FishProdDelay',]),
	LivestockProdDelay = as.numeric(ParameterData['LivestockProdDelay',]),
	CropsProdDelay = as.numeric(ParameterData['CropsProdDelay',]),
	FoodIncomeElasticity_Low = as.numeric(ParameterData['FoodIncomeElasticity_Low',]),
	FoodIncomeElasticity_Mid = as.numeric(ParameterData['FoodIncomeElasticity_Mid',]),
	FoodIncomeElasticity_High = as.numeric(ParameterData['FoodIncomeElasticity_High',]),
	LivestockWasteFrac = as.numeric(ParameterData['LivestockWasteFrac',]),
	CropsWasteFrac = as.numeric(ParameterData['CropsWasteFrac',]),
	WaterCropFrac = as.numeric(ParameterData['WaterCropFrac',]),
	MinFoodProd = as.numeric(ParameterData['MinFoodProd',]),

	# Health and Education Submodel
	ZetaE_Low = as.numeric(ParameterData['ZetaE_Low',]),
	ZetaH_Low = as.numeric(ParameterData['ZetaH_Low',]),
	LambdaE_Low = as.numeric(ParameterData['LambdaE_Low',]),
	LambdaH_Low = as.numeric(ParameterData['LambdaH_Low',]),
	ChiEF1_Low = as.numeric(ParameterData['ChiEF1_Low',]),
	ChiHF1_Low = as.numeric(ParameterData['ChiHF1_Low',]),
	ChiHA1_RichLow = as.numeric(ParameterData['ChiHA1_RichLow',]),
	ChiHA1_PoorLow = as.numeric(ParameterData['ChiHA1_PoorLow',]),
	ChiEF2_Low = as.numeric(ParameterData['ChiEF2_Low',]),
	ChiHF2_Low = as.numeric(ParameterData['ChiHF2_Low',]),
	ChiHA2_RichLow = as.numeric(ParameterData['ChiHA2_RichLow',]),
	ChiHA2_PoorLow = as.numeric(ParameterData['ChiHA2_PoorLow',]),
	ChiHA3_RichLow = as.numeric(ParameterData['ChiHA3_RichLow',]),
	ChiHA3_PoorLow = as.numeric(ParameterData['ChiHA3_PoorLow',]),
	ZetaE_Mid = as.numeric(ParameterData['ZetaE_Mid',]),
	ZetaH_Mid = as.numeric(ParameterData['ZetaH_Mid',]),
	LambdaE_Mid = as.numeric(ParameterData['LambdaE_Mid',]),
	LambdaH_Mid = as.numeric(ParameterData['LambdaH_Mid',]),
	ChiEF1_Mid = as.numeric(ParameterData['ChiEF1_Mid',]),
	ChiHF1_Mid = as.numeric(ParameterData['ChiHF1_Mid',]),
	ChiHA1_RichMid = as.numeric(ParameterData['ChiHA1_RichMid',]),
	ChiHA1_PoorMid = as.numeric(ParameterData['ChiHA1_PoorMid',]),
	ChiEF2_Mid = as.numeric(ParameterData['ChiEF2_Mid',]),
	ChiHF2_Mid = as.numeric(ParameterData['ChiHF2_Mid',]),
	ChiHA2_RichMid = as.numeric(ParameterData['ChiHA2_RichMid',]),
	ChiHA2_PoorMid = as.numeric(ParameterData['ChiHA2_PoorMid',]),
	ChiHA3_RichMid = as.numeric(ParameterData['ChiHA3_RichMid',]),
	ChiHA3_PoorMid = as.numeric(ParameterData['ChiHA3_PoorMid',]),
	ZetaE_High = as.numeric(ParameterData['ZetaE_High',]),
	ZetaH_High = as.numeric(ParameterData['ZetaH_High',]),
	LambdaE_High = as.numeric(ParameterData['LambdaE_High',]),
	LambdaH_High = as.numeric(ParameterData['LambdaH_High',]),
	ChiEF1_High = as.numeric(ParameterData['ChiEF1_High',]),
	ChiHF1_High = as.numeric(ParameterData['ChiHF1_High',]),
	ChiHA1_RichHigh = as.numeric(ParameterData['ChiHA1_RichHigh',]),
	ChiHA1_PoorHigh = as.numeric(ParameterData['ChiHA1_PoorHigh',]),
	ChiEF2_High = as.numeric(ParameterData['ChiEF2_High',]),
	ChiHF2_High = as.numeric(ParameterData['ChiHF2_High',]),
	ChiHA2_RichHigh = as.numeric(ParameterData['ChiHA2_RichHigh',]),
	ChiHA2_PoorHigh = as.numeric(ParameterData['ChiHA2_PoorHigh',]),
	ChiHA3_RichHigh = as.numeric(ParameterData['ChiHA3_RichHigh',]),
	ChiHA3_PoorHigh = as.numeric(ParameterData['ChiHA3_PoorHigh',]),

	# Popoulation Submodel
	MinDeath_RichM1 = as.numeric(ParameterData['MinDeath_RichM1',]),
	MinDeath_PoorM1 = as.numeric(ParameterData['MinDeath_PoorM1',]),
	MinDeath_M2 = as.numeric(ParameterData['MinDeath_M2',]),
	MinDeath_M3 = as.numeric(ParameterData['MinDeath_M3',]),
	MinDeath_M4 = as.numeric(ParameterData['MinDeath_M4',]),
	MinDeath_RichF1 = as.numeric(ParameterData['MinDeath_RichF1',]),
	MinDeath_PoorF1 = as.numeric(ParameterData['MinDeath_PoorF1',]),
	MinDeath_F2 = as.numeric(ParameterData['MinDeath_F2',]),
	MinDeath_F3 = as.numeric(ParameterData['MinDeath_F3',]),
	MinDeath_F4 = as.numeric(ParameterData['MinDeath_F4',]),
	OmegaH_RichM1 = as.numeric(ParameterData['OmegaH_RichM1',]),
	OmegaH_PoorM1 = as.numeric(ParameterData['OmegaH_PoorM1',]),
	OmegaH_M2 = as.numeric(ParameterData['OmegaH_M2',]),
	OmegaH_M3 = as.numeric(ParameterData['OmegaH_M3',]),
	OmegaH_M4 = as.numeric(ParameterData['OmegaH_M4',]),
	OmegaH_RichF1 = as.numeric(ParameterData['OmegaH_RichF1',]),
	OmegaH_PoorF1 = as.numeric(ParameterData['OmegaH_PoorF1',]),
	OmegaH_F2 = as.numeric(ParameterData['OmegaH_F2',]),
	OmegaH_F3 = as.numeric(ParameterData['OmegaH_F3',]),
	OmegaH_F4 = as.numeric(ParameterData['OmegaH_F4',]),
	OmegaF_RichM1 = as.numeric(ParameterData['OmegaF_RichM1',]),
	OmegaF_PoorM1 = as.numeric(ParameterData['OmegaF_PoorM1',]),
	OmegaF_M2 = as.numeric(ParameterData['OmegaF_M2',]),
	OmegaF_M3 = as.numeric(ParameterData['OmegaF_M3',]),
	OmegaF_M4 = as.numeric(ParameterData['OmegaF_M4',]),
	OmegaF_RichF1 = as.numeric(ParameterData['OmegaF_RichF1',]),
	OmegaF_PoorF1 = as.numeric(ParameterData['OmegaF_PoorF1',]),
	OmegaF_F2 = as.numeric(ParameterData['OmegaF_F2',]),
	OmegaF_F3 = as.numeric(ParameterData['OmegaF_F3',]),
	OmegaF_F4 = as.numeric(ParameterData['OmegaF_F4',]),
	AlphaGFR = as.numeric(ParameterData['AlphaGFR',]),
	BetaE = as.numeric(ParameterData['BetaE',]),
	BetaH = as.numeric(ParameterData['BetaH',]),

	# Water Submodel
	ZetaI1_Low = as.numeric(ParameterData['ZetaI1_Low',]),
	ZetaI1_Mid = as.numeric(ParameterData['ZetaI1_Mid',]),
	ZetaI1_High = as.numeric(ParameterData['ZetaI1_High',]),
	ZetaI2_Low = as.numeric(ParameterData['ZetaI2_Low',]),
	ZetaI2_Mid = as.numeric(ParameterData['ZetaI2_Mid',]),
	ZetaI2_High = as.numeric(ParameterData['ZetaI2_High',]),
	WaterDemandPC_Low = as.numeric(ParameterData['WaterDemandPC_Low',]),
	WaterDemandPC_Mid = as.numeric(ParameterData['WaterDemandPC_Mid',]),
	WaterDemandPC_High = as.numeric(ParameterData['WaterDemandPC_High',]),
	WaterReplDelay = as.numeric(ParameterData['WaterReplDelay',])
)

ParValue = CalibPars[,1]
ParMin =  CalibPars[,2]
ParMax = CalibPars[,3]
ParRange = data.frame(min = ParMin,max = ParMax)

GlobalFit = ga( 
	type = 'real-valued',
	fitness = GlobalCost(
		p = ParValue,
		t0 = t0,
		tf = tf,
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		init = LocalFitInitValue,
		parms = LocalFitParmameterValue,
		yactual = GlobalActual
	),
	lower = ParMin,
	upper = ParMax)
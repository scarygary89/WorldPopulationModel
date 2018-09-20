########################                       ########################
########################      FOOD SUBMODEL    ########################
########################                       ########################

library(foreach)
library(doParallel)
library(GA)
numcore = detectCores()
cl<-makeCluster(numcore) 
print(cl)

################# SUBMODEL WITH EXOGENOUS INPUTS

FoodMod = function(t0,tf,delta_t,delayyearlength,exog,init,parms) 
{	
	with(as.list(c(parms)), {	
		tspan = seq(from=t0, to=tf, by=delta_t)
		aux_names = c(
			'FishProduction',
			'LivestockProduction',
			'CropProduction',
			'FishWaste',
			'LivestockWaste',
			'CropWaste',
			'FishConsumption',
			'LivestockConsumption',
			'CropConsumption',
			'NutritionConsPC_RichLow',
			'NutritionConsPC_PoorLow',
			'NutritionConsPC_RichMid',
			'NutritionConsPC_PoorMid',
			'NutritionConsPC_RichHigh',
			'NutritionConsPC_PoorHigh',
			'AgriWaterDemand'
			)
		AuxData = matrix(NA,nrow = (length(tspan)),ncol = length(aux_names))		
		colnames(AuxData) = aux_names
		StockData = matrix(NA,nrow = (length(tspan) + 1),ncol = length(init))
		colnames(StockData) = names(init)	
		stocks = init
		StockData[1,] = stocks
		for(i in 1:length(tspan)) {
			FoodStock_l = c(
				Fishstock = as.numeric(stocks['Fishstock']),
				Livestock = as.numeric(stocks['Livestock']),
				Crops = as.numeric(stocks['Crops']))
			FoodDemandPC_r = c( 
				Low = as.numeric(stocks['FoodDemandPC_Low']),
				Mid = as.numeric(stocks['FoodDemandPC_Mid']),
				High = as.numeric(stocks['FoodDemandPC_High']))
			RegPop_r = c(
				Low = exog[i,'LowPop'],
				Mid = exog[i,'MidPop'],
				High = exog[i,'HighPop'])

		# Extract Delayed Values
			if (i < (1 + delayyearlength / delta_t)) {
				PrevFoodDemandPC_Low = stocks['FoodDemandPC_Low'] 
				PrevFoodDemandPC_Mid = stocks['FoodDemandPC_Mid']
				PrevFoodDemandPC_High = stocks['FoodDemandPC_High']
				PrevEconOutput_Low = (1 - InitEconOutputGrowth_Low) * exog[i,'EconOutput_Low']
				PrevEconOutput_Mid = (1 - InitEconOutputGrowth_Mid) * exog[i,'EconOutput_Mid']
				PrevEconOutput_High = (1 - InitEconOutputGrowth_High) * exog[i,'EconOutput_High']
				PrevEconOutputPC_Low =  PrevEconOutput_Low / (RegPop_r['Low'] * 1000)
				PrevEconOutputPC_Mid =  PrevEconOutput_Mid / (RegPop_r['Mid'] * 1000)
				PrevEconOutputPC_High = PrevEconOutput_High / (RegPop_r['High'] * 1000)
			}
			if (i >= (1 + delayyearlength / delta_t)) { 
				PrevFoodDemandPC_Low = StockData[(i - delayyearlength / delta_t),"FoodDemandPC_Low"]
				PrevFoodDemandPC_Mid = StockData[(i - delayyearlength / delta_t), "FoodDemandPC_Mid"]
				PrevFoodDemandPC_High = StockData[(i - delayyearlength / delta_t), "FoodDemandPC_High"]
				PrevEconOutput_Low = exog[(i - delayyearlength / delta_t), "EconOutput_Low"]
				PrevEconOutput_Mid = exog[(i - delayyearlength / delta_t), "EconOutput_Mid"]
				PrevEconOutput_High = exog[(i - delayyearlength / delta_t), "EconOutput_High"]
				PrevEconOutputPC_Low =  PrevEconOutput_Low / 
									(exog[(i - delayyearlength / delta_t), "LowPop"]*1000)
				PrevEconOutputPC_Mid =  PrevEconOutput_Mid / 
									(exog[(i - delayyearlength / delta_t), "MidPop"]*1000)
				PrevEconOutputPC_High =  PrevEconOutput_High / 
									(exog[(i - delayyearlength / delta_t), "HighPop"]*1000)
			} 
			PrevFoodDemandPC_r = c( 	
				Low =  PrevFoodDemandPC_Low, 
				Mid =  PrevFoodDemandPC_Mid, 
				High = PrevFoodDemandPC_High)
			PrevEconOutputPC_r = c(		
				Low = 	PrevEconOutputPC_Low, 
				Mid = 	PrevEconOutputPC_Mid, 
				High =	PrevEconOutputPC_High)		
			EconOutputPC_r = c(
				Low = 	exog[i,'EconOutput_Low'] / (RegPop_r['Low']*1000),
				Mid = 	exog[i,'EconOutput_Mid'] / (RegPop_r['Mid']*1000),
				High = 	exog[i,'EconOutput_High'] / (RegPop_r['High']*1000)) 	

			# Global Food System 
			FoodOut       	= Food(
								FoodStock_l,
								stocks['Fisheries'],
								FoodDemandPC_r,
								stocks['GrazeLand'],
								stocks['CropLand'],
								exog[i,'TempAnamoly'],
								RegPop_r,
								EconOutputPC_r,
								PrevEconOutputPC_r,
								PrevFoodDemandPC_r,
								exog[i,'Freshwater'],
								parms)

			################ STORE OUTPUT
			# print(parms)

			# AUXILIARY VARIABLES
			aux = c(
				FoodOut[['FoodProd_l']],
				FoodOut[['FoodWaste_l']],
				FoodOut[['FoodCons_l']],
				FoodOut[['NutritionConsPC_kr']],
				FoodOut[['AgriWaterDemand']]
			)

			AuxData[i,] = aux

			# STOCK VARIABLES
			dstocks = c(
			# Food Stocks (Global)
				FoodOut[["dFisheries"]], 
				FoodOut[["dFoodStock_l"]],
				FoodOut[["dFoodDemandPC_r"]],
				FoodOut[["dGrazeLand"]], 
				FoodOut[["dCropLand"]]
			) 

			stocks = stocks + dstocks * delta_t
			StockData[i+1,] = stocks
			# print(StockData)
			# print(AuxData)
		}
	Output = cbind(tspan, StockData[-(length(tspan)+1),],AuxData)
	colnames(Output)[1] = 'time'
	return(Output)
	})
}


################# COST FUNCTION

FoodCost = function(p,t0,tf,delta_t,delayyearlength,exog,init,parms,yactual)
{
	whichpar = names(parms)[names(parms) %in% names(p)]
	whichinit = names(init)[names(init) %in% names(p)]
	parms[whichpar] = p[whichpar]
	init[whichinit] = p[whichinit]
	ysim = FoodMod(t0,tf,delta_t,delayyearlength,exog,init,parms)
	RunCost = modCost(ysim,yactual,x = 'time',y = 'value',err = 'error',
		scaleVar=T)
	# print(RunCost)
	return(RunCost)
}

################# PARAMETER ESTIMATION FUNCTION

FoodFit = function(parvalue,parmin,parmax,yactual,optmethod,control=list(),
	delta_t,delayyearlength,exog,init,parms)
{
	t0 = min(exog$time)
	tf = max(exog$time)
	ptm = proc.time() 
	FoodFit = modFit(
		f = FoodCost,
		p = parvalue,
		lower = parmin,
		upper = parmax,
		t0 = t0,
		tf = tf,
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		exog = exog,
		init = init,
		parms = parms,
		yactual = yactual,
		method = optmethod,
		control = control
		)
	ptm = proc.time() - ptm
	print(ptm)
	return(FoodFit)
}

################# DEFINE INITIAL PARAMETER VALUES

FoodParms = c( 	
	ThetaU = as.numeric(ParameterValue['ThetaU']),
	ZetaU = as.numeric(ParameterValue['ZetaU']),
	FishingTech = as.numeric(ParameterValue['FishingTech']),
	GrazeLandGrowthRate = as.numeric(ParameterValue['GrazeLandGrowthRate']),
	GrazeLandLossRate = as.numeric(ParameterValue['GrazeLandLossRate']),
	CropLandGrowthRate = as.numeric(ParameterValue['CropLandGrowthRate']),
	CropLandLossRate = as.numeric(ParameterValue['CropLandLossRate']),
	LandProdElastLivestock = as.numeric(ParameterValue['LandProdElastLivestock']),
	WaterProdElastLivestock = as.numeric(ParameterValue['WaterProdElastLivestock']),
	LivestockTechMult = as.numeric(ParameterValue['LivestockTechMult']),
	WaterProdEastCrops = as.numeric(ParameterValue['WaterProdEastCrops']),
	LandProdElastCrops = as.numeric(ParameterValue['LandProdElastCrops']),
	CropsTechMult = as.numeric(ParameterValue['CropsTechMult']),
	FoodNutrConvMultiplier = as.numeric(ParameterValue['FoodNutrConvMultiplier']),
	FoodNutrConv_Fish = as.numeric(ParameterValue['FoodNutrConv_Fish']),
	FoodNutrConv_Livestock = as.numeric(ParameterValue['FoodNutrConv_Livestock']),
	FoodNutrConv_Crops = as.numeric(ParameterValue['FoodNutrConv_Crops']),
	FoodConsFrac_Fish = as.numeric(ParameterValue['FoodConsFrac_Fish']),
	FoodConsFrac_Livestock = as.numeric(ParameterValue['FoodConsFrac_Livestock']),
	FoodConsFrac_Crops = as.numeric(ParameterValue['FoodConsFrac_Crops']),
	FishProdDelay = as.numeric(ParameterValue['FishProdDelay']),
	LivestockProdDelay = as.numeric(ParameterValue['LivestockProdDelay']),
	CropsProdDelay = as.numeric(ParameterValue['CropsProdDelay']),
	FoodIncomeElasticity_Low = as.numeric(ParameterValue['FoodIncomeElasticity_Low']),
	FoodIncomeElasticity_Mid = as.numeric(ParameterValue['FoodIncomeElasticity_Mid']),
	FoodIncomeElasticity_High = as.numeric(ParameterValue['FoodIncomeElasticity_High']),
	FishAccess_RichLow = as.numeric(ParameterValue['FishAccess_RichLow']),
	FishAccess_RichMid = as.numeric(ParameterValue['FishAccess_RichMid']),
	FishAccess_RichHigh = as.numeric(ParameterValue['FishAccess_RichHigh']),
	FishAccess_PoorLow = as.numeric(ParameterValue['FishAccess_PoorLow']),
	FishAccess_PoorMid = as.numeric(ParameterValue['FishAccess_PoorMid']),
	FishAccess_PoorHigh = as.numeric(ParameterValue['FishAccess_PoorHigh']),
	LivestockAccess_RichLow = as.numeric(ParameterValue['LivestockAccess_RichLow']),
	LivestockAccess_RichMid = as.numeric(ParameterValue['LivestockAccess_RichMid']),
	LivestockAccess_RichHigh = as.numeric(ParameterValue['LivestockAccess_RichHigh']),
	LivestockAccess_PoorLow = as.numeric(ParameterValue['LivestockAccess_PoorLow']),
	LivestockAccess_PoorMid = as.numeric(ParameterValue['LivestockAccess_PoorMid']),
	LivestockAccess_PoorHigh = as.numeric(ParameterValue['LivestockAccess_PoorHigh']),
	CropsAccess_RichLow = as.numeric(ParameterValue['CropsAccess_RichLow']),
	CropsAccess_RichMid = as.numeric(ParameterValue['CropsAccess_RichMid']),
	CropsAccess_RichHigh = as.numeric(ParameterValue['CropsAccess_RichHigh']),
	CropsAccess_PoorLow = as.numeric(ParameterValue['CropsAccess_PoorLow']),
	CropsAccess_PoorMid = as.numeric(ParameterValue['CropsAccess_PoorMid']),
	CropsAccess_PoorHigh = as.numeric(ParameterValue['CropsAccess_PoorHigh']),
	FishWasteFrac = as.numeric(ParameterValue['FishWasteFrac']),
	LivestockWasteFrac = as.numeric(ParameterValue['LivestockWasteFrac']),
	CropsWasteFrac = as.numeric(ParameterValue['CropsWasteFrac']),
	WaterCropFrac = as.numeric(ParameterValue['WaterCropFrac']),
	MinFoodProd = as.numeric(ParameterValue['MinFoodProd']),
	InitEconOutputGrowth_Low = as.numeric(ParameterValue['InitEconOutputGrowth_Low']), 
	InitEconOutputGrowth_Mid = as.numeric(ParameterValue['InitEconOutputGrowth_Mid']),
	InitEconOutputGrowth_High = as.numeric(ParameterValue['InitEconOutputGrowth_High']),
	PoorFrac = as.numeric(ParameterValue['PoorFrac']),
	CropsWaterConsRate = as.numeric(ParameterValue['CropsWaterConsRate']),
	LivestockWaterConsRate = as.numeric(ParameterValue['LivestockWaterConsRate'])
)

################# DEFINE INITIAL CONDITIONS

FoodInit = c( 	
	Fisheries = as.numeric(InitValue['Fisheries']),
	Fishstock = as.numeric(InitValue['Fishstock']),
	Livestock = as.numeric(InitValue['Livestock']),
	Crops = as.numeric(InitValue['Crops']),
	FoodDemandPC_Low = as.numeric(InitValue['FoodDemandPC_Low']),
	FoodDemandPC_Mid = as.numeric(InitValue['FoodDemandPC_Mid']),
	FoodDemandPC_High = as.numeric(InitValue['FoodDemandPC_High']),
	GrazeLand = as.numeric(InitValue['GrazeLand']),
	CropLand = as.numeric(InitValue['CropLand'])
)


################# LOAD EXOGENOUS DATA

FoodExog = na.omit(cbind(
	time = CalibData['time'],
	LowPop = CalibData[,'LowPop'],
	MidPop = CalibData[,'MidPop'],
	HighPop = CalibData[,'HighPop'],
	EconOutput_Low = CalibData[,'EconOutput_Low'],
	EconOutput_Mid = CalibData[,'EconOutput_Mid'],	
	EconOutput_High = CalibData[,'EconOutput_High'],
	GlobalTemp = CalibData[,'TempAnamoly'],
	Freshwater = CalibData[,'Freshwater']
))

################# LOAD ACTUAL DATA

FoodActual = na.omit(melt(
	data.frame(cbind(
		time = CalibData$time,
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
		AgriWaterDemand = CalibData$SmoothAgriculturalWaterConsumption
		)),
	id ='time'))
FoodActual$variable = as.character(FoodActual$variable)
FoodActual = FoodActual[,c('variable','time','value')]

FoodActual$error[FoodActual$variable == 'FishProduction'] =
	mean(FoodActual$value[FoodActual$variable == 'FishProduction']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'FishProduction'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'LivestockProduction'] =
	mean(FoodActual$value[FoodActual$variable == 'LivestockProduction']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'LivestockProduction'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'Fishstock'] =
	mean(FoodActual$value[FoodActual$variable == 'Fishstock']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'Fishstock'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'Livestock'] =
	mean(FoodActual$value[FoodActual$variable == 'Livestock']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'Livestock'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'Crops'] =
	mean(FoodActual$value[FoodActual$variable == 'Crops']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'Crops'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'CropProduction'] =
	mean(FoodActual$value[FoodActual$variable == 'CropProduction']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'CropProduction'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'LivestockWaste'] =
	mean(FoodActual$value[FoodActual$variable == 'LivestockWaste']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'LivestockWaste'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'CropWaste'] =
	mean(FoodActual$value[FoodActual$variable == 'CropWaste']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'CropWaste'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'FishConsumption'] =
	mean(FoodActual$value[FoodActual$variable == 'FishConsumption']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'FishConsumption'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'LivestockConsumption'] =
	mean(FoodActual$value[FoodActual$variable == 'LivestockConsumption']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'LivestockConsumption'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'CropConsumption'] =
	mean(FoodActual$value[FoodActual$variable == 'CropConsumption']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'CropConsumption'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'NutritionConsPC_RichLow'] =
	mean(FoodActual$value[FoodActual$variable == 'NutritionConsPC_RichLow']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'NutritionConsPC_RichLow'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'NutritionConsPC_PoorLow'] =
	mean(FoodActual$value[FoodActual$variable == 'NutritionConsPC_PoorLow']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'NutritionConsPC_PoorLow'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'NutritionConsPC_RichMid'] =
	mean(FoodActual$value[FoodActual$variable == 'NutritionConsPC_RichMid']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'NutritionConsPC_RichMid'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'NutritionConsPC_PoorMid'] =
	mean(FoodActual$value[FoodActual$variable == 'NutritionConsPC_PoorMid']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'NutritionConsPC_PoorMid'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'NutritionConsPC_RichHigh'] =
	mean(FoodActual$value[FoodActual$variable == 'NutritionConsPC_RichHigh']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'NutritionConsPC_RichHigh'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'NutritionConsPC_PoorHigh'] =
	mean(FoodActual$value[FoodActual$variable == 'NutritionConsPC_PoorHigh']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'NutritionConsPC_PoorHigh'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'CropLand'] =
	mean(FoodActual$value[FoodActual$variable == 'CropLand']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'CropLand'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'GrazeLand'] =
	mean(FoodActual$value[FoodActual$variable == 'GrazeLand']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'GrazeLand'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'Fisheries'] =
	mean(FoodActual$value[FoodActual$variable == 'Fisheries']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'Fisheries'] - 1979) ^ 2 / 1297)
FoodActual$error[FoodActual$variable == 'AgriWaterDemand'] =
	mean(FoodActual$value[FoodActual$variable == 'AgriWaterDemand']) #* 
	# (1 - (FoodActual$time[FoodActual$variable == 'AgriWaterDemand'] - 1979) ^ 2 / 1297)

################# DEFINE CALIBRATION PARAMETERS

FoodCalibPars =  rbind(
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
	CropsWaterConsRate = as.numeric(ParameterData['CropsWaterConsRate',]),
	LivestockWaterConsRate = as.numeric(ParameterData['LivestockWaterConsRate',])
)
FoodParValue = FoodCalibPars[,1]
FoodParMin =  FoodCalibPars[,2]
FoodParMax = FoodCalibPars[,3]
FoodFitness = function(p) 
{
	names(p) = names(FoodParValue)
	FoodCost = -FoodCost(
		p=p,
		t0 = t0,
		tf = tf,
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		init = FoodInit,
		parms = FoodParms,
		exog = FoodExog,
		yactual = FoodActual
	)$model
	return(FoodCost)
}
################# Genetic Algo
registerDoParallel(cl)
ptm = proc.time() 
FoodResults = gaisl( 
	type = 'real-valued',
	fitness = FoodFitness,
	lower = FoodParMin,
	upper = FoodParMax,
	suggestions = FoodParValue,
	numIslands = numcore,
    popSize = 1000,
    run = 400,
    crossover = gareal_blxCrossover, 
    maxiter = 10000)
ptm = proc.time() - ptm
print(ptm)	
stopCluster(cl)

################# PLOT FITTED VALUES

FoodFitData = CalibPlotFunc(FoodResults,FoodActual,FoodParms,FoodExog,FoodInit,
	FoodMod,delta_t,delayyearlength,'FoodSubmodel')



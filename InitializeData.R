
print('----------------- INITIALIZE INPUTS ---------------------')

Pop_RM1 = 4600
Pop_RM2 = 4800
Pop_RM3 = 800
Pop_RM4 = 300
Pop_RF1 = 5500
Pop_RF2 = 5700
Pop_RF3 = 900
Pop_RF4 = 400             
Pop_PM1 = 42000
Pop_PM2 = 43400
Pop_PM3 = 7700
Pop_PM4 = 2900
Pop_PF1 = 49900
Pop_PF2 = 52100
Pop_PF3 = 8800
Pop_PF4 = 3800


InitTotPop = sum(
		Pop_RM1,
		Pop_RM2,
		Pop_RM3,
		Pop_RM4,
		Pop_RF1,
		Pop_RF2,
		Pop_RF3,
		Pop_RF4, 
		Pop_PM1,
		Pop_PM2,
		Pop_PM3,
		Pop_PM4,
		Pop_PF1,
		Pop_PF2,
		Pop_PF3,
		Pop_PF4)

InitCO2Concentration = 15
InitTemp = 13

RenewableInputElast = 0.2
NonrenewableInputElast = 0.2
CapitalInputElast = 0.2
LaborInputElast = 0.4

EmployedWorkRatio_RM1 = 0
EmployedWorkRatio_RM2 = 0.7
EmployedWorkRatio_RM3 = 0.5
EmployedWorkRatio_RM4 = 0
EmployedWorkRatio_RF1 = 0
EmployedWorkRatio_RF2 = 0.7
EmployedWorkRatio_RF3 = 0.5
EmployedWorkRatio_RF4 = 0             
EmployedWorkRatio_PM1 = 0
EmployedWorkRatio_PM2 = 0.7
EmployedWorkRatio_PM3 = 0.5
EmployedWorkRatio_PM4 = 0
EmployedWorkRatio_PF1 = 0
EmployedWorkRatio_PF2 = 0.7
EmployedWorkRatio_PF3 = 0.5
EmployedWorkRatio_PF4 = 0

EmployedWorkRatio_ijk = c(
	EmployedWorkRatio_RM1,
	EmployedWorkRatio_RM2,
	EmployedWorkRatio_RM3,
	EmployedWorkRatio_RM4,
	EmployedWorkRatio_RF1,
	EmployedWorkRatio_RF2,
	EmployedWorkRatio_RF3,
	EmployedWorkRatio_RF4,
	EmployedWorkRatio_PM1,
	EmployedWorkRatio_PM2,
	EmployedWorkRatio_PM3,
	EmployedWorkRatio_PM4,
	EmployedWorkRatio_PF1,
	EmployedWorkRatio_PF2,
	EmployedWorkRatio_PF3,
	EmployedWorkRatio_PF4)


InitCapital = 1e7
InitLabor = sum(
	c(	Pop_RM1,
		Pop_RM2,
		Pop_RM3,
		Pop_RM4,
		Pop_RF1,
		Pop_RF2,
		Pop_RF3,
		Pop_RF4, 
		Pop_PM1,
		Pop_PM2,
		Pop_PM3,
		Pop_PM4,
		Pop_PF1,
		Pop_PF2,
		Pop_PF3,
		Pop_PF4) * EmployedWorkRatio_ijk)
InitRenewableResources = 1e20
InitNonrenewableResources = 1e20
TechMult = 1/1000

InitEconOutput = TechMult*InitLabor^LaborInputElast * 
				InitCapital^CapitalInputElast *
			 	InitNonrenewableResources^NonrenewableInputElast * 
			 	InitRenewableResources^RenewableInputElast 

################# STOCK DATA #################

InitData = c( 
  # Economic Stocks
		Capital = InitCapital,
		RenewableResources = InitRenewableResources,
		NonrenewableResources = InitNonrenewableResources,
		EconOutput = InitEconOutput,

  # Climate Stocks
		CO2Conc = InitCO2Concentration,
		GlobalTemp = InitTemp,

  # Food Stocks
		Fisheries = 24999,  
		Fishstock = 50000,   
		Livestock = 1,        
		Crops = 0,
		FoodDemandPC = 1,
		AgriculturalLand = 1,

  # Health and Education Stocks
		EducationServices = 1000,
		HealthServices = 1000,
		
  # Population Stocks         
		RM1 = Pop_RM1,
		RM2 = Pop_RM2,
		RM3 = Pop_RM3,
		RM4 = Pop_RM4,
		RF1 = Pop_RF1,
		RF2 = Pop_RF2,
		RF3 = Pop_RF3,
		RF4 = Pop_RF4, 
		PM1 = Pop_PM1,
		PM2 = Pop_PM2,
		PM3 = Pop_PM3,
		PM4 = Pop_PM4,
		PF1 = Pop_PF1,
		PF2 = Pop_PF2,
		PF3 = Pop_PF3,
		PF4 = Pop_PF4,
		TotalPop = InitTotPop,

  # Water Stocks
		Freshwater = 100000)


################# PARAMETER DATA #################

ParameterData = list(
  # Population Coefficients
		MinDeath = 10,
		TNMFR = 0.9,
		AlphaC = 0.414,
		BetaEC = 0.311,
		BetaHC = 0.153,
		AlphaM = 0.148,
		BetaEM = 0.545,
		TotalFecundity = 15,
		Beta1 = 29.4,
		FemaleBirthRatio = 0.5,

		OmegaF_RM1 = 1,
		OmegaF_RM2 = 1,
		OmegaF_RM3 = 1,
		OmegaF_RM4 = 1,
		OmegaF_RF1 = 1,
		OmegaF_RF2 = 1,
		OmegaF_RF3 = 1,
		OmegaF_RF4 = 1,
		OmegaF_PM1 = 1,
		OmegaF_PM2 = 1,
		OmegaF_PM3 = 1,
		OmegaF_PM4 = 1,
		OmegaF_PF1 = 1,
		OmegaF_PF2 = 1,
		OmegaF_PF3 = 1,
		OmegaF_PF4 = 1,

		OmegaH_RM1 = 1,
		OmegaH_RM2 = 1,
		OmegaH_RM3 = 1,
		OmegaH_RM4 = 1,
		OmegaH_RF1 = 1,
		OmegaH_RF2 = 1,
		OmegaH_RF3 = 1,
		OmegaH_RF4 = 1,	
		OmegaH_PM1 = 1,
		OmegaH_PM2 = 1,
		OmegaH_PM3 = 1,
		OmegaH_PM4 = 1,
		OmegaH_PF1 = 1,
		OmegaH_PF2 = 1,
		OmegaH_PF3 = 1,
		OmegaH_PF4 = 1,
		NutritionReq = 2000,

  # Food Coefficients
		ThetaU = 0.1,
		LandGrowthRate = 0.005,
		LandLossRate = 0,
		LandProdElastLivestock = 0.5,
		WaterProdElastLivestock = 0.5,
		LivestockTechMult = 1,
		WaterProdEastCrops = 0.5,
		LandProdElastCrops = 0.5,
		CropsTechMult = 1,

		FoodNutrConv_Fish = 1,
		FoodNutrConv_Livestock = 1,
		FoodNutrConv_Crops = 1,
		
		FishConsFrac_Fish = .2,
		FishConsFrac_Livestock = .4,
		FishConsFrac_Crops = .4,		
		FishProdDelay = 1,
		
		LivestockProdDelay = 1,
		CropsProdDelay = 1,
		FoodIncomeElasticity = 1/10,
		FoodAccess_Rich = 0.3,
		FoodAccess_Poor = 0.7,
		FishWasteFrac = 0.1,
		LivestockWasteFrac = 0.1,
		CropsWasteFrac = 0.1,

    # Climate Coefficients
		Lambda = 22,
		RefTemp = InitTemp,
		PsiE = 1/10,
		PsiL = 1/10,
		Gamma = 1/10,
		RefCO2Conc = InitCO2Concentration,
		OtherRadForce = 0,

    # Economy Coefficients
		RenewableInputElast = RenewableInputElast,
		NonrenewableInputElast = NonrenewableInputElast,
		CapitalInputElast = CapitalInputElast,
		LaborInputElast = LaborInputElast,
		TechMult = TechMult,

		EmployedWorkRatio_RM1 = EmployedWorkRatio_RM1, 
		EmployedWorkRatio_RM2 = EmployedWorkRatio_RM2, 
		EmployedWorkRatio_RM3 = EmployedWorkRatio_RM3, 
		EmployedWorkRatio_RM4 = EmployedWorkRatio_RM4, 
		EmployedWorkRatio_RF1 = EmployedWorkRatio_RF1, 
		EmployedWorkRatio_RF2 = EmployedWorkRatio_RF2, 
		EmployedWorkRatio_RF3 = EmployedWorkRatio_RF3, 
		EmployedWorkRatio_RF4 = EmployedWorkRatio_RF4,            
		EmployedWorkRatio_PM1 = EmployedWorkRatio_PM1, 
		EmployedWorkRatio_PM2 = EmployedWorkRatio_PM2, 
		EmployedWorkRatio_PM3 = EmployedWorkRatio_PM3, 
		EmployedWorkRatio_PM4 = EmployedWorkRatio_PM4, 
		EmployedWorkRatio_PF1 = EmployedWorkRatio_PF1, 
		EmployedWorkRatio_PF2 = EmployedWorkRatio_PF2, 
		EmployedWorkRatio_PF3 = EmployedWorkRatio_PF3, 
		EmployedWorkRatio_PF4 = EmployedWorkRatio_PF4, 	
	
		ReplRateRenewable = 0.02,
		SavingsRate = 0.1,
		DeprecRate = 0.01,
		RenewableConsIntensity = 1e-9,
		NonrenewableConsIntensity = 1e-9,
		IneqMult = 1,

    # Water Coefficients
		DeltaW = 0.01,
		WaterReplRate = 0.3,
		ZetaI = 1,
		WaterDemandPC = 1,

    # Health and Education Coefficients
		EducationInvestFrac = 0.1,
		HealthInvestFrac = 0.2,
		ZetaE = 1,
		ZetaH = 1,
		LambdaE = 100,
		LambdaH = 100,
		ChiEF_Rich = 0.05, 
		ChiEF_Poor = 0.05,
		ChiHF_Rich = 0.05, 
		ChiHF_Poor = 0.05,
		ChiHA_Rich = 0.05, 
		ChiHA_Poor = 0.05)

print('-----------------      COMPLETE     ---------------------')

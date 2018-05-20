########################                       ########################
########################    FOOD SUBSYSTEM     ########################
########################                       ########################

print('-------------    LOAD FOOD SUBSYSTEM  -------------------')


Food = function(Crops,Livestock,Fishstock,Fisheries,FoodDemandPC,AgriculturalLand,
    GlobalTemp,Pop_ijk,TotalPop,EconOutputPC,PrevEconOutputPC,PrevFoodDemandPC,Freshwater,
    parms) 
{
    with(parms, {        

        # Input Conversion
        FoodNutritionCoversion = c(
            Fish = FoodNutrConv_Fish,
            Livestock = FoodNutrConv_Livestock,
            Crops = FoodNutrConv_Crops)

        FoodConsumptionPerc = c(
            Fish = FishConsFrac_Fish,
            Livestock = FishConsFrac_Livestock,
            Crops = FishConsFrac_Crops)

        FoodAccess_k = c(
            FoodAccess_Rich,
            FoodAccess_Poor)

        # Auxiliary Variables

        FoodDemand = TotalPop * FoodDemandPC
        ChangeEconOutputPC = EconOutputPC - PrevEconOutputPC
        FishProdCap     = Fisheries * 0.8
        CropsProdCap    = CropsTechMult * AgriculturalLand^LandProdElastCrops * Freshwater^WaterProdEastCrops
        LivestockProdCap= LivestockTechMult * AgriculturalLand^LandProdElastLivestock * 
        	Freshwater^WaterProdElastLivestock
        TargetFishStock = FoodConsumptionPerc['Fish'] * FoodDemand
        TargetLivestock = FoodConsumptionPerc['Livestock'] * FoodDemand
        TargetCrops     = FoodConsumptionPerc['Crops'] * FoodDemand
        AgriWaterDemand = (Crops/(CropsTechMult*AgriculturalLand^LandProdElastCrops))^(1/WaterProdEastCrops) + 
        (Livestock/(LivestockTechMult*AgriculturalLand^LandProdElastLivestock))^(1/WaterProdElastLivestock)
        FishRepl = ThetaU * Fisheries 
        FishProd = max(min((TargetFishStock - Fishstock)/FishProdDelay,FishProdCap),0) 
        FishExtract = FishProd 
        FishCons = min(FoodConsumptionPerc['Fish'] * FoodDemand,Fishstock)
        FishWaste = FishWasteFrac * Fishstock
        LivestockProd = max(min((TargetLivestock - Livestock)/LivestockProdDelay,LivestockProdCap),0)
        LivestockCons = min(FoodConsumptionPerc['Livestock'] * FoodDemand,
        	max(Livestock*(1-LivestockWasteFrac ),0))
        LivestockWaste = LivestockWasteFrac * Livestock
        CropsProd = max(min((TargetCrops - Crops)/CropsProdDelay,CropsProdCap),0)
        CropsCons = min(FoodConsumptionPerc['Crops'] * FoodDemand, Crops)
        CropsWaste = CropsWasteFrac * Crops
        AgriLandGain = LandGrowthRate * AgriculturalLand
        ArgiLandLoss = LandLossRate * AgriculturalLand
        NutritionCons = sum(c(FishCons,LivestockCons,CropsCons) * FoodNutritionCoversion) 
		TotalPop_k = c(Rich = sum(Pop_ijk[c('RM1','RM2','RM3','RM4','RF1','RF2','RF3','RF4')]),
			Poor = sum(Pop_ijk[c('PM1','PM2','PM3','PM4','PF1','PF2','PF3','PF4')]))
        NutritionConsPC_k = NutritionCons / TotalPop_k * FoodAccess_k

        # Stock and Flow Variables
        dFisheries      = FishRepl - FishExtract - FishWaste 
        dFishstock      = FishProd - FishCons - FishWaste 
        dLivestock      = LivestockProd - LivestockCons - LivestockWaste       
        dCrops          = CropsProd - CropsCons
        dFoodDemandPC   = FoodIncomeElasticity*(ChangeEconOutputPC/PrevEconOutputPC)*PrevFoodDemandPC 
        dAgriculturalLand = AgriLandGain - ArgiLandLoss
        
        # Output
        list( dFisheries = dFisheries,
              dFishstock = dFishstock,
              dLivestock = dLivestock,
              dCrops = dCrops,
              dFoodDemandPC = dFoodDemandPC,
              dAgriculturalLand = dAgriculturalLand,
              NutritionConsPC_k = NutritionConsPC_k,
              AgriWaterDemand = AgriWaterDemand)
   	})
}
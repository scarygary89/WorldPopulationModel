########################                       ########################
########################    FOOD SUBSYSTEM     ########################
########################                       ########################


Food = function(FoodStock_l,Fisheries,FoodDemandPC_r,GrazeLand,CropLand,
    GlobalTemp,RegPop_ijkr,RegPop_r,EconOutputPC_r,PrevEconOutputPC_r,
    PrevFoodDemandPC_r,Freshwater,
    parms) 
{
    with(parms, {        

        # Input Conversion
        FoodNutritionCoversion_l = c(
            Fish = FoodNutrConv_Fish,
            Livestock = FoodNutrConv_Livestock,
            Crops = FoodNutrConv_Crops)
        FoodConsumptionPerc_l = c(
            Fish = FishConsFrac_Fish,
            Livestock = FishConsFrac_Livestock,
            Crops = FishConsFrac_Crops)
        ProdDelay_l = c(
            Fish = FishProdDelay,
            Livestock = LivestockProdDelay,
            Crops = CropsProdDelay)
        FoodWasteFrac_l = c(
            Fish = FishWasteFrac,
            Livestock = LivestockWasteFrac,
            Crops = CropsWasteFrac)         
        FoodAccess_kr =  cbind(   
            Low = c(
                Rich = FoodAccess_RichLow,
                Poor = FoodAccess_PoorLow),
            Mid = c(
                Rich =  FoodAccess_RichMid,
                Poor = FoodAccess_PoorMid),
            High = c(
                Rich = FoodAccess_RichHigh,
                Poor = FoodAccess_PoorHigh))
        FoodIncomeElasticity_r = c(
            Low = FoodIncomeElasticity_Low,
            Mid = FoodIncomeElasticity_Mid,
            High = FoodIncomeElasticity_High)

        # Auxiliary Variables

        FoodDemand = sum(RegPop_r * FoodDemandPC_r)
        ChangeEconOutputPC_r = EconOutputPC_r - PrevEconOutputPC_r
        FishProdCap     = Fisheries * ZetaU
        LivestockProdCap= LivestockTechMult * GrazeLand^LandProdElastLivestock * 
        	Freshwater^WaterProdElastLivestock
        CropsProdCap    = CropsTechMult *CropLand^LandProdElastCrops * 
                            Freshwater^WaterProdEastCrops
        FoodProdCap_l = c(
            Fish = FishProdCap,
            Livestock = LivestockProdCap,
            Crops = CropsProdCap)
        TargetFood_l = FoodConsumptionPerc_l * FoodDemand
        AgriWaterDemand = (FoodStock_l['Crops'] / 
            (CropsTechMult*CropLand^LandProdElastCrops))^(1/WaterProdEastCrops) + 
            (FoodStock_l['Livestock']/(LivestockTechMult*GrazeLand^LandProdElastLivestock)) ^ 
            (1/WaterProdElastLivestock)
        FoodCons_l = pmin(FoodConsumptionPerc_l * FoodDemand,FoodStock_l)
        FoodProd_l = pmax(pmin((TargetFood_l - FoodStock_l)/ProdDelay_l,FoodProdCap_l),0)
        FoodWaste_l = FoodWasteFrac_l * FoodStock_l

        GrazeLandGain = GrazeLandGrowthRate * GrazeLand
        GrazeLandLoss = GrazeLandLossRate * GrazeLand
        CropLandGain = CropLandGrowthRate * CropLand
        CropLandLoss = CropLandLossRate * CropLand

        NutritionCons = sum(FoodCons_l * FoodNutritionCoversion_l) 
        RegPop_kr = cbind(
            Low = c(
                Rich  = sum(RegPop_ijkr[['Low']][c('RM1','RM2','RM3','RM4','RF1','RF2','RF3','RF4')]),
                Poor  = sum(RegPop_ijkr[['Low']][c('PM1','PM2','PM3','PM4','PF1','PF2','PF3','PF4')])),
            Mid = c(    
                Rich  = sum(RegPop_ijkr[['Mid']][c('RM1','RM2','RM3','RM4','RF1','RF2','RF3','RF4')]),
                Poor  = sum(RegPop_ijkr[['Mid']][c('PM1','PM2','PM3','PM4','PF1','PF2','PF3','PF4')])),
            High = c(
                Rich = sum(RegPop_ijkr[['High']][c('RM1','RM2','RM3','RM4','RF1','RF2','RF3','RF4')]),
                Poor = sum(RegPop_ijkr[['High']][c('PM1','PM2','PM3','PM4','PF1','PF2','PF3','PF4')])))
        NutritionConsPC_kr = (NutritionCons * FoodAccess_kr / RegPop_kr) 
        
        FishRepl = ThetaU * Fisheries 
        FishExtract = FoodProd_l['Fish'] 

        # Stock and Flow Variables
        dFisheries      = FishRepl - FishExtract  
        dFoodStock_l    = FoodProd_l - FoodCons_l - FoodWaste_l 
        dFoodDemandPC_r   = FoodIncomeElasticity_r * (ChangeEconOutputPC_r/PrevEconOutputPC_r) * 
                                PrevFoodDemandPC_r 
        dGrazeLand      = GrazeLandGain - GrazeLandLoss
        dCropLand       = CropLandGain - CropLandLoss

        # Output
        list( dFisheries = dFisheries,
              dFoodStock_l = dFoodStock_l,
              dFoodDemandPC_r = dFoodDemandPC_r,
              dGrazeLand = dGrazeLand,
              dCropLand = dCropLand,
              NutritionConsPC_kr = NutritionConsPC_kr,
              AgriWaterDemand = AgriWaterDemand)
   	})
}
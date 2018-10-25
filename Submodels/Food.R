########################                       ########################
########################    FOOD SUBSYSTEM     ########################
########################                       ########################


Food = function(
    FoodProd_l,
    Fisheries,
    FoodDemandPC_r,
    GrazeLand,    
    CropLand,
    GlobalTemp,
    RegPop_r,
    EconOutputPC_r,
    PrevEconOutputPC_r,
    PrevFoodDemandPC_r,
    Freshwater,
    parms) 
{
    with(as.list(c(parms)), {        

        # Input Conversion
        FoodNutritionCoversion_l = c(
            Fish = FoodNutrConv_Fish,
            Livestock = FoodNutrConv_Livestock,
            Crops = FoodNutrConv_Crops)
        FoodConsumptionFrac_l = c(
            Fish = FoodConsFrac_Fish,
            Livestock = FoodConsFrac_Livestock,
            Crops = FoodConsFrac_Crops)
        ProdDelay_l = c(
            Fish = FishProdDelay,
            Livestock = LivestockProdDelay,
            Crops = CropsProdDelay)
        AdjDelay_l = c(
            Fish = FishAdjDelay,
            Livestock = LivestockAdjDelay,
            Crops = CropsAdjDelay)
        FoodWasteFrac_l = c(
            Fish = FishWasteFrac,
            Livestock = LivestockWasteFrac,
            Crops = CropsWasteFrac)         
        FoodAccess_krl =  array(c(
            Fish = cbind(   
                Low = c(
                    Rich = FishAccess_RichLow,
                    Poor = FishAccess_PoorLow),
                Mid = c(
                    Rich = FishAccess_RichMid,
                    Poor = FishAccess_PoorMid),
                High = c(
                    Rich = FishAccess_RichHigh,
                    Poor = FishAccess_PoorHigh)),
            Livestock = cbind(   
                Low = c(
                    Rich = LivestockAccess_RichLow,
                    Poor = LivestockAccess_PoorLow),
                Mid = c(
                    Rich = LivestockAccess_RichMid,
                    Poor = LivestockAccess_PoorMid),
                High = c(
                    Rich = LivestockAccess_RichHigh,
                    Poor = LivestockAccess_PoorHigh)),
            Crops = cbind(   
                Low = c(
                    Rich = CropsAccess_RichLow,
                    Poor = CropsAccess_PoorLow),
                Mid = c(
                    Rich = CropsAccess_RichMid,
                    Poor = CropsAccess_PoorMid),
                High = c(
                    Rich = CropsAccess_RichHigh,
                    Poor = CropsAccess_PoorHigh))),
            dim = c(2,3,3),
            dimnames = list(c('Rich','Poor'),
                            c('Low','Mid','High'),
                            c('Fish','Livestock','Crops'))
            )

        FoodIncomeElasticity_r = c(
            Low = FoodIncomeElasticity_Low,
            Mid = FoodIncomeElasticity_Mid,
            High = FoodIncomeElasticity_High)
        FoodNutrConv_l = c(
            Fish = FoodNutrConv_Fish,
            Livestock = FoodNutrConv_Livestock,
            Crops = FoodNutrConv_Crops)

        # Auxiliary Variables

        FoodDemand = sum(RegPop_r * FoodDemandPC_r)
        ChangeEconOutputPC_r = EconOutputPC_r - PrevEconOutputPC_r
        logFishProdCap = log(FishingTech) + ZetaU * log(Fisheries)  
        logLivestockProdCap = log(LivestockTechMult)  + 
            LandProdElastLivestock * log(GrazeLand) +  
        	WaterProdElastLivestock * log(Freshwater * (1 - WaterCropFrac)) 
        logCropsProdCap = log(CropsTechMult)  +
            LandProdElastCrops * log(CropLand) + 
            WaterProdEastCrops * log(Freshwater * WaterCropFrac)  
        FoodProdCap_l = c(
            Fish = exp(logFishProdCap),
            Livestock = exp(logLivestockProdCap),
            Crops = exp(logCropsProdCap))
        TargetFood_l = FoodConsumptionFrac_l * FoodDemand
        AgriWaterDemand = FoodProd_l['Crops'] * CropsWaterConsRate + 
            FoodProd_l['Livestock'] * LivestockWaterConsRate
        FoodAdj_l = pmin((TargetFood_l - FoodProd_l)/ProdDelay_l,(FoodProdCap_l-FoodProd_l)/AdjDelay_l)
        FoodWaste_l = pmin(FoodWasteFrac_l * FoodProd_l,FoodProd_l)
        FoodCons_l = FoodProd_l - FoodWaste_l
        names(FoodCons_l) = c('Fish','Livestock','Crops')
        GrazeLandGain = GrazeLandGrowthRate * GrazeLand
        GrazeLandLoss = GrazeLandLossRate * GrazeLand
        CropLandGain = CropLandGrowthRate * CropLand
        CropLandLoss = CropLandLossRate * CropLand
        RegPop_kr = cbind(
            Low = c(
                Rich  = (1- PoorFrac) * RegPop_r['Low'],
                Poor  = PoorFrac * RegPop_r['Low']),
            Mid = c(
                Rich  = (1- PoorFrac) * RegPop_r['Mid'],
                Poor  = PoorFrac * RegPop_r['Mid']),
            High = c(
                Rich  = (1- PoorFrac) * RegPop_r['High'],
                Poor  = PoorFrac * RegPop_r['High'])
            )
        FoodConsPC_krl = sapply(c('Fish','Livestock','Crops'), function(x) {
            (FoodCons_l[x] * FoodAccess_krl[,,x]) / (RegPop_kr * 1000) },
            simplify = 'array')
        NutritionConsPC_krl = sapply(c('Fish','Livestock','Crops'),function(x) {
            log(FoodConsPC_krl[,,x]) * FoodNutrConv_l[x]},
            simplify = 'array') 
        NutritionConsPC_kr = FoodNutrConvMultiplier + 
                                NutritionConsPC_krl[,,'Fish'] + 
                                NutritionConsPC_krl[,,'Livestock'] +  
                                NutritionConsPC_krl[,,'Crops']
        FishRepl = ThetaU * Fisheries 
        FishExtract = FoodProd_l['Fish'] 

        # Stock and Flow Variables
        dFisheries      = FishRepl - FishExtract  
        dFoodProd_l     = FoodAdj_l 
        dFoodDemandPC_r   = FoodIncomeElasticity_r * (ChangeEconOutputPC_r/PrevEconOutputPC_r) * 
                                PrevFoodDemandPC_r 
        dGrazeLand      = GrazeLandGain - GrazeLandLoss
        dCropLand       = CropLandGain - CropLandLoss

        # Output
        list( dFisheries = dFisheries,
              dFoodProd_l = dFoodProd_l,
              dFoodDemandPC_r = dFoodDemandPC_r,
              dGrazeLand = dGrazeLand,
              dCropLand = dCropLand,
              NutritionConsPC_kr = NutritionConsPC_kr,
              FoodWaste_l = FoodWaste_l,
              FoodCons_l = FoodCons_l,
              AgriWaterDemand = AgriWaterDemand)
   	})
}
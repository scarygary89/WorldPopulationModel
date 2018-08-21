########################                       ########################
########################    FOOD SUBSYSTEM     ########################
########################                       ########################


Food = function(
    FoodStock_l,
    Fisheries,
    FoodDemandPC_r,
    GrazeLand,    
    CropLand,
    GlobalTemp,
    RegPop_ijkr,
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
        FishProdCap     = Fisheries * ZetaU
        LivestockProdCap= LivestockTechMult * GrazeLand ^ LandProdElastLivestock * 
        	(Freshwater * (1 - WaterCropFrac)) ^ WaterProdElastLivestock
        CropsProdCap    = CropsTechMult *CropLand ^ LandProdElastCrops * 
                            (Freshwater * WaterCropFrac) ^ WaterProdEastCrops
        FoodProdCap_l = c(
            Fish = FishProdCap,
            Livestock = LivestockProdCap,
            Crops = CropsProdCap)
        TargetFood_l = FoodConsumptionFrac_l * FoodDemand
        AgriWaterDemand = (FoodStock_l['Crops'] / 
            (CropsTechMult*CropLand^LandProdElastCrops))^(1/WaterProdEastCrops) + 
            (FoodStock_l['Livestock']/(LivestockTechMult*GrazeLand^LandProdElastLivestock)) ^ 
            (1/WaterProdElastLivestock)
        FoodCons_l = pmin(TargetFood_l,FoodStock_l)
        names(FoodCons_l) = c('Fish','Livestock','Crops')
        FoodProd_l = pmax(pmin((TargetFood_l - FoodStock_l)/ProdDelay_l,FoodProdCap_l),MinFoodProd)
        FoodWaste_l = FoodWasteFrac_l * FoodStock_l

        GrazeLandGain = GrazeLandGrowthRate * GrazeLand
        GrazeLandLoss = GrazeLandLossRate * GrazeLand
        CropLandGain = CropLandGrowthRate * CropLand
        CropLandLoss = CropLandLossRate * CropLand
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
        FoodConsPC_krl = sapply(c('Fish','Livestock','Crops'), function(x) {
            (FoodCons_l[x] * FoodAccess_krl[,,x]) / RegPop_kr },
            simplify = 'array')
        NutritionConsPC_kr = sapply(c('Fish','Livestock','Crops'),function(x) {
            FoodConsPC_krl[,,x] ^ FoodNutrConv_l[x]},
            simplify = 'array') 
        NutritionConsPC_kr = FoodNutrConvMultiplier * NutritionConsPC_kr[,,'Fish'] * 
                                NutritionConsPC_kr[,,'Livestock'] *  NutritionConsPC_kr[,,'Crops']
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
              FoodProd_l  = FoodProd_l,
              FoodWaste_l = FoodWaste_l,
              FoodCons_l = FoodCons_l,
              AgriWaterDemand = AgriWaterDemand)
   	})
}
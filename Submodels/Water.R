########################                       ########################
########################    WATER SUBSYSTEM    ########################
########################                       ########################

Water = function(
    Freshwater,
    TempAnamoly,
    EconOutput_r,
    AgriWaterDemand,
    RegPop_r,
    parms) 
{
  with(as.list(c(parms)), {
    # Assemble Input
    WaterDemandPC_r = c(
        Low = WaterDemandPC_Low,
        Mid = WaterDemandPC_Mid,
        High = WaterDemandPC_High)

    ZetaI1_r = c(
        Low = ZetaI1_Low,
        Mid = ZetaI1_Mid,
        High = ZetaI1_High)
    
    ZetaI2_r = c(
        Low = ZetaI2_Low,
        Mid = ZetaI2_Mid,
        High = ZetaI2_High)

    # Auxiliary Variables
    ClimateChangeWaterLossRate = DeltaW * TempAnamoly
    ClimateChangeWaterLoss = Freshwater * ClimateChangeWaterLossRate
    WaterRepl = max((WaterReplMax - Freshwater)/WaterReplDelay, 0)
    MunWaterDemand = sum(WaterDemandPC_r * RegPop_r)
    IndWaterDemand = sum(ZetaI1_r + ZetaI2_r * EconOutput_r)
    WaterDemand = AgriWaterDemand + MunWaterDemand + IndWaterDemand
    WaterCons = min(WaterDemand,Freshwater * (1 - ClimateChangeWaterLossRate))

    # Stock and Flow Variables
    dFreshwater = WaterRepl - ClimateChangeWaterLoss - WaterCons 

    # Output
    list(   dFreshwater = dFreshwater,
            MunWaterDemand = MunWaterDemand,
            IndWaterDemand = IndWaterDemand)

    })
}
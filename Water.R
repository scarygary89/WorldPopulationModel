########################                       ########################
########################    WATER SUBSYSTEM    ########################
########################                       ########################

Water = function(
    Freshwater,
    GlobalTemp,
    EconOutput_r,
    AgriWaterDemand,
    RegPop_r,
    parms) 
{
  with(parms, {
    # Assemble Input
    WaterDemandPC_r = c(
        Low = WaterDemandPC_Low,
        Mid = WaterDemandPC_Mid,
        High = WaterDemandPC_High)

    ZetaI_r = c(
        Low = ZetaI_Low,
        Mid = ZetaI_Mid,
        High = ZetaH_High)

    # Auxiliary Variables
    ClimateChangeWaterLossRate = DeltaW*(GlobalTemp/RefTemp)
    ClimateChangeWaterLoss = Freshwater * ClimateChangeWaterLossRate
    WaterRepl = Freshwater * WaterReplRate
    MunWaterDemand = sum(WaterDemandPC_r * RegPop_r)
    IndWaterDemand = sum(ZetaI_r * EconOutput_r)
    WaterDemand = AgriWaterDemand + MunWaterDemand + IndWaterDemand
    WaterCons = min(WaterDemand,Freshwater * (1 - ClimateChangeWaterLossRate))

    # Stock and Flow Variables
    dFreshwater = WaterRepl - ClimateChangeWaterLoss - WaterCons 

    # Output
    list( dFreshwater = dFreshwater)

    })

}
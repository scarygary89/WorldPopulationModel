########################                       ########################
########################    WATER SUBSYSTEM    ########################
########################                       ########################

print('-------------   LOAD WATER SUBSYSTEM  -------------------')


Water = function(Freshwater,GlobalTemp,EconOutput,AgriWaterDemand,TotalPop,
  parms) 
{
  with(parms, {
    # Auxiliary Variables
    ClimateChangeWaterLossRate = DeltaW*(GlobalTemp/RefTemp)
    ClimateChangeWaterLoss = Freshwater * ClimateChangeWaterLossRate
    WaterRepl = Freshwater * WaterReplRate
    MunWaterDemand = TotalPop * WaterDemandPC
    IndWaterDemand = ZetaI * EconOutput
    WaterDemand = AgriWaterDemand + MunWaterDemand + IndWaterDemand
    WaterCons = min(WaterDemand,Freshwater * (1 - ClimateChangeWaterLossRate))

    # Stock and Flow Variables
    dFreshwater = WaterRepl - ClimateChangeWaterLoss - WaterCons

    # Output
    list( dFreshwater = dFreshwater)

})

}
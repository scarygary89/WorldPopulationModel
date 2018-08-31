########################                       ########################
########################  CLIMATE SUBSYSTEM    ########################
########################                       ########################



Climate = function(
    CO2Conc,
    RegEconOutputPC,
    RegPop,
    parms) 
{
    with(as.list(c(parms)), {
        # Assemble Inputs
        PsiE1_r = c(     
            Low = PsiE1_Low, 
            Mid = PsiE1_Mid, 
            High = PsiE1_High)
        PsiE2_r = c(     
            Low = PsiE2_Low, 
            Mid = PsiE2_Mid, 
            High = PsiE2_High)

    # Auxiliary Variables
        CO2EmissionPC = PsiE1_r * RegEconOutputPC ^ PsiE2_r
        CO2Emission = sum(RegPop * CO2EmissionPC)
        CO2Storage = Gamma * CO2Conc
        CO2Radiative = 5.35 * CO2Conc / RefCO2Conc
        RadiativeForce = CO2Radiative + OtherRadForce
        TempAnomaly = Lambda * RadiativeForce

    # Stock and Flow Variables
        dCO2Conc = CO2Emission - CO2Storage

    # Output
        list(   dCO2Conc = dCO2Conc,
                TempAnamoly = TempAnomaly,
                CO2EmissionPC = CO2EmissionPC)
    })
}
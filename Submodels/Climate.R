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
        PsiE3_r = c(
            Low = PsiE3_Low,
            Mid = PsiE3_Mid,
            High = PsiE3_High)

    # Auxiliary Variables  
        CO2Emission_r = PsiE1_r + PsiE2_r * log(RegEconOutputPC) + PsiE3_r * log(RegPop)
        CO2Emission = sum(CO2Emission_r)
        CO2Gain = 1 / CO2EmissionConcConv * CO2Emission
        CO2Storage = Gamma * CO2Conc
        CO2Radiative = 5.35 * log(CO2Conc / RefCO2Conc)
        RadiativeForce = CO2Radiative + OtherRadForce
        TempAnomaly = Lambda * RadiativeForce

    # Stock and Flow Variables
        dCO2Conc = CO2Gain - CO2Storage

    # Output
        list(   dCO2Conc = dCO2Conc,
                TempAnamoly = TempAnomaly,
                CO2Emission_r = CO2Emission_r)
    })
}
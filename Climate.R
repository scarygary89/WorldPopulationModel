########################                       ########################
########################  CLIMATE SUBSYSTEM    ########################
########################                       ########################



Climate = function(GlobalTemp,CO2Conc,RegEconOutputPC,RegPop,parms) 
{
    with(parms, {
        # Assemble Inputs
        PsiE_r = c(     
            Low = PsiE_Low, 
            Mid = PsiE_Mid, 
            High = PsiE_High)

    # Auxiliary Variables
        CO2EmissionPC = RegEconOutputPC ^ PsiE_r
        CO2Emission = sum(RegPop * CO2EmissionPC)
        CO2Storage = Gamma * CO2Conc
        CO2Radiative = 5.35 * CO2Conc / RefCO2Conc
        RadiativeForce = CO2Radiative + OtherRadForce
        TempChange = Lambda * RadiativeForce

    # Stock and Flow Variables
        dCO2Conc = CO2Emission - CO2Storage
        dGlobalTemp = TempChange

    # Output
        list(   dCO2Conc = dCO2Conc,
                dGlobalTemp = dGlobalTemp)
    })
}
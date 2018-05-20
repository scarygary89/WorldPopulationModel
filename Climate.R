########################                       ########################
########################  CLIMATE SUBSYSTEM    ########################
########################                       ########################

print('------------- LOAD CLIMATE SUBSYSTEM  -------------------')


Climate = function(GlobalTemp,CO2Conc,EconOutput,Livestock,parms) 
{
    with (parms,{
    # Auxiliary Variables
        CO2Emission = EconOutput^PsiE + Livestock^PsiL
        CO2Storage = Gamma * CO2Conc
        CO2Radiative = 5.35 * CO2Conc / RefCO2Conc
        RadiativeForce = CO2Radiative + OtherRadForce
        TempChange = Lambda * RadiativeForce

    # Stock and Flow Variables
        dCO2Conc = CO2Emission - CO2Storage
        dGlobalTemp = TempChange

    # Output
        list( dCO2Conc = dCO2Conc,
          dGlobalTemp = dGlobalTemp)
    })
}
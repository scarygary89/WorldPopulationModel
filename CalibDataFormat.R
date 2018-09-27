library(reshape)


#LOAD CALIBRATION DATA WORKSPACE

load(file ='./Calibration/CalibrationOutput/EconomyParmsEstimate.RData')
load(file ='./Calibration/CalibrationOutput/ResourceParmsEstimate.RData')
load(file ='./Calibration/CalibrationOutput/ClimateParmsEstimate.RData')
load(file ='./Calibration/CalibrationOutput/FoodParmsEstimate.RData')
load(file ='./Calibration/CalibrationOutput/HealthEducationParmsEstimate.RData')
load(file ='./Calibration/CalibrationOutput/PopulationParmsEstimate.RData')
load(file ='./Calibration/CalibrationOutput/WaterParmsEstimate.RData')

#################  EXTRACT BEST FIT PARAMETERS    #################

# Extract Best Fit Parameter Function
BestParmExtract = function(CalibModResults){
	if(class(CalibModResults) == 'ga' | class(CalibModResults) == 'gaisl')
	{
		FitParm = CalibModResults@solution[1,]
	}
	else
	{
		SSR = sapply(CalibModResults,function(x) x$ssr)
		BestFit = CalibModResults[[which.min(SSR[which(SSR != 0)])]]
		FitParm = coef(BestFit)
	}
	return(FitParm)
}

# Economy Submodel
EconFitParm_Low = BestParmExtract(EconResults_Low)
EconFitParm_Mid = BestParmExtract(EconResults_Mid)
EconFitParm_High = BestParmExtract(EconResults_High)
# Resource Submodel
ResourceFitParm = BestParmExtract(ResourceResults)
# Climate Submodel
ClimateFitParm = BestParmExtract(ClimateResults)
# Food Submodel
FoodFitParm = BestParmExtract(FoodResults)
# Health and Education Submodel
HealthEducationFitParm_Low = BestParmExtract(HealthEducationResults_Low)
HealthEducationFitParm_Mid = BestParmExtract(HealthEducationResults_Mid)
HealthEducationFitParm_High = BestParmExtract(HealthEducationResults_High)
# Population Submodel
PopFitParm_Low = BestParmExtract(PopResults_Low)
PopFitParm_Mid = BestParmExtract(PopResults_Mid)
PopFitParm_High = BestParmExtract(PopResults_High)
# Water Submodel
WaterFitParm = BestParmExtract(WaterResults)

################# ASSEMBLE GLOBAL PARAMETER INPUT #################

# Economy Submodel
LocalFitParameterValue['TechGrowth_Low'] = EconFitParm_Low['TechGrowth']
LocalFitParameterValue['TechGrowth_Mid'] = EconFitParm_Mid['TechGrowth']
LocalFitParameterValue['TechGrowth_Mid'] = EconFitParm_High['TechGrowth']
LocalFitParameterValue['LaborInputElast_Low'] = EconFitParm_Low['LaborInputElast']
LocalFitParameterValue['LaborInputElast_Mid'] = EconFitParm_Mid['LaborInputElast']
LocalFitParameterValue['LaborInputElast_Mid'] = EconFitParm_High['LaborInputElast']
LocalFitParameterValue['CapitalInputElast_Low'] = EconFitParm_Low['CapitalInputElast']
LocalFitParameterValue['CapitalInputElast_Mid'] = EconFitParm_Mid['CapitalInputElast']
LocalFitParameterValue['CapitalInputElast_Mid'] = EconFitParm_High['CapitalInputElast']
LocalFitParameterValue['SavingsRate_Low'] = EconFitParm_Low['SavingsRate']
LocalFitParameterValue['SavingsRate_Mid'] = EconFitParm_Mid['SavingsRate']
LocalFitParameterValue['SavingsRate_High'] = EconFitParm_High['SavingsRate']
LocalFitParameterValue['DeprecRate_Low'] = EconFitParm_Low['DeprecRate']
LocalFitParameterValue['DeprecRate_Mid'] = EconFitParm_Mid['DeprecRate']
LocalFitParameterValue['DeprecRate_High'] = EconFitParm_High['DeprecRate']
LocalFitParameterValue['IneqMult_Low'] = EconFitParm_Low['IneqMult']
LocalFitParameterValue['IneqMult_Mid'] = EconFitParm_Mid['IneqMult']
LocalFitParameterValue['IneqMult_High'] = EconFitParm_High['IneqMult']
LocalFitParameterValue['IneqInt_Low'] = EconFitParm_Low['IneqInt']
LocalFitParameterValue['IneqInt_Mid'] = EconFitParm_Mid['IneqInt']
LocalFitParameterValue['IneqInt_High'] = EconFitParm_High['IneqInt']
LocalFitParameterValue['InitEconOutputGrowth_Low'] = EconFitParm_Low['InitEconOutputGrowth']
LocalFitParameterValue['InitEconOutputGrowth_Mid'] = EconFitParm_Mid['InitEconOutputGrowth']
LocalFitParameterValue['InitEconOutputGrowth_High'] = EconFitParm_High['InitEconOutputGrowth']
LocalFitParameterValue['CoalInputElast_Low'] = EconFitParm_Low['CoalInputElast']
LocalFitParameterValue['CoalInputElast_Mid'] = EconFitParm_Mid['CoalInputElast']
LocalFitParameterValue['CoalInputElast_High'] = EconFitParm_High['CoalInputElast']
LocalFitParameterValue['OilInputElast_Low'] = EconFitParm_Low['OilInputElast']
LocalFitParameterValue['OilInputElast_Mid'] = EconFitParm_Mid['OilInputElast']
LocalFitParameterValue['OilInputElast_High'] = EconFitParm_High['OilInputElast']
LocalFitParameterValue['GasInputElast_Low'] = EconFitParm_Low['GasInputElast']
LocalFitParameterValue['GasInputElast_Mid'] = EconFitParm_Mid['GasInputElast']
LocalFitParameterValue['GasInputElast_High'] = EconFitParm_High['GasInputElast']
LocalFitInitValue['TechMult_Low'] = EconFitParm_Low['TechMult']
LocalFitInitValue['TechMult_Mid'] = EconFitParm_Mid['TechMult']
LocalFitInitValue['TechMult_High'] = EconFitParm_High['TechMult']

# Resource Submodel
LocalFitParameterValue[names(ResourceFitParm)] = ResourceFitParm

# Climate Submodel
LocalFitParameterValue[names(ClimateFitParm)] = ClimateFitParm

# Food Submodel
LocalFitParameterValue[names(FoodFitParm)] = FoodFitParm

# Health and Education Submodel
LocalFitParameterValue['ZetaE_Low'] = HealthEducationFitParm_Low['ZetaE']
LocalFitParameterValue['ZetaE_Mid'] = HealthEducationFitParm_Mid['ZetaE']
LocalFitParameterValue['ZetaE_High'] = HealthEducationFitParm_High['ZetaE']
LocalFitParameterValue['ZetaH_Low'] = HealthEducationFitParm_Low['ZetaH']
LocalFitParameterValue['ZetaH_Mid'] = HealthEducationFitParm_Mid['ZetaH']
LocalFitParameterValue['ZetaH_High'] = HealthEducationFitParm_High['ZetaH']
LocalFitParameterValue['LambdaE_Low'] = HealthEducationFitParm_Low['LambdaE']
LocalFitParameterValue['LambdaE_Mid'] = HealthEducationFitParm_Mid['LambdaE']
LocalFitParameterValue['LambdaE_High'] = HealthEducationFitParm_High['LambdaE']
LocalFitParameterValue['LambdaH_Low'] = HealthEducationFitParm_Low['LambdaH']
LocalFitParameterValue['LambdaH_Mid'] = HealthEducationFitParm_Mid['LambdaH']
LocalFitParameterValue['LambdaH_High'] = HealthEducationFitParm_High['LambdaH']
LocalFitParameterValue['ChiEF1_Low'] = HealthEducationFitParm_Low['ChiEF1']
LocalFitParameterValue['ChiEF1_Mid'] = HealthEducationFitParm_Mid['ChiEF1']
LocalFitParameterValue['ChiEF1_High'] = HealthEducationFitParm_High['ChiEF1']
LocalFitParameterValue['ChiHF1_Low'] = HealthEducationFitParm_Low['ChiHF1']
LocalFitParameterValue['ChiHF1_Mid'] = HealthEducationFitParm_Mid['ChiHF1']
LocalFitParameterValue['ChiHF1_High'] = HealthEducationFitParm_High['ChiHF1']
LocalFitParameterValue['ChiHA1_RichLow'] = HealthEducationFitParm_Low['ChiHA1_Rich']
LocalFitParameterValue['ChiHA1_RichMid'] = HealthEducationFitParm_Mid['ChiHA1_Rich']
LocalFitParameterValue['ChiHA1_RichHigh'] = HealthEducationFitParm_High['ChiHA1_Rich']
LocalFitParameterValue['ChiHA1_PoorLow'] = HealthEducationFitParm_Low['ChiHA1_Poor']
LocalFitParameterValue['ChiHA1_PoorMid'] = HealthEducationFitParm_Mid['ChiHA1_Poor']
LocalFitParameterValue['ChiHA1_PoorHigh'] = HealthEducationFitParm_High['ChiHA1_Poor']
LocalFitParameterValue['ChiEF2_Low'] = HealthEducationFitParm_Low['ChiEF2']
LocalFitParameterValue['ChiEF2_Mid'] = HealthEducationFitParm_Mid['ChiEF2']
LocalFitParameterValue['ChiEF2_High'] = HealthEducationFitParm_High['ChiEF2']
LocalFitParameterValue['ChiHF2_Low'] = HealthEducationFitParm_Low['ChiHF2']
LocalFitParameterValue['ChiHF2_Mid'] = HealthEducationFitParm_Mid['ChiHF2']
LocalFitParameterValue['ChiHF2_High'] = HealthEducationFitParm_High['ChiHF2']
LocalFitParameterValue['ChiHA2_RichLow'] = HealthEducationFitParm_Low['ChiHA2_Rich']
LocalFitParameterValue['ChiHA2_RichMid'] = HealthEducationFitParm_Mid['ChiHA2_Rich']
LocalFitParameterValue['ChiHA2_RichHigh'] = HealthEducationFitParm_High['ChiHA2_Rich']
LocalFitParameterValue['ChiHA2_PoorLow'] = HealthEducationFitParm_Low['ChiHA2_Poor']
LocalFitParameterValue['ChiHA2_PoorMid'] = HealthEducationFitParm_Mid['ChiHA2_Poor']
LocalFitParameterValue['ChiHA2_PoorHigh'] = HealthEducationFitParm_High['ChiHA2_Poor']
LocalFitParameterValue['ChiHA3_RichLow'] = HealthEducationFitParm_Low['ChiHA3_Rich']
LocalFitParameterValue['ChiHA3_RichMid'] = HealthEducationFitParm_Mid['ChiHA3_Rich']
LocalFitParameterValue['ChiHA3_RichHigh'] = HealthEducationFitParm_High['ChiHA3_Rich']
LocalFitParameterValue['ChiHA3_PoorLow'] = HealthEducationFitParm_Low['ChiHA3_Poor']
LocalFitParameterValue['ChiHA3_PoorMid'] = HealthEducationFitParm_Mid['ChiHA3_Poor']
LocalFitParameterValue['ChiHA3_PoorHigh'] = HealthEducationFitParm_High['ChiHA3_Poor']

# Population Submodel
LocalFitParameterValue['MinDeath_RichM1Low'] = PopFitParm_Low['MinDeath_RichM1']
LocalFitParameterValue['MinDeath_PoorM1Low'] = PopFitParm_Low['MinDeath_PoorM1']
LocalFitParameterValue['MinDeath_M2Low'] = PopFitParm_Low['MinDeath_M2']
LocalFitParameterValue['MinDeath_M3Low'] = PopFitParm_Low['MinDeath_M3']
LocalFitParameterValue['MinDeath_M4Low'] = PopFitParm_Low['MinDeath_M4']
LocalFitParameterValue['MinDeath_RichF1Low'] = PopFitParm_Low['MinDeath_RichF1']
LocalFitParameterValue['MinDeath_PoorF1Low'] = PopFitParm_Low['MinDeath_PoorF1']
LocalFitParameterValue['MinDeath_F2Low'] = PopFitParm_Low['MinDeath_F2']
LocalFitParameterValue['MinDeath_F3Low'] = PopFitParm_Low['MinDeath_F3']
LocalFitParameterValue['MinDeath_F4Low'] = PopFitParm_Low['MinDeath_F4']
LocalFitParameterValue['OmegaH_RichM1Low'] = PopFitParm_Low['OmegaH_RichM1']
LocalFitParameterValue['OmegaH_PoorM1Low'] = PopFitParm_Low['OmegaH_PoorM1']
LocalFitParameterValue['OmegaH_M2Low'] = PopFitParm_Low['OmegaH_M2']
LocalFitParameterValue['OmegaH_M3Low'] = PopFitParm_Low['OmegaH_M3']
LocalFitParameterValue['OmegaH_M4Low'] = PopFitParm_Low['OmegaH_M4']
LocalFitParameterValue['OmegaH_RichF1Low'] = PopFitParm_Low['OmegaH_RichF1']
LocalFitParameterValue['OmegaH_PoorF1Low'] = PopFitParm_Low['OmegaH_PoorF1']
LocalFitParameterValue['OmegaH_F2Low'] = PopFitParm_Low['OmegaH_F2']
LocalFitParameterValue['OmegaH_F3Low'] = PopFitParm_Low['OmegaH_F3']
LocalFitParameterValue['OmegaH_F4Low'] = PopFitParm_Low['OmegaH_F4']
LocalFitParameterValue['OmegaF_RichM1Low'] = PopFitParm_Low['OmegaF_RichM1']
LocalFitParameterValue['OmegaF_PoorM1Low'] = PopFitParm_Low['OmegaF_PoorM1']
LocalFitParameterValue['OmegaF_M2Low'] = PopFitParm_Low['OmegaF_M2']
LocalFitParameterValue['OmegaF_M3Low'] = PopFitParm_Low['OmegaF_M3']
LocalFitParameterValue['OmegaF_M4Low'] = PopFitParm_Low['OmegaF_M4']
LocalFitParameterValue['OmegaF_RichF1Low'] = PopFitParm_Low['OmegaF_RichF1']
LocalFitParameterValue['OmegaF_PoorF1Low'] = PopFitParm_Low['OmegaF_PoorF1']
LocalFitParameterValue['OmegaF_F2Low'] = PopFitParm_Low['OmegaF_F2']
LocalFitParameterValue['OmegaF_F3Low'] = PopFitParm_Low['OmegaF_F3']
LocalFitParameterValue['OmegaF_F4Low'] = PopFitParm_Low['OmegaF_F4']
LocalFitParameterValue['AlphaGFR_Low'] = PopFitParm_Low['AlphaGFR']
LocalFitParameterValue['BetaE_Low'] = PopFitParm_Low['BetaE']
LocalFitParameterValue['BetaH_Low'] = PopFitParm_Low['BetaH']

LocalFitParameterValue['MinDeath_RichM1Mid'] = PopFitParm_Mid['MinDeath_RichM1']
LocalFitParameterValue['MinDeath_PoorM1Mid'] = PopFitParm_Mid['MinDeath_PoorM1']
LocalFitParameterValue['MinDeath_M2Mid'] = PopFitParm_Mid['MinDeath_M2']
LocalFitParameterValue['MinDeath_M3Mid'] = PopFitParm_Mid['MinDeath_M3']
LocalFitParameterValue['MinDeath_M4Mid'] = PopFitParm_Mid['MinDeath_M4']
LocalFitParameterValue['MinDeath_RichF1Mid'] = PopFitParm_Mid['MinDeath_RichF1']
LocalFitParameterValue['MinDeath_PoorF1Mid'] = PopFitParm_Mid['MinDeath_PoorF1']
LocalFitParameterValue['MinDeath_F2Mid'] = PopFitParm_Mid['MinDeath_F2']
LocalFitParameterValue['MinDeath_F3Mid'] = PopFitParm_Mid['MinDeath_F3']
LocalFitParameterValue['MinDeath_F4Mid'] = PopFitParm_Mid['MinDeath_F4']
LocalFitParameterValue['OmegaH_RichM1Mid'] = PopFitParm_Mid['OmegaH_RichM1']
LocalFitParameterValue['OmegaH_PoorM1Mid'] = PopFitParm_Mid['OmegaH_PoorM1']
LocalFitParameterValue['OmegaH_M2Mid'] = PopFitParm_Mid['OmegaH_M2']
LocalFitParameterValue['OmegaH_M3Mid'] = PopFitParm_Mid['OmegaH_M3']
LocalFitParameterValue['OmegaH_M4Mid'] = PopFitParm_Mid['OmegaH_M4']
LocalFitParameterValue['OmegaH_RichF1Mid'] = PopFitParm_Mid['OmegaH_RichF1']
LocalFitParameterValue['OmegaH_PoorF1Mid'] = PopFitParm_Mid['OmegaH_PoorF1']
LocalFitParameterValue['OmegaH_F2Mid'] = PopFitParm_Mid['OmegaH_F2']
LocalFitParameterValue['OmegaH_F3Mid'] = PopFitParm_Mid['OmegaH_F3']
LocalFitParameterValue['OmegaH_F4Mid'] = PopFitParm_Mid['OmegaH_F4']
LocalFitParameterValue['OmegaF_RichM1Mid'] = PopFitParm_Mid['OmegaF_RichM1']
LocalFitParameterValue['OmegaF_PoorM1Mid'] = PopFitParm_Mid['OmegaF_PoorM1']
LocalFitParameterValue['OmegaF_M2Mid'] = PopFitParm_Mid['OmegaF_M2']
LocalFitParameterValue['OmegaF_M3Mid'] = PopFitParm_Mid['OmegaF_M3']
LocalFitParameterValue['OmegaF_M4Mid'] = PopFitParm_Mid['OmegaF_M4']
LocalFitParameterValue['OmegaF_RichF1Mid'] = PopFitParm_Mid['OmegaF_RichF1']
LocalFitParameterValue['OmegaF_PoorF1Mid'] = PopFitParm_Mid['OmegaF_PoorF1']
LocalFitParameterValue['OmegaF_F2Mid'] = PopFitParm_Mid['OmegaF_F2']
LocalFitParameterValue['OmegaF_F3Mid'] = PopFitParm_Mid['OmegaF_F3']
LocalFitParameterValue['OmegaF_F4Mid'] = PopFitParm_Mid['OmegaF_F4']
LocalFitParameterValue['AlphaGFR_Mid'] = PopFitParm_Mid['AlphaGFR']
LocalFitParameterValue['BetaE_Mid'] = PopFitParm_Mid['BetaE']
LocalFitParameterValue['BetaH_Mid'] = PopFitParm_Mid['BetaH']

LocalFitParameterValue['MinDeath_RichM1High'] = PopFitParm_High['MinDeath_RichM1']
LocalFitParameterValue['MinDeath_PoorM1High'] = PopFitParm_High['MinDeath_PoorM1']
LocalFitParameterValue['MinDeath_M2High'] = PopFitParm_High['MinDeath_M2']
LocalFitParameterValue['MinDeath_M3High'] = PopFitParm_High['MinDeath_M3']
LocalFitParameterValue['MinDeath_M4High'] = PopFitParm_High['MinDeath_M4']
LocalFitParameterValue['MinDeath_RichF1High'] = PopFitParm_High['MinDeath_RichF1']
LocalFitParameterValue['MinDeath_PoorF1High'] = PopFitParm_High['MinDeath_PoorF1']
LocalFitParameterValue['MinDeath_F2High'] = PopFitParm_High['MinDeath_F2']
LocalFitParameterValue['MinDeath_F3High'] = PopFitParm_High['MinDeath_F3']
LocalFitParameterValue['MinDeath_F4High'] = PopFitParm_High['MinDeath_F4']
LocalFitParameterValue['OmegaH_RichM1High'] = PopFitParm_High['OmegaH_RichM1']
LocalFitParameterValue['OmegaH_PoorM1High'] = PopFitParm_High['OmegaH_PoorM1']
LocalFitParameterValue['OmegaH_M2High'] = PopFitParm_High['OmegaH_M2']
LocalFitParameterValue['OmegaH_M3High'] = PopFitParm_High['OmegaH_M3']
LocalFitParameterValue['OmegaH_M4High'] = PopFitParm_High['OmegaH_M4']
LocalFitParameterValue['OmegaH_RichF1High'] = PopFitParm_High['OmegaH_RichF1']
LocalFitParameterValue['OmegaH_PoorF1High'] = PopFitParm_High['OmegaH_PoorF1']
LocalFitParameterValue['OmegaH_F2High'] = PopFitParm_High['OmegaH_F2']
LocalFitParameterValue['OmegaH_F3High'] = PopFitParm_High['OmegaH_F3']
LocalFitParameterValue['OmegaH_F4High'] = PopFitParm_High['OmegaH_F4']
LocalFitParameterValue['OmegaF_RichM1High'] = PopFitParm_High['OmegaF_RichM1']
LocalFitParameterValue['OmegaF_PoorM1High'] = PopFitParm_High['OmegaF_PoorM1']
LocalFitParameterValue['OmegaF_M2High'] = PopFitParm_High['OmegaF_M2']
LocalFitParameterValue['OmegaF_M3High'] = PopFitParm_High['OmegaF_M3']
LocalFitParameterValue['OmegaF_M4High'] = PopFitParm_High['OmegaF_M4']
LocalFitParameterValue['OmegaF_RichF1High'] = PopFitParm_High['OmegaF_RichF1']
LocalFitParameterValue['OmegaF_PoorF1High'] = PopFitParm_High['OmegaF_PoorF1']
LocalFitParameterValue['OmegaF_F2High'] = PopFitParm_High['OmegaF_F2']
LocalFitParameterValue['OmegaF_F3High'] = PopFitParm_High['OmegaF_F3']
LocalFitParameterValue['OmegaF_F4High'] = PopFitParm_High['OmegaF_F4']
LocalFitParameterValue['AlphaGFR_High'] = PopFitParm_High['AlphaGFR']
LocalFitParameterValue['BetaE_High'] = PopFitParm_High['BetaE']
LocalFitParameterValue['BetaH_High'] = PopFitParm_High['BetaH']

# Water Submodel
LocalFitParameterValue[names(WaterFitParm)] = WaterFitParm
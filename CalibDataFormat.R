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
	SSR = sapply(CalibModResults,function(x) x$ssr)
	BestFit = CalibModResults[[which.min(SSR[which(SSR != 0)])]]
	FitParm = coef(BestFit)
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
PopFitParm = BestParmExtract(PopResults)
# Water Submodel
WaterFitParm = BestParmExtract(WaterResults)

################# ASSEMBLE GLOBAL PARAMETER INPUT #################

LocalFitParmameterValue = ParameterValue
LocalFitInitValue = InitValue

# Economy Submodel
LocalFitParmameterValue['TechGrowth_Low'] = EconFitParm_Low['TechGrowth']
LocalFitParmameterValue['TechGrowth_Mid'] = EconFitParm_Mid['TechGrowth']
LocalFitParmameterValue['TechGrowth_Mid'] = EconFitParm_High['TechGrowth']
LocalFitParmameterValue['LaborInputElast_Low'] = EconFitParm_Low['LaborInputElast']
LocalFitParmameterValue['LaborInputElast_Mid'] = EconFitParm_Mid['LaborInputElast']
LocalFitParmameterValue['LaborInputElast_Mid'] = EconFitParm_High['LaborInputElast']
LocalFitParmameterValue['CapitalInputElast_Low'] = EconFitParm_Low['CapitalInputElast']
LocalFitParmameterValue['CapitalInputElast_Mid'] = EconFitParm_Mid['CapitalInputElast']
LocalFitParmameterValue['CapitalInputElast_Mid'] = EconFitParm_High['CapitalInputElast']
LocalFitParmameterValue['SavingsRate_Low'] = EconFitParm_Low['SavingsRate']
LocalFitParmameterValue['SavingsRate_Mid'] = EconFitParm_Mid['SavingsRate']
LocalFitParmameterValue['SavingsRate_High'] = EconFitParm_High['SavingsRate']
LocalFitParmameterValue['DeprecRate_Low'] = EconFitParm_Low['DeprecRate']
LocalFitParmameterValue['DeprecRate_Mid'] = EconFitParm_Mid['DeprecRate']
LocalFitParmameterValue['DeprecRate_High'] = EconFitParm_High['DeprecRate']
LocalFitParmameterValue['IneqMult_Low'] = EconFitParm_Low['IneqMult']
LocalFitParmameterValue['IneqMult_Mid'] = EconFitParm_Mid['IneqMult']
LocalFitParmameterValue['IneqMult_High'] = EconFitParm_High['IneqMult']
LocalFitParmameterValue['IneqInt_Low'] = EconFitParm_Low['IneqInt']
LocalFitParmameterValue['IneqInt_Mid'] = EconFitParm_Mid['IneqInt']
LocalFitParmameterValue['IneqInt_High'] = EconFitParm_High['IneqInt']
LocalFitParmameterValue['InitEconOutputGrowth_Low'] = EconFitParm_Low['InitEconOutputGrowth']
LocalFitParmameterValue['InitEconOutputGrowth_Mid'] = EconFitParm_Mid['InitEconOutputGrowth']
LocalFitParmameterValue['InitEconOutputGrowth_High'] = EconFitParm_High['InitEconOutputGrowth']
LocalFitParmameterValue['CoalInputElast_Low'] = EconFitParm_Low['CoalInputElast']
LocalFitParmameterValue['CoalInputElast_Mid'] = EconFitParm_Mid['CoalInputElast']
LocalFitParmameterValue['CoalInputElast_High'] = EconFitParm_High['CoalInputElast']
LocalFitParmameterValue['OilInputElast_Low'] = EconFitParm_Low['OilInputElast']
LocalFitParmameterValue['OilInputElast_Mid'] = EconFitParm_Mid['OilInputElast']
LocalFitParmameterValue['OilInputElast_High'] = EconFitParm_High['OilInputElast']
LocalFitParmameterValue['GasInputElast_Low'] = EconFitParm_Low['GasInputElast']
LocalFitParmameterValue['GasInputElast_Mid'] = EconFitParm_Mid['GasInputElast']
LocalFitParmameterValue['GasInputElast_High'] = EconFitParm_High['GasInputElast']
LocalFitInitValue['TechMult_Low'] = EconFitParm_Low['TechMult']
LocalFitInitValue['TechMult_Mid'] = EconFitParm_Mid['TechMult']
LocalFitInitValue['TechMult_High'] = EconFitParm_High['TechMult']

# Resource Submodel
LocalFitParmameterValue[names(ResourceFitParm)] = ResourceFitParm

# Climate Submodel
LocalFitParmameterValue[names(ClimateFitParm)] = ClimateFitParm

# Food Submodel
LocalFitParmameterValue[names(FoodFitParm)] = FoodFitParm

# Health and Education Submodel
LocalFitParmameterValue['ZetaE_Low'] = HealthEducationFitParm_Low['ZetaE']
LocalFitParmameterValue['ZetaE_Mid'] = HealthEducationFitParm_Mid['ZetaE']
LocalFitParmameterValue['ZetaE_High'] = HealthEducationFitParm_High['ZetaE']
LocalFitParmameterValue['ZetaH_Low'] = HealthEducationFitParm_Low['ZetaH']
LocalFitParmameterValue['ZetaH_Mid'] = HealthEducationFitParm_Mid['ZetaH']
LocalFitParmameterValue['ZetaH_High'] = HealthEducationFitParm_High['ZetaH']
LocalFitParmameterValue['LambdaE_Low'] = HealthEducationFitParm_Low['LambdaE']
LocalFitParmameterValue['LambdaE_Mid'] = HealthEducationFitParm_Mid['LambdaE']
LocalFitParmameterValue['LambdaE_High'] = HealthEducationFitParm_High['LambdaE']
LocalFitParmameterValue['LambdaH_Low'] = HealthEducationFitParm_Low['LambdaH']
LocalFitParmameterValue['LambdaH_Mid'] = HealthEducationFitParm_Mid['LambdaH']
LocalFitParmameterValue['LambdaH_High'] = HealthEducationFitParm_High['LambdaH']
LocalFitParmameterValue['ChiEF1_Low'] = HealthEducationFitParm_Low['ChiEF1']
LocalFitParmameterValue['ChiEF1_Mid'] = HealthEducationFitParm_Mid['ChiEF1']
LocalFitParmameterValue['ChiEF1_High'] = HealthEducationFitParm_High['ChiEF1']
LocalFitParmameterValue['ChiHF1_Low'] = HealthEducationFitParm_Low['ChiHF1']
LocalFitParmameterValue['ChiHF1_Mid'] = HealthEducationFitParm_Mid['ChiHF1']
LocalFitParmameterValue['ChiHF1_High'] = HealthEducationFitParm_High['ChiHF1']
LocalFitParmameterValue['ChiHA1_RichLow'] = HealthEducationFitParm_Low['ChiHA1_Rich']
LocalFitParmameterValue['ChiHA1_RichMid'] = HealthEducationFitParm_Mid['ChiHA1_Rich']
LocalFitParmameterValue['ChiHA1_RichHigh'] = HealthEducationFitParm_High['ChiHA1_Rich']
LocalFitParmameterValue['ChiHA1_PoorLow'] = HealthEducationFitParm_Low['ChiHA1_Poor']
LocalFitParmameterValue['ChiHA1_PoorMid'] = HealthEducationFitParm_Mid['ChiHA1_Poor']
LocalFitParmameterValue['ChiHA1_PoorHigh'] = HealthEducationFitParm_High['ChiHA1_Poor']
LocalFitParmameterValue['ChiEF2_Low'] = HealthEducationFitParm_Low['ChiEF2']
LocalFitParmameterValue['ChiEF2_Mid'] = HealthEducationFitParm_Mid['ChiEF2']
LocalFitParmameterValue['ChiEF2_High'] = HealthEducationFitParm_High['ChiEF2']
LocalFitParmameterValue['ChiHF2_Low'] = HealthEducationFitParm_Low['ChiHF2']
LocalFitParmameterValue['ChiHF2_Mid'] = HealthEducationFitParm_Mid['ChiHF2']
LocalFitParmameterValue['ChiHF2_High'] = HealthEducationFitParm_High['ChiHF2']
LocalFitParmameterValue['ChiHA2_RichLow'] = HealthEducationFitParm_Low['ChiHA2_Rich']
LocalFitParmameterValue['ChiHA2_RichMid'] = HealthEducationFitParm_Mid['ChiHA2_Rich']
LocalFitParmameterValue['ChiHA2_RichHigh'] = HealthEducationFitParm_High['ChiHA2_Rich']
LocalFitParmameterValue['ChiHA2_PoorLow'] = HealthEducationFitParm_Low['ChiHA2_Poor']
LocalFitParmameterValue['ChiHA2_PoorMid'] = HealthEducationFitParm_Mid['ChiHA2_Poor']
LocalFitParmameterValue['ChiHA2_PoorHigh'] = HealthEducationFitParm_High['ChiHA2_Poor']
LocalFitParmameterValue['ChiHA3_RichLow'] = HealthEducationFitParm_Low['ChiHA3_Rich']
LocalFitParmameterValue['ChiHA3_RichMid'] = HealthEducationFitParm_Mid['ChiHA3_Rich']
LocalFitParmameterValue['ChiHA3_RichHigh'] = HealthEducationFitParm_High['ChiHA3_Rich']
LocalFitParmameterValue['ChiHA3_PoorLow'] = HealthEducationFitParm_Low['ChiHA3_Poor']
LocalFitParmameterValue['ChiHA3_PoorMid'] = HealthEducationFitParm_Mid['ChiHA3_Poor']
LocalFitParmameterValue['ChiHA3_PoorHigh'] = HealthEducationFitParm_High['ChiHA3_Poor']

# Population Submodel
LocalFitParmameterValue[names(PopFitParm)] = PopFitParm

# Water Submodel
LocalFitParmameterValue[names(WaterFitParm)] = WaterFitParm
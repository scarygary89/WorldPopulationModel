print('----------------- CALIBRATE MODEL   ---------------------')

library(FME)

fitvar =  c(
	'Low_RM1',
	'Low_RM2',
	'Low_RM3',
	'Low_RM4',
	'Low_RF1',
	'Low_RF2',
	'Low_RF3',
	'Low_RF4',
	'GFR_Low',
	# 'MortRate_LowM1',
	# 'MortRate_LowM2',
	# 'MortRate_LowM3',
	# 'MortRate_LowM4',
	# 'MortRate_LowF1',
	# 'MortRate_LowF2',
	# 'MortRate_LowF3',
	# 'MortRate_LowF4',
	# 'Migrants_Low',
	# 'Migrants_RichLow',
	# 'Migrants_PoorLow',
	# 'MigRate_Low',
	'EconOutput_Low',
	# 'LowPop',
	# 'HealthCapitalExpenditures_Low',
	# 'HealthServices_Low',
	# 'FemaleAccessHealthFacility_Low',
	# 'FemaleHealthAccess_RichLow',
	# 'FemaleHealthAccess_PoorLow',
	# 'GeneralHealthAccess_RichLow',
	# 'GeneralHealthAccess_PoorLow',
	# 'GovtExpendituresEducation_Low',
	'EducationServices_Low',
	# 'FemaleSecondaryEducation_Low',
	# 'FemaleEduAttain_Low',
	'Capital_Low',
	'Mid_RM1',
	'Mid_RM2',
	'Mid_RM3',
	'Mid_RM4',
	'Mid_RF1',
	'Mid_RF2',
	'Mid_RF3',
	'Mid_RF4',
	'GFR_Mid',
	# 'MortRate_MidM1',
	# 'MortRate_MidM2',
	# 'MortRate_MidM3',
	# 'MortRate_MidM4',
	# 'MortRate_MidF1',
	# 'MortRate_MidF2',
	# 'MortRate_MidF3',
	# 'MortRate_MidF4',
	# 'Migrants_Mid',
	# 'Migrants_RichMid',
	# 'Migrants_PoorMid',
	# 'MigRate_Mid',
	'EconOutput_Mid',
	# 'MidPop',
	# 'HealthCapitalExpenditures_Mid',
	'HealthServices_Mid',
	# 'FemaleAccessHealthFacility_Mid',
	# 'FemaleHealthAccess_RichMid',
	# 'FemaleHealthAccess_PoorMid',
	# 'GeneralHealthAccess_RichMid',
	# 'GeneralHealthAccess_PoorMid',
	# 'GovtExpendituresEducation_Mid',
	'EducationServices_Mid',
	# 'FemaleSecondaryEducation_Mid',
	# 'FemaleEduAttain_Mid',
	'Capital_Mid',
	'High_RM1',
	'High_RM2',
	'High_RM3',
	'High_RM4',
	'High_RF1',
	'High_RF2',
	'High_RF3',
	'High_RF4',
	'GFR_High',
	# 'MortRate_HighM1',
	# 'MortRate_HighM2',
	# 'MortRate_HighM3',
	# 'MortRate_HighM4',
	# 'MortRate_HighF1',
	# 'MortRate_HighF2',
	# 'MortRate_HighF3',
	# 'MortRate_HighF4',
	# 'Migrants_High',
	# 'Migrants_RichHigh',
	# 'Migrants_PoorHigh',
	# 'MigRate_High',
	'EconOutput_High',
	# 'HighPop',
	# 'HealthCapitalExpenditures_High',
	'HealthServices_High',
	# 'FemaleAccessHealthFacility_High',
	# 'FemaleHealthAccess_RichHigh',
	# 'FemaleHealthAccess_PoorHigh',
	# 'GeneralHealthAccess_RichHigh',
	# 'GeneralHealthAccess_PoorHigh',
	# 'GovtExpendituresEducation_High',
	'EducationServices_High',
	# 'FemaleSecondaryEducation_High',
	# 'FemaleEduAttain_High',
	'Capital_High',
	'Fishstock',
	'Livestock',
	# 'GlobalNonrenewableConsumption',
	'Crops',
	'CO2Conc',
	'TempAnamoly',
	'CO2EmissionPC_Low',
	'CO2EmissionPC_Mid',
	'CO2EmissionPC_High')
yactual = yobs[yobs$variable %in% fitvar,]
obstime = sort(unique(yactual[,'time']))


CalibParms = c( 
	'OmegaF_RichM1',
	'OmegaF_PoorM1',
	'OmegaF_M2',
	'OmegaF_M3',
	'OmegaF_M4',
	'OmegaF_RichF1',
	'OmegaF_PoorF1',
	'OmegaF_F2',
	'OmegaF_F3',
	'OmegaF_F4',
	'OmegaH_RichM1',
	'OmegaH_PoorM1',
	'OmegaH_M2',
	'OmegaH_M3',
	'OmegaH_M4',
	'OmegaH_RichF1',
	'OmegaH_PoorF1',
	'OmegaH_F2',
	'OmegaH_F3',
	'OmegaH_F4',
	'ThetaU',
	'ZetaU',
	# 'LandProdElastLivestock',
	# 'WaterProdElastLivestock',
	# 'LivestockTechMult',
	# 'RenewableCapitalReturn_Low',
	# 'RenewableCapitalReturn_Mid',
	# 'RenewableCapitalReturn_High',
	# 'NonrenewableCapitalReturn_Low',
	# 'NonrenewableCapitalReturn_Mid',
	# 'NonrenewableCapitalReturn_High',
	'TechMult_Low',
	'TechMult_Mid',
	'TechMult_High',
	'FishProdDelay',
	'LivestockProdDelay',
	'CropsProdDelay',
	# 'FoodIncomeElasticity_Low',
	# 'FoodIncomeElasticity_Mid',
	# 'FoodIncomeElasticity_High',
	'PsiE1_Low',
	'PsiE1_Mid',
	'PsiE1_High',
	'PsiE2_Low',
	'PsiE2_Mid',
	'PsiE2_High',
	'LambdaE_Low',
	'LambdaE_Mid',
	'LambdaE_High',
	'LambdaH_Low',
	'LambdaH_Mid',
	'LambdaH_High',
	'ChiEF1_Low',
	'ChiEF1_Mid',
	'ChiEF1_High',
	'ChiHF1_Low',
	'ChiHF1_Mid',
	'ChiHF1_High',
	'ChiHA1_RichLow',
	'ChiHA1_RichMid',
	'ChiHA1_RichHigh',
	'ChiHA1_PoorLow',
	'ChiHA1_PoorMid',
	'ChiHA1_PoorHigh',
	'ChiEF2_Low',
	'ChiEF2_Mid',
	'ChiEF2_High',
	'ChiHF2_Low',
	'ChiHF2_Mid',
	'ChiHF2_High',
	'ChiHA2_RichLow',
	'ChiHA2_RichMid',
	'ChiHA2_RichHigh',
	'ChiHA2_PoorLow',
	'ChiHA2_PoorMid',
	'ChiHA2_PoorHigh',
	'ChiHA3_RichLow',
	'ChiHA3_RichMid',
	'ChiHA3_RichHigh',
	'ChiHA3_PoorLow',
	'ChiHA3_PoorMid',
	'ChiHA3_PoorHigh'
)

ObjCost = function(p, obstime, delta_t, delayyearlength, init, parms, yactual){
	whichpar = names(p)
	parms[whichpar] = p
	t0 = min(obstime)
	tf = max(obstime)
	ysim = WorldMod(t0,tf,delta_t,delayyearlength,init,parms)
	RunCost = modCost(ysim,yactual,x = 'time',y='value')
	# print(RunCost)
	return(RunCost)
}

ParValue = setNames(as.numeric(as.character(ParameterData[CalibParms,'value'])),CalibParms)
ParMin = setNames(as.numeric(as.character(ParameterData[CalibParms,'min'])),CalibParms)
ParMax = setNames(as.numeric(as.character(ParameterData[CalibParms,'max'])),CalibParms)



####################### OPTIMIZATION
print(paste('Optimization Solver: ',OptSolver))
ptm = proc.time() 
Fit = modFit(
	p = ParValue, 
	lower = ParMin, 
	upper = ParMax, 
	f = ObjCost, 
	obstime=obstime,
	delta_t=delta_t,
	delayyearlength = delayyearlength,
	init =InitValue,
	parms = ParameterValue, 
	yactual = yactual, 
	method = OptSolver,
	control = list( 
		verbose = T
		# print.level = 1 
	)
)		
ptm = proc.time() - ptm
print(ptm)
print(suppressWarnings(summary(Fit)))
FittedParameters = ParameterValue
FittedParameters[names(coef(Fit))] = coef(Fit)
write.csv(coef(Fit),file='FittedCoef.csv')

# ####################### MCMC
# print(paste('Optimization Solver: ',OptSolver))
# ptm = proc.time() 
# MCMCOut = modMCMC(
# 		p = ParValue, 
# 		lower = ParMin, 
# 		upper = ParMax, 
# 		f = ObjCost, 
# 		obstime=obstime,
# 		delta_t=delta_t,
# 		delayyearlength = delayyearlength,
# 		init =InitValue,
# 		parms = ParameterValue, 
# 		yobs=yobs)		
# ptm = proc.time() - ptm
# print(ptm)
# print(summary(MCMCOut))

print('----------------- CALIBRATE MODEL   ---------------------')

library(FME)

# IMPORT DATA


ObsData = read.csv(  file = './DataInput/CalibrationInput.csv',sep =',',
						header = T, fileEncoding="UTF-8-BOM")

dropvar = names(ObsData) %in% c(
						'Pop1Male_Low',
						'Pop2Male_Low',
						'Pop3Male_Low',
						'Pop4Male_Low',
						'Pop1Female_Low',
						'Pop2Female_Low',
						'Pop3Female_Low',
						'Pop4Female_Low',
						'Pop1Male_Mid',
						'Pop2Male_Mid',
						'Pop3Male_Mid',
						'Pop4Male_Mid',
						'Pop1Female_Mid',
						'Pop2Female_Mid',
						'Pop3Female_Mid',
						'Pop4Female_Mid',
						'Pop1Male_High',
						'Pop2Male_High',
						'Pop3Male_High',
						'Pop4Male_High',
						'Pop1Female_High',
						'Pop2Female_High',
						'Pop3Female_High',
						'Pop4Female_High',
						'Bottom20Perc_Low',
						'Bottom20Perc_Mid',
						'Bottom20Perc_High',
						'SmoothBottom20Perc_Low',
						'SmoothBottom20Perc_Mid',
						'SmoothBottom20Perc_High'
						)

CalibData = ObsData[,!dropvar]

colnames(CalibData) = c(  	'time',
							'Low_RM1',
							'Low_PM1',
							'Low_RM2',
							'Low_PM2',
							'Low_RM3',
							'Low_PM3',
							'Low_RM4',
							'Low_PM4',
							'Low_RF1',
							'Low_PF1',
							'Low_RF2',
							'Low_PF2',
							'Low_RF3',
							'Low_PF3',
							'Low_RF4',
							'Low_PF4',
							'GFR_Low',
							'TFR_Low',
							'DeathRateM1_Low',
							'DeathRateM2_Low',
							'DeathRateM3_Low',
							'DeathRateM4_Low',
							'DeathRateF1_Low',
							'DeathRateF2_Low',
							'DeathRateF3_Low',
							'DeathRateF4_Low',
							'EconOutput_Low',
							'LowPop',
							'EconOutputPC_Low',
							'HealthServices_Low',
							'FemaleHealthAccess_RichLow',
							'FemaleHealthAccess_PoorLow',
							'GeneralHealthAccess_RichLow',
							'GeneralHealthAccess_PoorLow',
							'Mid_RM1',
							'Mid_PM1',
							'Mid_RM2',
							'Mid_PM2',
							'Mid_RM3',
							'Mid_PM3',
							'Mid_RM4',
							'Mid_PM4',
							'Mid_RF1',
							'Mid_PF1',
							'Mid_RF2',
							'Mid_PF2',
							'Mid_RF3',
							'Mid_PF3',
							'Mid_RF4',
							'Mid_PF4',
							'GFR_Mid',
							'TFR_Mid',
							'DeathRateM1_Mid',
							'DeathRateM2_Mid',
							'DeathRateM3_Mid',
							'DeathRateM4_Mid',
							'DeathRateF1_Mid',
							'DeathRateF2_Mid',
							'DeathRateF3_Mid',
							'DeathRateF4_Mid',
							'EconOutput_Mid',
							'MidPop',
							'EconOutputPC_Mid',
							'HealthServices_Mid',
							'FemaleHealthAccess_RichMid',
							'FemaleHealthAccess_PoorMid',
							'GeneralHealthAccess_RichMid',
							'GeneralHealthAccess_PoorMid',
							'High_RM1',
							'High_PM1',
							'High_RM2',
							'High_PM2',
							'High_RM3',
							'High_PM3',
							'High_RM4',
							'High_PM4',
							'High_RF1',
							'High_PF1',
							'High_RF2',
							'High_PF2',
							'High_RF3',
							'High_PF3',
							'High_RF4',
							'High_PF4',
							'GFR_High',
							'TFR_High',
							'DeathRateM1_High',
							'DeathRateM2_High',
							'DeathRateM3_High',
							'DeathRateM4_High',
							'DeathRateF1_High',
							'DeathRateF2_High',
							'DeathRateF3_High',
							'DeathRateF4_High',
							'EconOutput_High',
							'HighPop',
							'EconOutputPC_High',
							'HealthServices_High',
							'FemaleHealthAccess_RichHigh',
							'FemaleHealthAccess_PoorHigh',
							'GeneralHealthAccess_RichHigh',
							'GeneralHealthAccess_PoorHigh')


yobs = na.omit(melt(CalibData,id='time'))
yobs$variable = as.character(yobs$variable)
yobs = yobs[,c('variable','time','value')]
obstime = sort(unique(yobs[,'time']))

CalibParms = c( 
	'OmegaF_M1',
	'OmegaF_M2',
	'OmegaF_M3',
	'OmegaF_M4',
	'OmegaF_F1',
	'OmegaF_F2',
	'OmegaF_F3',
	'OmegaF_F4',
	'OmegaH_M1',
	'OmegaH_M2',
	'OmegaH_M3',
	'OmegaH_M4',
	'OmegaH_F1',
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
	'FishProdDelay',
	'LivestockProdDelay',
	'CropsProdDelay',
	# 'FoodIncomeElasticity_Low',
	# 'FoodIncomeElasticity_Mid',
	# 'FoodIncomeElasticity_High',
	'PsiE_Low',
	'PsiE_Mid',
	'PsiE_High',
	'LambdaE_Low',
	'LambdaE_Mid',
	'LambdaE_High',
	'LambdaH_Low',
	'LambdaH_Mid',
	'LambdaH_High',
	'ChiEF1_RichLow',
	'ChiEF1_RichMid',
	'ChiEF1_RichHigh',
	'ChiEF1_PoorLow',
	'ChiEF1_PoorMid',
	'ChiEF1_PoorHigh',
	'ChiHF1_RichLow',
	'ChiHF1_RichMid',
	'ChiHF1_RichHigh',
	'ChiHF1_PoorLow',
	'ChiHF1_PoorMid',
	'ChiHF1_PoorHigh',
	'ChiHA1_RichLow',
	'ChiHA1_RichMid',
	'ChiHA1_RichHigh',
	'ChiHA1_PoorLow',
	'ChiHA1_PoorMid',
	'ChiHA1_PoorHigh',
	'ChiEF2_RichLow',
	'ChiEF2_RichMid',
	'ChiEF2_RichHigh',
	'ChiEF2_PoorLow',
	'ChiEF2_PoorMid',
	'ChiEF2_PoorHigh',
	'ChiHF2_RichLow',
	'ChiHF2_RichMid',
	'ChiHF2_RichHigh',
	'ChiHF2_PoorLow',
	'ChiHF2_PoorMid',
	'ChiHF2_PoorHigh',
	'ChiHA2_RichLow',
	'ChiHA2_RichMid',
	'ChiHA2_RichHigh',
	'ChiHA2_PoorLow',
	'ChiHA2_PoorMid',
	'ChiHA2_PoorHigh',
	'ChiEF3_RichLow',
	'ChiEF3_RichMid',
	'ChiEF3_RichHigh',
	'ChiEF3_PoorLow',
	'ChiEF3_PoorMid',
	'ChiEF3_PoorHigh',
	'ChiHF3_RichLow',
	'ChiHF3_RichMid',
	'ChiHF3_RichHigh',
	'ChiHF3_PoorLow',
	'ChiHF3_PoorMid',
	'ChiHF3_PoorHigh',
	'ChiHA3_RichLow',
	'ChiHA3_RichMid',
	'ChiHA3_RichHigh',
	'ChiHA3_PoorLow',
	'ChiHA3_PoorMid',
	'ChiHA3_PoorHigh'
)

ObjCost = function(p, obstime, delta_t, delayyearlength, init, parms, yobs){
	whichpar = names(p)
	parms[whichpar] = p
	t0 = min(obstime)
	tf = max(obstime)
	ysim = WorldMod(t0,tf,delta_t,delayyearlength,init,parms)
	modCost(ysim,yobs,x = 'time',y='value')
}

ParValue = setNames(as.numeric(as.character(ParameterData[CalibParms,'value'])),CalibParms)
ParMin = setNames(as.numeric(as.character(ParameterData[CalibParms,'min'])),CalibParms)
ParMax = setNames(as.numeric(as.character(ParameterData[CalibParms,'max'])),CalibParms)



####################### OPTIMIZATION
# OptSolver = 'Newton'
OptSolver = 'BFGS'
# OptSolver = 'CG'
# OptSolver = 'Pseudo'

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
		yobs=yobs, 
		method = OptSolver)		
ptm = proc.time() - ptm
print(ptm)
print(summary(Fit))
FittedParameters = ParameterValue
FittedParameters[names(coef(Fit))] = coef(Fit)


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

print('----------------- CALIBRATE MODEL   ---------------------')

library(FME)

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
	'TechMult_Low',
	'TechMult_Mid',
	'TechMult_High',
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

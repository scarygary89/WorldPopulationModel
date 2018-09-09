########################                       ########################
########################  ECONOMY SUBMODEL     ########################
########################                       ########################

library(foreach)
library(doParallel)
numcore = detectCores()
cl<-makeCluster(numcore) 
print(cl)

################# SUBMODEL WITH EXOGENOUS INPUTS

EconomyMod = function(t0,tf,delta_t,delayyearlength,exog,init,parms)
{
	with(as.list(c(parms)), {	
		tspan = seq(from=t0, to=tf, by=delta_t)
		auxnames = c(
			'Inequality',
			'EconOutput',
			'logEconOutput')
		AuxData = matrix(NA,nrow = length(tspan),ncol = length(auxnames))
		colnames(AuxData) = auxnames
		StockData = matrix(NA,nrow = (length(tspan) + 1),ncol = length(init)) 
		colnames(StockData) = names(init)
		stocks = init
		StockData[1,] = stocks
		for(i in 1:length(tspan)){
			EmployedWorkRatio_ij = c(
				M1 = EmployedWorkRatio_M1,
				M2 = EmployedWorkRatio_M2,
				M3 = EmployedWorkRatio_M3,
				M4 = EmployedWorkRatio_M4,
				F1 = EmployedWorkRatio_F1,
				F2 = EmployedWorkRatio_F2,
				F3 = EmployedWorkRatio_F3,
				F4 = EmployedWorkRatio_F4
			)
			RegPop_ij = c(
				M1 = exog[i,'Pop_M1'],
				M2 = exog[i,'Pop_M2'],
				M3 = exog[i,'Pop_M3'],
				M4 = exog[i,'Pop_M4'],
				F1 = exog[i,'Pop_F1'],
				F2 = exog[i,'Pop_F2'],
				F3 = exog[i,'Pop_F3'],
				F4 = exog[i,'Pop_F4']
			)
			RegPop = exog[i,'RegPop']

			EconOut	= Economy(
				exog[i,'CoalReserves'],
				exog[i,'OilReserves'],
				exog[i,'GasReserves'],
				RegPop,
				stocks['Capital'],
				CoalAccess,
				OilAccess,
				GasAccess,
				stocks['TechMult'],
				TechGrowth,
				LaborInputElast,
				CapitalInputElast,
				CoalInputElast,
				OilInputElast,
				GasInputElast,
				SavingsRate,
				DeprecRate,
				EmployedWorkRatio_ij,
				RegPop_ij,
				IneqMult,
				IneqInt,
				parms
			)

			aux = c(
				EconOut[['Inequality']],
				EconOut[['EconOutput']],
				EconOut[['logEconOutput']]
				)
			AuxData[i,] = aux
			dstocks = c(
				EconOut[['dCapital']],
				EconOut[['dTechMult']])
			stocks = stocks + dstocks * delta_t
			stocks = pmax(stocks,1e-16)
			StockData[i+1,] = stocks
		}
	Output = cbind(tspan, StockData[-(length(tspan) + 1),],AuxData)
	colnames(Output)[1] = 'time'
	return(Output)
	})
}

################# COST FUNCTION

EconomyCost = function(p,t0,tf,delta_t,delayyearlength,exog,init,parms,yactual)
{
	whichpar = names(parms)[names(parms) %in% names(p)]
	whichinit = names(init)[names(init) %in% names(p)]
	parms[whichpar] = p[whichpar]
	init[whichinit] = p[whichinit]
	ysim = EconomyMod(t0,tf,delta_t,delayyearlength,exog,init,parms)
	RunCost = modCost(ysim,yactual,x = 'time',y = 'value',err = 'error',
		scaleVar=T)
	# print(RunCost)
	return(RunCost)
}

################# PARAMETER ESTIMATION FUNCTION

EconomyFit = function(parvalue,parmin,parmax,yactual,optmethod,control=list(),
	delta_t,delayyearlength,exog,init,parms)
{
	t0 = min(exog$time)
	tf = max(exog$time)
	ptm = proc.time() 
	EconFit = modFit(
		f = EconomyCost,
		p = parvalue,
		lower = parmin,
		upper = parmax,
		t0 = t0,
		tf = tf,
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		exog = exog,
		init = init,
		parms = parms,
		yactual = yactual,
		method = optmethod,
		control = control
		)
	ptm = proc.time() - ptm
	print(ptm)
	return(EconFit)
}

################# DEFINE INITIAL PARAMETER VALUES

EconParms_Low = c( 	
	CoalAccess = as.numeric(ParameterValue['CoalAccess_Low']),
	OilAccess = as.numeric(ParameterValue['OilAccess_Low']),
	GasAccess = as.numeric(ParameterValue['GasAccess_Low']),
	TechGrowth = as.numeric(ParameterValue['TechGrowth_Low']),
	LaborInputElast = as.numeric(ParameterValue['LaborInputElast_Low']),
	CapitalInputElast = as.numeric(ParameterValue['CapitalInputElast_Low']),
	EmployedWorkRatio_M1 = as.numeric(ParameterValue['LowEmployedWorkRatio_M1']),
	EmployedWorkRatio_M2 = as.numeric(ParameterValue['LowEmployedWorkRatio_M2']),
	EmployedWorkRatio_M3 = as.numeric(ParameterValue['LowEmployedWorkRatio_M3']),
	EmployedWorkRatio_M4 = as.numeric(ParameterValue['LowEmployedWorkRatio_M4']),
	EmployedWorkRatio_F1 = as.numeric(ParameterValue['LowEmployedWorkRatio_F1']),
	EmployedWorkRatio_F2 = as.numeric(ParameterValue['LowEmployedWorkRatio_F2']),
	EmployedWorkRatio_F3 = as.numeric(ParameterValue['LowEmployedWorkRatio_F3']),
	EmployedWorkRatio_F4 = as.numeric(ParameterValue['LowEmployedWorkRatio_F4']),
	SavingsRate = as.numeric(ParameterValue['SavingsRate_Low']),
	DeprecRate = as.numeric(ParameterValue['DeprecRate_Low']),
	IneqMult = as.numeric(ParameterValue['IneqMult_Low']),
	IneqInt = as.numeric(ParameterValue['IneqInt_Low']),
	InitEconOutputGrowth = as.numeric(ParameterValue['InitEconOutputGrowth_Low']),
	CoalInputElast = as.numeric(ParameterValue['CoalInputElast_Low']),
	OilInputElast = as.numeric(ParameterValue['OilInputElast_Low']),
	GasInputElast = as.numeric(ParameterValue['GasInputElast_Low'])

)

EconParms_Mid = c( 	
	CoalAccess = as.numeric(ParameterValue['CoalAccess_Mid']),
	OilAccess = as.numeric(ParameterValue['OilAccess_Mid']),
	GasAccess = as.numeric(ParameterValue['GasAccess_Mid']),
	TechGrowth = as.numeric(ParameterValue['TechGrowth_Mid']),
	LaborInputElast = as.numeric(ParameterValue['LaborInputElast_Mid']),
	CapitalInputElast = as.numeric(ParameterValue['CapitalInputElast_Mid']),
	EmployedWorkRatio_M1 = as.numeric(ParameterValue['MidEmployedWorkRatio_M1']),
	EmployedWorkRatio_M2 = as.numeric(ParameterValue['MidEmployedWorkRatio_M2']),
	EmployedWorkRatio_M3 = as.numeric(ParameterValue['MidEmployedWorkRatio_M3']),
	EmployedWorkRatio_M4 = as.numeric(ParameterValue['MidEmployedWorkRatio_M4']),
	EmployedWorkRatio_F1 = as.numeric(ParameterValue['MidEmployedWorkRatio_F1']),
	EmployedWorkRatio_F2 = as.numeric(ParameterValue['MidEmployedWorkRatio_F2']),
	EmployedWorkRatio_F3 = as.numeric(ParameterValue['MidEmployedWorkRatio_F3']),
	EmployedWorkRatio_F4 = as.numeric(ParameterValue['MidEmployedWorkRatio_F4']),
	SavingsRate = as.numeric(ParameterValue['SavingsRate_Mid']),
	DeprecRate = as.numeric(ParameterValue['DeprecRate_Mid']),
	IneqMult = as.numeric(ParameterValue['IneqMult_Mid']),
	IneqInt = as.numeric(ParameterValue['IneqInt_Mid']),
	InitEconOutputGrowth = as.numeric(ParameterValue['InitEconOutputGrowth_Mid']),
	CoalInputElast = as.numeric(ParameterValue['CoalInputElast_Mid']),
	OilInputElast = as.numeric(ParameterValue['OilInputElast_Mid']),
	GasInputElast = as.numeric(ParameterValue['GasInputElast_Mid'])
)

EconParms_High = c( 	
	CoalAccess = as.numeric(ParameterValue['CoalAccess_High']),
	OilAccess = as.numeric(ParameterValue['OilAccess_High']),
	GasAccess = as.numeric(ParameterValue['GasAccess_High']),
	TechGrowth = as.numeric(ParameterValue['TechGrowth_High']),
	LaborInputElast = as.numeric(ParameterValue['LaborInputElast_High']),
	CapitalInputElast = as.numeric(ParameterValue['CapitalInputElast_High']),
	EmployedWorkRatio_M1 = as.numeric(ParameterValue['HighEmployedWorkRatio_M1']),
	EmployedWorkRatio_M2 = as.numeric(ParameterValue['HighEmployedWorkRatio_M2']),
	EmployedWorkRatio_M3 = as.numeric(ParameterValue['HighEmployedWorkRatio_M3']),
	EmployedWorkRatio_M4 = as.numeric(ParameterValue['HighEmployedWorkRatio_M4']),
	EmployedWorkRatio_F1 = as.numeric(ParameterValue['HighEmployedWorkRatio_F1']),
	EmployedWorkRatio_F2 = as.numeric(ParameterValue['HighEmployedWorkRatio_F2']),
	EmployedWorkRatio_F3 = as.numeric(ParameterValue['HighEmployedWorkRatio_F3']),
	EmployedWorkRatio_F4 = as.numeric(ParameterValue['HighEmployedWorkRatio_F4']),
	SavingsRate = as.numeric(ParameterValue['SavingsRate_High']),
	DeprecRate = as.numeric(ParameterValue['DeprecRate_High']),
	IneqMult = as.numeric(ParameterValue['IneqMult_High']),
	IneqInt = as.numeric(ParameterValue['IneqInt_High']),
	InitEconOutputGrowth = as.numeric(ParameterValue['InitEconOutputGrowth_High']),
	CoalInputElast = as.numeric(ParameterValue['CoalInputElast_High']),
	OilInputElast = as.numeric(ParameterValue['OilInputElast_High']),
	GasInputElast = as.numeric(ParameterValue['GasInputElast_High'])
)
			
################# DEFINE INITIAL CONDITIONS

EconInit_Low = c( 	
	Capital = as.numeric(InitValue['Capital_Low']),
	TechMult = as.numeric(InitValue['TechMult_Low'])
)

EconInit_Mid = c( 	
	Capital = as.numeric(InitValue['Capital_Mid']),
	TechMult = as.numeric(InitValue['TechMult_Mid'])
)

EconInit_High = c( 	
	Capital = as.numeric(InitValue['Capital_High']),
	TechMult = as.numeric(InitValue['TechMult_High'])
)

################# LOAD EXOGENOUS DATA

EconExog_Low = na.omit(cbind(
	time = CalibData['time'],
	Pop_M1 = CalibData[,'Low_M1'],
	Pop_M2 = CalibData[,'Low_M2'],
	Pop_M3 = CalibData[,'Low_M3'],
	Pop_M4 = CalibData[,'Low_M4'],
	Pop_F1 = CalibData[,'Low_F1'],
	Pop_F2 = CalibData[,'Low_F2'],
	Pop_F3 = CalibData[,'Low_F3'],
	Pop_F4 = CalibData[,'Low_F4'],
	CoalReserves = CalibData[,'CoalReserves'],
	OilReserves = CalibData[,'OilReserves'],
	GasReserves = CalibData[,'GasReserves'],
	RegPop = CalibData[,'LowPop']
))

EconExog_Mid = na.omit(cbind(
	time = CalibData['time'],
	Pop_M1 = CalibData[,'Mid_M1'],
	Pop_M2 = CalibData[,'Mid_M2'],
	Pop_M3 = CalibData[,'Mid_M3'],
	Pop_M4 = CalibData[,'Mid_M4'],
	Pop_F1 = CalibData[,'Mid_F1'],
	Pop_F2 = CalibData[,'Mid_F2'],
	Pop_F3 = CalibData[,'Mid_F3'],
	Pop_F4 = CalibData[,'Mid_F4'],
	CoalReserves = CalibData[,'CoalReserves'],
	OilReserves = CalibData[,'OilReserves'],
	GasReserves = CalibData[,'GasReserves'],
	RegPop = CalibData[,'LowPop']
))

EconExog_High = na.omit(cbind(
	time = CalibData['time'],
	Pop_M1 = CalibData[,'High_M1'],
	Pop_M2 = CalibData[,'High_M2'],
	Pop_M3 = CalibData[,'High_M3'],
	Pop_M4 = CalibData[,'High_M4'],
	Pop_F1 = CalibData[,'High_F1'],
	Pop_F2 = CalibData[,'High_F2'],
	Pop_F3 = CalibData[,'High_F3'],
	Pop_F4 = CalibData[,'High_F4'],
	CoalReserves = CalibData[,'CoalReserves'],
	OilReserves = CalibData[,'OilReserves'],
	GasReserves = CalibData[,'GasReserves'],
	RegPop = CalibData[,'LowPop']

))

################# LOAD ACTUAL DATA

EconActual_Low = na.omit(melt(
	data.frame(cbind(
		time = CalibData$time,
		# EconOutput = CalibData$EconOutput_Low,
		logEconOutput = log(CalibData$EconOutput_Low),
		Inequality = CalibData$Inequality_Low,
		Capital = CalibData$Capital_Low)),
	id ='time'))
EconActual_Low$variable = as.character(EconActual_Low$variable)
EconActual_Low = EconActual_Low[,c('variable','time','value')]
# EconActual_Low$error[EconActual_Low$variable == 'EconOutput'] = 
# 	mean(EconActual_Low$value[EconActual_Low$variable == 'EconOutput']) #/ 
# 	# (EconActual_Low$time[EconActual_Low$variable == 'EconOutput'] - 1979) ^ 2
EconActual_Low$error[EconActual_Low$variable == 'logEconOutput'] = 
	mean(EconActual_Low$value[EconActual_Low$variable == 'logEconOutput']) #/ 
	# (EconActual_Low$time[EconActual_Low$variable == 'logEconOutput'] - 1979) ^ 2
EconActual_Low$error[EconActual_Low$variable == 'Inequality'] = 
	mean(EconActual_Low$value[EconActual_Low$variable == 'Inequality']) #/ 
	# (EconActual_Low$time[EconActual_Low$variable == 'Inequality'] - 1979) ^ 2
EconActual_Low$error[EconActual_Low$variable == 'Capital'] =  
	mean(EconActual_Low$value[EconActual_Low$variable == 'Capital']) #/ 
	# (EconActual_Low$time[EconActual_Low$variable == 'Capital'] - 1979) ^ 2

EconActual_Mid = na.omit(melt(
	data.frame(cbind(
		time = CalibData$time,
		# EconOutput = CalibData$EconOutput_Mid,
		logEconOutput = log(CalibData$EconOutput_Mid),
		Inequality = CalibData$Inequality_Mid,
		Capital = CalibData$Capital_Mid)),
	id ='time'))
EconActual_Mid$variable = as.character(EconActual_Mid$variable)
EconActual_Mid = EconActual_Mid[,c('variable','time','value')]
# EconActual_Mid$error[EconActual_Mid$variable == 'EconOutput'] = 
# 	mean(EconActual_Mid$value[EconActual_Mid$variable == 'EconOutput']) #/ 
# 	# (EconActual_Mid$time[EconActual_Mid$variable == 'EconOutput'] - 1979) ^ 2
EconActual_Mid$error[EconActual_Mid$variable == 'logEconOutput'] = 
	mean(EconActual_Mid$value[EconActual_Mid$variable == 'logEconOutput']) #/ 
	# (EconActual_Mid$time[EconActual_Mid$variable == 'logEconOutput'] - 1979) ^ 2
EconActual_Mid$error[EconActual_Mid$variable == 'Inequality'] = 
	mean(EconActual_Mid$value[EconActual_Mid$variable == 'Inequality']) #/ 
	# (EconActual_Mid$time[EconActual_Mid$variable == 'Inequality'] - 1979) ^ 2
EconActual_Mid$error[EconActual_Mid$variable == 'Capital'] =  
	mean(EconActual_Mid$value[EconActual_Mid$variable == 'Capital']) #/ 
	# (EconActual_Mid$time[EconActual_Mid$variable == 'Capital'] - 1979) ^ 2

EconActual_High = na.omit(melt(
	data.frame(cbind(
		time = CalibData$time,
		# EconOutput = CalibData$EconOutput_High,
		logEconOutput = log(CalibData$EconOutput_High),
		Inequality = CalibData$Inequality_High,
		Capital = CalibData$Capital_High)),
	id ='time'))
EconActual_High$variable = as.character(EconActual_High$variable)
EconActual_High = EconActual_High[,c('variable','time','value')]
# EconActual_High$error[EconActual_High$variable == 'EconOutput'] = 
# 	mean(EconActual_High$value[EconActual_High$variable == 'EconOutput']) # / 
# 	# (EconActual_High$time[EconActual_High$variable == 'EconOutput'] - 1979) ^ 2
EconActual_High$error[EconActual_High$variable == 'logEconOutput'] = 
	mean(EconActual_High$value[EconActual_High$variable == 'logEconOutput']) #/ 
	# (EconActual_High$time[EconActual_High$variable == 'logEconOutput'] - 1979) ^ 2
EconActual_High$error[EconActual_High$variable == 'Inequality'] = 
	mean(EconActual_High$value[EconActual_High$variable == 'Inequality']) #/ 
	# (EconActual_High$time[EconActual_High$variable == 'Inequality'] - 1979) ^ 2
EconActual_High$error[EconActual_High$variable == 'Capital'] =  
	mean(EconActual_High$value[EconActual_High$variable == 'Capital']) #/ 
	# (EconActual_High$time[EconActual_High$variable == 'Capital'] - 1979) ^ 2


################# DEFINE CALIBRATION PARAMETERS

EconCalibPars_Low =  rbind(
	TechMult = as.numeric(InitialData['TechMult_Low',]),
	TechGrowth = as.numeric(ParameterData['TechGrowth_Low',]),
	SavingsRate = as.numeric(ParameterData['SavingsRate_Low',]),
	DeprecRate = as.numeric(ParameterData['DeprecRate_Low',]),
	CoalInputElast = as.numeric(ParameterData['CoalInputElast_Low',]),
	OilInputElast = as.numeric(ParameterData['OilInputElast_Low',]),
	GasInputElast = as.numeric(ParameterData['GasInputElast_Low',]),
	CapitalInputElast = as.numeric(ParameterData['CapitalInputElast_Low',]),
	LaborInputElast = as.numeric(ParameterData['LaborInputElast_Low',]),
	IneqMult = as.numeric(ParameterData['IneqMult_Low',]),
	IneqInt = as.numeric(ParameterData['IneqInt_Low',])
)
EconParValue_Low = EconCalibPars_Low[,1]
EconParMin_Low =  EconCalibPars_Low[,2]
EconParMax_Low = EconCalibPars_Low[,3]

EconCalibPars_Mid =  rbind(
	TechMult = as.numeric(InitialData['TechMult_Mid',]),
	TechGrowth = as.numeric(ParameterData['TechGrowth_Mid',]),
	SavingsRate = as.numeric(ParameterData['SavingsRate_Mid',]),
	DeprecRate = as.numeric(ParameterData['DeprecRate_Mid',]),
	CoalInputElast = as.numeric(ParameterData['CoalInputElast_Mid',]),
	OilInputElast = as.numeric(ParameterData['OilInputElast_Mid',]),
	GasInputElast = as.numeric(ParameterData['GasInputElast_Mid',]),
	CapitalInputElast = as.numeric(ParameterData['CapitalInputElast_Mid',]),
	LaborInputElast = as.numeric(ParameterData['LaborInputElast_Mid',]),
	IneqMult = as.numeric(ParameterData['IneqMult_Mid',]),
	IneqInt = as.numeric(ParameterData['IneqInt_Mid',])
)
EconParValue_Mid = EconCalibPars_Mid[,1]
EconParMin_Mid =  EconCalibPars_Mid[,2]
EconParMax_Mid = EconCalibPars_Mid[,3]

EconCalibPars_High =  rbind(
	TechMult = as.numeric(InitialData['TechMult_High',]),
	TechGrowth = as.numeric(ParameterData['TechGrowth_High',]),
	SavingsRate = as.numeric(ParameterData['SavingsRate_High',]),
	DeprecRate = as.numeric(ParameterData['DeprecRate_High',]),
	CoalInputElast = as.numeric(ParameterData['CoalInputElast_High',]),
	OilInputElast = as.numeric(ParameterData['OilInputElast_High',]),
	GasInputElast = as.numeric(ParameterData['GasInputElast_High',]),
	CapitalInputElast = as.numeric(ParameterData['CapitalInputElast_High',]),
	LaborInputElast = as.numeric(ParameterData['LaborInputElast_High',]),
	IneqMult = as.numeric(ParameterData['IneqMult_High',]),
	IneqInt = as.numeric(ParameterData['IneqInt_High',])
)
EconParValue_High = EconCalibPars_High[,1]
EconParMin_High =  EconCalibPars_High[,2]
EconParMax_High = EconCalibPars_High[,3]

################# 2-STAGE FIT PARAMETERS

N = 1000
registerDoParallel(cl)
ptm = proc.time() 
EconResults_Low = foreach(i=1:N,.packages='FME') %dopar%
{
	EconParValue_Low = sapply(names(EconParMin_Low),function(x) {
		runif(1,EconParMin_Low[x],EconParMax_Low[x])
	})	

	EconFitPseudo_Low = EconomyFit(
		parvalue = EconParValue_Low,
		parmin = EconParMin_Low,
		parmax = EconParMax_Low,
		yactual = EconActual_Low,
		optmethod = 'Pseudo',
		# control = list(numiter = 100000),
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		exog = EconExog_Low,
		init = EconInit_Low,
		parms = EconParms_Low)

	EconFit_Low = EconomyFit(
		parvalue = coef(EconFitPseudo_Low),
		# parvalue = EconParValue_Low,
		parmin = EconParMin_Low,
		parmax = EconParMax_Low,
		yactual = EconActual_Low,
		optmethod = 'CG',
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		exog = EconExog_Low,
		init = EconInit_Low,
		parms = EconParms_Low)
	
	return(EconFit_Low)
}
ptm = proc.time() - ptm
print(ptm)
ptm = proc.time() 
EconResults_Mid = foreach(i=1:N,.packages='FME') %dopar%
{

	EconParValue_Mid = sapply(names(EconParMin_Mid),function(x) {
		runif(1,EconParMin_Mid[x],EconParMax_Mid[x])
	})

	EconFitPseudo_Mid = EconomyFit(
		parvalue = EconParValue_Mid,
		parmin = EconParMin_Mid,
		parmax = EconParMax_Mid,
		yactual = EconActual_Mid,
		optmethod = 'Pseudo',
		# control = list(numiter = 100000),
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		exog = EconExog_Mid,
		init = EconInit_Mid,
		parms = EconParms_Mid)

	EconFit_Mid = EconomyFit(
		parvalue = coef(EconFitPseudo_Mid),
		# parvalue = EconParValue_High,
		parmin = EconParMin_Mid,
		parmax = EconParMax_Mid,
		yactual = EconActual_Mid,
		optmethod = 'CG',
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		exog = EconExog_Mid,
		init = EconInit_Mid,
		parms = EconParms_Mid)

	return(EconFit_Mid)
}
ptm = proc.time() - ptm
print(ptm)
ptm = proc.time() 	
EconResults_High = foreach(i = 1:N,.packages='FME') %dopar%
{
	EconParValue_High = sapply(names(EconParMin_High),function(x) {
		runif(1,EconParMin_High[x],EconParMax_High[x])
	})	

	EconFitPseudo_High = EconomyFit(
		parvalue = EconParValue_High,
		parmin = EconParMin_High,
		parmax = EconParMax_High,
		yactual = EconActual_High,
		optmethod = 'Pseudo',
		# control = list(numiter = 100000),
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		exog = EconExog_High,
		init = EconInit_High,
		parms = EconParms_High)

	EconFit_High = EconomyFit(
		parvalue = coef(EconFitPseudo_High),
		# parvalue = EconParValue_High,
		parmin = EconParMin_High,
		parmax = EconParMax_High,
		yactual = EconActual_High,
		optmethod = 'Marq',
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		exog = EconExog_High,
		init = EconInit_High,
		parms = EconParms_High)

	return(EconFit_High)
}
ptm = proc.time() - ptm
print(ptm)
stopCluster(cl)

################# PLOT FITTED VALUES

CalibPlotFunc(EconResults_Low,EconActual_Low,EconParms_Low,EconExog_Low,EconInit_Low,
	EconomyMod,delta_t,delayyearlength,'EconomySubmodel_LowIncome')
CalibPlotFunc(EconResults_Mid,EconActual_Mid,EconParms_Mid,EconExog_Mid,
	EconInit_Mid,EconomyMod,delta_t,delayyearlength,'EconomySubmodel_MiddleIncome')
CalibPlotFunc(EconResults_High,EconActual_High,EconParms_High,EconExog_High,
	EconInit_High,EconomyMod,delta_t,delayyearlength,'EconomySubmodel_HighIncome')



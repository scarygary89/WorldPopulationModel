########################                       ########################
########################   HEALTH & EDUCATION  ########################
########################   SUBMODEL            ########################

library(foreach)
library(doParallel)
numcore = detectCores()
cl<-makeCluster(numcore) 
print(cl)

################# SUBMODEL WITH EXOGENOUS INPUTS

HealthEducationMod = function(t0,tf,delta_t,delayyearlength,exog,init,parms) 
{
	with(as.list(c(parms)), {	
		tspan = seq(from=t0, to=tf, by=delta_t)
		aux_names = c(
			'FemaleHealthAccess',
			'GeneralHealthAccess_Rich',
			'GeneralHealthAccess_Poor',
			'FemaleEduAttain'
			)
		AuxData = matrix(NA,
			nrow = (length(tspan)),
			ncol = length(aux_names)
			)		
		colnames(AuxData) = aux_names
		StockData = matrix(NA,
			nrow = (length(tspan) + 1),
			ncol = length(init)
			)
		colnames(StockData) = names(init)	
		stocks = init
		StockData[1,] = stocks
		for(i in 1:length(tspan)) {
			ChiHA1_k = c(Rich = ChiHA1_Rich, Poor = ChiHA1_Poor)
			ChiHA2_k = c(Rich = ChiHA2_Rich, Poor = ChiHA2_Poor)
			ChiHA3_k = c(Rich = ChiHA3_Rich, Poor = ChiHA3_Poor)

		# Extract Delayed Values
			if (i < (1 + delayyearlength / delta_t)) {
				PrevEconOutput = (1 - InitEconOutputGrowth) * exog[i,'EconOutput']
			}
			if (i >= (1 + delayyearlength / delta_t)) { 
				PrevEconOutput = exog[(i - delayyearlength / delta_t), "EconOutput"]
			} 		

			ChangeEconOutput = exog[i,'EconOutput'] - PrevEconOutput

			
			HealthEduOut = HealthEducation(
								stocks['HealthServices'],
								stocks['EducationServices'],
								ChangeEconOutput,
								exog[i,'EconOutput'],
								exog[i,'RegPop'],
								ZetaE,
								ZetaH,
								LambdaE,
								LambdaH,
								ChiEF1,
								ChiHF1,
								ChiHA1_k,
								ChiEF2,
								ChiHF2,
								ChiHA2_k,
								ChiHA3_k,
								exog[i,'Inequality'],
								parms)


			################ STORE OUTPUT

			# AUXILIARY VARIABLES
			aux = c(
				HealthEduOut[['FemaleHealthAccess']],
				HealthEduOut[['GeneralHealthAccess_k']]['Rich'],
				HealthEduOut[['GeneralHealthAccess_k']]['Poor'],
				HealthEduOut[['FemaleEduAttain']]
			)
			AuxData[i,] = aux

			# STOCK VARIABLES
			dstocks = c(
				HealthEduOut[["dEducationServices"]],
				HealthEduOut[["dHealthServices"]]
			) 
			stocks = stocks + dstocks * delta_t
			stocks = pmax(stocks,1e-16)
			StockData[i+1,] = stocks
		}
	Output = cbind(tspan, StockData[-(length(tspan)+1),],AuxData)
	colnames(Output)[1] = 'time'
	return(Output)
	})
}


################# COST FUNCTION

HealthEducationCost = function(p,t0,tf,delta_t,delayyearlength,exog,init,parms,yactual)
{
	whichpar = names(parms)[names(parms) %in% names(p)]
	whichinit = names(init)[names(init) %in% names(p)]
	parms[whichpar] = p[whichpar]
	init[whichinit] = p[whichinit]
	ysim = HealthEducationMod(t0,tf,delta_t,delayyearlength,exog,init,parms)
	RunCost = modCost(ysim,yactual,x = 'time',y = 'value',err = 'error',
		scaleVar=T)
	# print(RunCost)
	return(RunCost)
}

################# PARAMETER ESTIMATION FUNCTION

HealthEducationFit = function(parvalue,parmin,parmax,yactual,optmethod,control=list(),
	delta_t,delayyearlength,exog,init,parms)
{
	t0 = min(exog$time)
	tf = max(exog$time)
	ptm = proc.time() 
	HealthEducationFit = modFit(
		f = HealthEducationCost,
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
	return(HealthEducationFit)
}

################# DEFINE INITIAL PARAMETER VALUES

HealthEducationParms_Low = c( 
	InitEconOutputGrowth = as.numeric(ParameterValue['InitEconOutputGrowth_Low']),	
	ZetaE = as.numeric(ParameterValue['ZetaE_Low']),
	ZetaH = as.numeric(ParameterValue['ZetaH_Low']),
	LambdaE = as.numeric(ParameterValue['LambdaE_Low']),
	LambdaH = as.numeric(ParameterValue['LambdaH_Low']),
	ChiEF1 = as.numeric(ParameterValue['ChiEF1_Low']),
	ChiHF1 = as.numeric(ParameterValue['ChiHF1_Low']),
	ChiHA1_Rich = as.numeric(ParameterValue['ChiHA1_RichLow']),
	ChiHA1_Poor = as.numeric(ParameterValue['ChiHA1_PoorLow']),
	ChiEF2 = as.numeric(ParameterValue['ChiEF2_Low']),
	ChiHF2 = as.numeric(ParameterValue['ChiHF2_Low']),
	ChiHA2_Rich = as.numeric(ParameterValue['ChiHA2_RichLow']),
	ChiHA2_Poor = as.numeric(ParameterValue['ChiHA2_PoorLow']),
	ChiHA3_Rich = as.numeric(ParameterValue['ChiHA3_RichLow']),
	ChiHA3_Poor = as.numeric(ParameterValue['ChiHA3_PoorLow'])
)

HealthEducationParms_Mid = c( 
	InitEconOutputGrowth = as.numeric(ParameterValue['InitEconOutputGrowth_Mid']),	
	ZetaE = as.numeric(ParameterValue['ZetaE_Mid']),
	ZetaH = as.numeric(ParameterValue['ZetaH_Mid']),
	LambdaE = as.numeric(ParameterValue['LambdaE_Mid']),
	LambdaH = as.numeric(ParameterValue['LambdaH_Mid']),
	ChiEF1 = as.numeric(ParameterValue['ChiEF1_Mid']),
	ChiHF1 = as.numeric(ParameterValue['ChiHF1_Mid']),
	ChiHA1_Rich = as.numeric(ParameterValue['ChiHA1_RichMid']),
	ChiHA1_Poor = as.numeric(ParameterValue['ChiHA1_PoorMid']),
	ChiEF2 = as.numeric(ParameterValue['ChiEF2_Mid']),
	ChiHF2 = as.numeric(ParameterValue['ChiHF2_Mid']),
	ChiHA2_Rich = as.numeric(ParameterValue['ChiHA2_RichMid']),
	ChiHA2_Poor = as.numeric(ParameterValue['ChiHA2_PoorMid']),
	ChiHA3_Rich = as.numeric(ParameterValue['ChiHA3_RichMid']),
	ChiHA3_Poor = as.numeric(ParameterValue['ChiHA3_PoorMid'])
)

HealthEducationParms_High = c( 
	InitEconOutputGrowth = as.numeric(ParameterValue['InitEconOutputGrowth_High']),	
	ZetaE = as.numeric(ParameterValue['ZetaE_High']),
	ZetaH = as.numeric(ParameterValue['ZetaH_High']),
	LambdaE = as.numeric(ParameterValue['LambdaE_High']),
	LambdaH = as.numeric(ParameterValue['LambdaH_High']),
	ChiEF1 = as.numeric(ParameterValue['ChiEF1_High']),
	ChiHF1 = as.numeric(ParameterValue['ChiHF1_High']),
	ChiHA1_Rich = as.numeric(ParameterValue['ChiHA1_RichHigh']),
	ChiHA1_Poor = as.numeric(ParameterValue['ChiHA1_PoorHigh']),
	ChiEF2 = as.numeric(ParameterValue['ChiEF2_High']),
	ChiHF2 = as.numeric(ParameterValue['ChiHF2_High']),
	ChiHA2_Rich = as.numeric(ParameterValue['ChiHA2_RichHigh']),
	ChiHA2_Poor = as.numeric(ParameterValue['ChiHA2_PoorHigh']),
	ChiHA3_Rich = as.numeric(ParameterValue['ChiHA3_RichHigh']),
	ChiHA3_Poor = as.numeric(ParameterValue['ChiHA3_PoorHigh'])
)

################# DEFINE INITIAL CONDITIONS

HealthEducationInit_Low = c( 	
	EducationServices = as.numeric(InitValue['EducationServices_Low']),
	HealthServices = as.numeric(InitValue['HealthServices_Low'])
)

HealthEducationInit_Mid = c( 	
	EducationServices = as.numeric(InitValue['EducationServices_Mid']),
	HealthServices = as.numeric(InitValue['HealthServices_Mid'])
)

HealthEducationInit_High = c( 	
	EducationServices = as.numeric(InitValue['EducationServices_High']),
	HealthServices = as.numeric(InitValue['HealthServices_High'])
)

################# LOAD EXOGENOUS DATA

HealthEducationExog_Low = na.omit(cbind(
	time = CalibData['time'],
	RegPop = CalibData[,'LowPop'],
	EconOutput = CalibData[,'EconOutput_Low'],
	Inequality = CalibData[,'Inequality_Low']
))

HealthEducationExog_Mid = na.omit(cbind(
	time = CalibData['time'],
	RegPop = CalibData[,'MidPop'],
	EconOutput = CalibData[,'EconOutput_Mid'],
	Inequality = CalibData[,'Inequality_Mid']
))

HealthEducationExog_High = na.omit(cbind(
	time = CalibData['time'],
	RegPop = CalibData[,'HighPop'],
	EconOutput = CalibData[,'EconOutput_High'],
	Inequality = CalibData[,'Inequality_High']
))

################# LOAD ACTUAL DATA

HealthEducationActual_Low = na.omit(melt(
	data.frame(cbind(
		time = CalibData$time,
		FemaleHealthAccess = CalibData$FemaleHealthAccess_Low,
		GeneralHealthAccess_Rich = CalibData$GeneralHealthAccess_RichLow,
		GeneralHealthAccess_Poor = CalibData$GeneralHealthAccess_PoorLow,
		EducationServices = CalibData$EducationServices_Low, 
		HealthServices = CalibData$HealthServices_Low,
		FemaleEduAttain = CalibData$FemaleEduAttain_Low
	)),
	id ='time'))

HealthEducationActual_Mid = na.omit(melt(
	data.frame(cbind(
		time = CalibData$time,
		FemaleHealthAccess = CalibData$FemaleHealthAccess_Mid,
		GeneralHealthAccess_Rich = CalibData$GeneralHealthAccess_RichMid,
		GeneralHealthAccess_Poor = CalibData$GeneralHealthAccess_PoorMid,
		EducationServices = CalibData$EducationServices_Mid, 
		HealthServices = CalibData$HealthServices_Mid,
		FemaleEduAttain = CalibData$FemaleEduAttain_Mid
	)),
	id ='time'))

HealthEducationActual_High = na.omit(melt(
	data.frame(cbind(
		time = CalibData$time,
		FemaleHealthAccess = CalibData$FemaleHealthAccess_High,
		GeneralHealthAccess_Rich = CalibData$GeneralHealthAccess_RichHigh,
		GeneralHealthAccess_Poor = CalibData$GeneralHealthAccess_PoorHigh,
		EducationServices = CalibData$EducationServices_High, 
		HealthServices = CalibData$HealthServices_High,
		FemaleEduAttain = CalibData$FemaleEduAttain_High
	)),
	id ='time'))


HealthEducationActual_Low$variable = as.character(HealthEducationActual_Low$variable)
HealthEducationActual_Low = HealthEducationActual_Low[,c('variable','time','value')]
HealthEducationActual_Mid$variable = as.character(HealthEducationActual_Mid$variable)
HealthEducationActual_Mid = HealthEducationActual_Mid[,c('variable','time','value')]
HealthEducationActual_High$variable = as.character(HealthEducationActual_High$variable)
HealthEducationActual_High = HealthEducationActual_High[,c('variable','time','value')]

HealthEducationActual_Low$error[HealthEducationActual_Low$variable == 'FemaleHealthAccess'] =
	mean(HealthEducationActual_Low$value[HealthEducationActual_Low$variable == 'FemaleHealthAccess']) #* 
	# (1 - (HealthEducationActual_Low$time[HealthEducationActual_Low$variable == 'FemaleHealthAccess'] - 1979) ^ 2 / 1297)
HealthEducationActual_Mid$error[HealthEducationActual_Mid$variable == 'FemaleHealthAccess'] =
	mean(HealthEducationActual_Mid$value[HealthEducationActual_Mid$variable == 'FemaleHealthAccess']) #* 
	# (1 - (HealthEducationActual_Mid$time[HealthEducationActual_Mid$variable == 'FemaleHealthAccess'] - 1979) ^ 2 / 1297)
HealthEducationActual_High$error[HealthEducationActual_High$variable == 'FemaleHealthAccess'] =
	mean(HealthEducationActual_High$value[HealthEducationActual_High$variable == 'FemaleHealthAccess']) #* 
	# (1 - (HealthEducationActual_High$time[HealthEducationActual_High$variable == 'FemaleHealthAccess'] - 1979) ^ 2 / 1297)
HealthEducationActual_Low$error[HealthEducationActual_Low$variable == 'GeneralHealthAccess_Rich'] =
	mean(HealthEducationActual_Low$value[HealthEducationActual_Low$variable == 'GeneralHealthAccess_Rich']) #* 
	# (1 - (HealthEducationActual_Low$time[HealthEducationActual_Low$variable == 'GeneralHealthAccess_Rich'] - 1979) ^ 2 / 1297)
HealthEducationActual_Mid$error[HealthEducationActual_Mid$variable == 'GeneralHealthAccess_Rich'] =
	mean(HealthEducationActual_Mid$value[HealthEducationActual_Mid$variable == 'GeneralHealthAccess_Rich']) #* 
	# (1 - (HealthEducationActual_Mid$time[HealthEducationActual_Mid$variable == 'GeneralHealthAccess_Rich'] - 1979) ^ 2 / 1297)
HealthEducationActual_High$error[HealthEducationActual_High$variable == 'GeneralHealthAccess_Rich'] =
	mean(HealthEducationActual_High$value[HealthEducationActual_High$variable == 'GeneralHealthAccess_Rich']) #* 
	# (1 - (HealthEducationActual_High$time[HealthEducationActual_High$variable == 'GeneralHealthAccess_Rich'] - 1979) ^ 2 / 1297)
HealthEducationActual_Low$error[HealthEducationActual_Low$variable == 'GeneralHealthAccess_Poor'] =
	mean(HealthEducationActual_Low$value[HealthEducationActual_Low$variable == 'GeneralHealthAccess_Poor']) #* 
	# (1 - (HealthEducationActual_Low$time[HealthEducationActual_Low$variable == 'GeneralHealthAccess_Poor'] - 1979) ^ 2 / 1297)
HealthEducationActual_Mid$error[HealthEducationActual_Mid$variable == 'GeneralHealthAccess_Poor'] =
	mean(HealthEducationActual_Mid$value[HealthEducationActual_Mid$variable == 'GeneralHealthAccess_Poor']) #* 
	# (1 - (HealthEducationActual_Mid$time[HealthEducationActual_Mid$variable == 'GeneralHealthAccess_Poor'] - 1979) ^ 2 / 1297)
HealthEducationActual_High$error[HealthEducationActual_High$variable == 'GeneralHealthAccess_Poor'] =
	mean(HealthEducationActual_High$value[HealthEducationActual_High$variable == 'GeneralHealthAccess_Poor']) #* 
	# (1 - (HealthEducationActual_High$time[HealthEducationActual_High$variable == 'GeneralHealthAccess_Poor'] - 1979) ^ 2 / 1297)
HealthEducationActual_Low$error[HealthEducationActual_Low$variable == 'EducationServices'] =
	mean(HealthEducationActual_Low$value[HealthEducationActual_Low$variable == 'EducationServices']) #* 
	# (1 - (HealthEducationActual_Low$time[HealthEducationActual_Low$variable == 'EducationServices'] - 1979) ^ 2 / 1297)
HealthEducationActual_Mid$error[HealthEducationActual_Mid$variable == 'EducationServices'] =
	mean(HealthEducationActual_Mid$value[HealthEducationActual_Mid$variable == 'EducationServices']) #* 
	# (1 - (HealthEducationActual_Mid$time[HealthEducationActual_Mid$variable == 'EducationServices'] - 1979) ^ 2 / 1297)
HealthEducationActual_High$error[HealthEducationActual_High$variable == 'EducationServices'] =
	mean(HealthEducationActual_High$value[HealthEducationActual_High$variable == 'EducationServices']) #* 
	# (1 - (HealthEducationActual_High$time[HealthEducationActual_High$variable == 'EducationServices'] - 1979) ^ 2 / 1297)
HealthEducationActual_Low$error[HealthEducationActual_Low$variable == 'HealthServices'] =
	mean(HealthEducationActual_Low$value[HealthEducationActual_Low$variable == 'HealthServices']) #* 
	# (1 - (HealthEducationActual_Low$time[HealthEducationActual_Low$variable == 'HealthServices'] - 1979) ^ 2 / 1297)
HealthEducationActual_Mid$error[HealthEducationActual_Mid$variable == 'HealthServices'] =
	mean(HealthEducationActual_Mid$value[HealthEducationActual_Mid$variable == 'HealthServices']) #* 
	# (1 - (HealthEducationActual_Mid$time[HealthEducationActual_Mid$variable == 'HealthServices'] - 1979) ^ 2 / 1297)
HealthEducationActual_High$error[HealthEducationActual_High$variable == 'HealthServices'] =
	mean(HealthEducationActual_High$value[HealthEducationActual_High$variable == 'HealthServices']) #* 
	# (1 - (HealthEducationActual_High$time[HealthEducationActual_High$variable == 'HealthServices'] - 1979) ^ 2 / 1297)
HealthEducationActual_Low$error[HealthEducationActual_Low$variable == 'FemaleEduAttain'] =
	mean(HealthEducationActual_Low$value[HealthEducationActual_Low$variable == 'FemaleEduAttain']) #* 
	# (1 - (HealthEducationActual_Low$time[HealthEducationActual_Low$variable == 'FemaleEduAttain'] - 1979) ^ 2 / 1297)
HealthEducationActual_Mid$error[HealthEducationActual_Mid$variable == 'FemaleEduAttain'] =
	mean(HealthEducationActual_Mid$value[HealthEducationActual_Mid$variable == 'FemaleEduAttain']) #* 
	# (1 - (HealthEducationActual_Mid$time[HealthEducationActual_Mid$variable == 'FemaleEduAttain'] - 1979) ^ 2 / 1297)
HealthEducationActual_High$error[HealthEducationActual_High$variable == 'FemaleEduAttain'] =
	mean(HealthEducationActual_High$value[HealthEducationActual_High$variable == 'FemaleEduAttain']) #* 
	# (1 - (HealthEducationActual_High$time[HealthEducationActual_High$variable == 'FemaleEduAttain'] - 1979) ^ 2 / 1297)
################# DEFINE CALIBRATION PARAMETERS

HealthEducationCalibPars_Low =  rbind(
	ZetaE = as.numeric(ParameterData['ZetaE_Low',]),
	ZetaH = as.numeric(ParameterData['ZetaH_Low',]),
	LambdaE = as.numeric(ParameterData['LambdaE_Low',]),
	LambdaH = as.numeric(ParameterData['LambdaH_Low',]),
	ChiEF1 = as.numeric(ParameterData['ChiEF1_Low',]),
	ChiHF1 = as.numeric(ParameterData['ChiHF1_Low',]),
	ChiHA1_Rich = as.numeric(ParameterData['ChiHA1_RichLow',]),
	ChiHA1_Poor = as.numeric(ParameterData['ChiHA1_PoorLow',]),
	ChiEF2 = as.numeric(ParameterData['ChiEF2_Low',]),
	ChiHF2 = as.numeric(ParameterData['ChiHF2_Low',]),
	ChiHA2_Rich = as.numeric(ParameterData['ChiHA2_RichLow',]),
	ChiHA2_Poor = as.numeric(ParameterData['ChiHA2_PoorLow',]),
	ChiHA3_Rich = as.numeric(ParameterData['ChiHA3_RichLow',]),
	ChiHA3_Poor = as.numeric(ParameterData['ChiHA3_PoorLow',])
)
HealthEducationParValue_Low = HealthEducationCalibPars_Low[,1]
HealthEducationParMin_Low =  HealthEducationCalibPars_Low[,2]
HealthEducationParMax_Low = HealthEducationCalibPars_Low[,3]
HealthEducationParRange_Low = data.frame(
	min = HealthEducationParMin_Low,
	max = HealthEducationParMax_Low)

HealthEducationCalibPars_Mid =  rbind(
	ZetaE = as.numeric(ParameterData['ZetaE_Mid',]),
	ZetaH = as.numeric(ParameterData['ZetaH_Mid',]),
	LambdaE = as.numeric(ParameterData['LambdaE_Mid',]),
	LambdaH = as.numeric(ParameterData['LambdaH_Mid',]),
	ChiEF1 = as.numeric(ParameterData['ChiEF1_Mid',]),
	ChiHF1 = as.numeric(ParameterData['ChiHF1_Mid',]),
	ChiHA1_Rich = as.numeric(ParameterData['ChiHA1_RichMid',]),
	ChiHA1_Poor = as.numeric(ParameterData['ChiHA1_PoorMid',]),
	ChiEF2 = as.numeric(ParameterData['ChiEF2_Mid',]),
	ChiHF2 = as.numeric(ParameterData['ChiHF2_Mid',]),
	ChiHA2_Rich = as.numeric(ParameterData['ChiHA2_RichMid',]),
	ChiHA2_Poor = as.numeric(ParameterData['ChiHA2_PoorMid',]),
	ChiHA3_Rich = as.numeric(ParameterData['ChiHA3_RichMid',]),
	ChiHA3_Poor = as.numeric(ParameterData['ChiHA3_PoorMid',])
)
HealthEducationParValue_Mid = HealthEducationCalibPars_Mid[,1]
HealthEducationParMin_Mid =  HealthEducationCalibPars_Mid[,2]
HealthEducationParMax_Mid = HealthEducationCalibPars_Mid[,3]
HealthEducationParRange_Mid = data.frame(
	min = HealthEducationParMin_Mid,
	max = HealthEducationParMax_Mid)

HealthEducationCalibPars_High =  rbind(
	ZetaE = as.numeric(ParameterData['ZetaE_High',]),
	ZetaH = as.numeric(ParameterData['ZetaH_High',]),
	LambdaE = as.numeric(ParameterData['LambdaE_High',]),
	LambdaH = as.numeric(ParameterData['LambdaH_High',]),
	ChiEF1 = as.numeric(ParameterData['ChiEF1_High',]),
	ChiHF1 = as.numeric(ParameterData['ChiHF1_High',]),
	ChiHA1_Rich = as.numeric(ParameterData['ChiHA1_RichHigh',]),
	ChiHA1_Poor = as.numeric(ParameterData['ChiHA1_PoorHigh',]),
	ChiEF2 = as.numeric(ParameterData['ChiEF2_High',]),
	ChiHF2 = as.numeric(ParameterData['ChiHF2_High',]),
	ChiHA2_Rich = as.numeric(ParameterData['ChiHA2_RichHigh',]),
	ChiHA2_Poor = as.numeric(ParameterData['ChiHA2_PoorHigh',]),
	ChiHA3_Rich = as.numeric(ParameterData['ChiHA3_RichHigh',]),
	ChiHA3_Poor = as.numeric(ParameterData['ChiHA3_PoorHigh',])
)
HealthEducationParValue_High = HealthEducationCalibPars_High[,1]
HealthEducationParMin_High =  HealthEducationCalibPars_High[,2]
HealthEducationParMax_High = HealthEducationCalibPars_High[,3]
HealthEducationParRange_High = data.frame(
	min = HealthEducationParMin_High,
	max = HealthEducationParMax_High)


################# 2-STAGE FIT PARAMETERS

N = 1000
HealthEducationParStart_Low = Latinhyper(HealthEducationParRange_Low,N)
HealthEducationParStart_Mid = Latinhyper(HealthEducationParRange_Mid,N)
HealthEducationParStart_High = Latinhyper(HealthEducationParRange_High,N)
registerDoParallel(cl)
ptm = proc.time() 
HealthEducationResults_Low = foreach(i = 1:N,.packages='FME') %dopar%
{
	HealthEducationParValue_Low = HealthEducationParStart_Low[i,]
	
	HealthEducationFitPseudo_Low = HealthEducationFit(
		parvalue = HealthEducationParValue_Low,
		parmin = HealthEducationParMin_Low,
		parmax = HealthEducationParMax_Low,
		yactual = HealthEducationActual_Low,
		optmethod = 'Pseudo',
		# control = list(numiter = 100000),
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		exog = HealthEducationExog_Low,
		init = HealthEducationInit_Low,
		parms = HealthEducationParms_Low)

	HealthEducationFit_Low = HealthEducationFit(
		parvalue = coef(HealthEducationFitPseudo_Low),
		parmin = HealthEducationParMin_Low,
		parmax = HealthEducationParMax_Low,
		yactual = HealthEducationActual_Low,
		optmethod = 'Marq',
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		exog = HealthEducationExog_Low,
		init = HealthEducationInit_Low,
		parms = HealthEducationParms_Low)

	return(HealthEducationFit_Low)
}
ptm = proc.time()  - ptm
print(ptm)

ptm = proc.time() 
HealthEducationResults_Mid = foreach(i = 1:N,.packages='FME') %dopar%
{
	HealthEducationParValue_Mid = HealthEducationParStart_Mid[i,]

	HealthEducationFitPseudo_Mid = HealthEducationFit(
		parvalue = HealthEducationParValue_Mid,
		parmin = HealthEducationParMin_Mid,
		parmax = HealthEducationParMax_Mid,
		yactual = HealthEducationActual_Mid,
		optmethod = 'Pseudo',
		# control = list(numiter = 100000),
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		exog = HealthEducationExog_Mid,
		init = HealthEducationInit_Mid,
		parms = HealthEducationParms_Mid)

	HealthEducationFit_Mid = HealthEducationFit(
		parvalue = coef(HealthEducationFitPseudo_Mid),
		parmin = HealthEducationParMin_Mid,
		parmax = HealthEducationParMax_Mid,
		yactual = HealthEducationActual_Mid,
		optmethod = 'Marq',
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		exog = HealthEducationExog_Mid,
		init = HealthEducationInit_Mid,
		parms = HealthEducationParms_Mid)

	return(HealthEducationFit_Mid)
}
ptm = proc.time()  - ptm
print(ptm)

ptm = proc.time() 
HealthEducationResults_High = foreach(i = 1:N,.packages='FME') %dopar%
{
	HealthEducationParValue_High = HealthEducationParStart_High[i,]

	HealthEducationFitPseudo_High = HealthEducationFit(
		parvalue = HealthEducationParValue_High,
		parmin = HealthEducationParMin_High,
		parmax = HealthEducationParMax_High,
		yactual = HealthEducationActual_High,
		optmethod = 'Pseudo',
		# control = list(numiter = 100000),
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		exog = HealthEducationExog_High,
		init = HealthEducationInit_High,
		parms = HealthEducationParms_High)

	HealthEducationFit_High = HealthEducationFit(
		parvalue = coef(HealthEducationFitPseudo_High),
		parmin = HealthEducationParMin_High,
		parmax = HealthEducationParMax_High,
		yactual = HealthEducationActual_High,
		optmethod = 'Marq',
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		exog = HealthEducationExog_High,
		init = HealthEducationInit_High,
		parms = HealthEducationParms_High)

	return(HealthEducationFit_High)
}
ptm = proc.time()  - ptm
print(ptm)
stopCluster(cl)

################# PLOT FITTED VALUES

HealthEducationFitData_Low = CalibPlotFunc(HealthEducationResults_Low,
	HealthEducationActual_Low,HealthEducationParms_Low,HealthEducationExog_Low,
	HealthEducationInit_Low,HealthEducationMod,delta_t,delayyearlength,
	'HealthEducationSubmodel_LowIncome')

HealthEducationFitData_Mid = CalibPlotFunc(HealthEducationResults_Mid,
	HealthEducationActual_Mid,HealthEducationParms_Mid,HealthEducationExog_Mid,
	HealthEducationInit_Mid,HealthEducationMod,delta_t,delayyearlength,
	'HealthEducationSubmodel_MidIncome')

HealthEducationFitData_High = CalibPlotFunc(HealthEducationResults_High,
	HealthEducationActual_High,HealthEducationParms_High,HealthEducationExog_High,
	HealthEducationInit_High,HealthEducationMod,delta_t,delayyearlength,
	'HealthEducationSubmodel_HighIncome')

SSRCoefPlot(HealthEducationResults_Low,HealthEducationParStart_Low,'HealthEducationSubmodel_LowIncome')
SSRCoefPlot(HealthEducationResults_Mid,HealthEducationParStart_Mid,'HealthEducationSubmodel_MidIncome')
SSRCoefPlot(HealthEducationResults_High,HealthEducationParStart_High,'HealthEducationSubmodel_HighIncome')

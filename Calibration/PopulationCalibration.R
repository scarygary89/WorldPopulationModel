########################                       ########################
########################  POPULATION SUBMODEL  ########################
########################                       ########################

library(foreach)
library(doParallel)
library(GA)
numcore = detectCores()
cl<-makeCluster(numcore) 
print(cl)

################# SUBMODEL WITH EXOGENOUS INPUTS

PopulationMod = function(t0,tf,delta_t,delayyearlength,exog,init,parms) 
{	
	with(as.list(c(parms)), {	
		tspan = seq(from=t0, to=tf, by=delta_t)
		aux_names = c(
			'MortRate_M1Rich',
			'MortRate_M2Rich',
			'MortRate_M3Rich',
			'MortRate_M4Rich',
			'MortRate_F1Rich',
			'MortRate_F2Rich',
			'MortRate_F3Rich',
			'MortRate_F4Rich',
			'MortRate_M1Poor',
			'MortRate_M2Poor',
			'MortRate_M3Poor',
			'MortRate_M4Poor',
			'MortRate_F1Poor',
			'MortRate_F2Poor',
			'MortRate_F3Poor',
			'MortRate_F4Poor',
			'GFR',
			'Births',
			'Deaths_M1',
			'Deaths_M2',
			'Deaths_M3',
			'Deaths_M4',
			'Deaths_F1',
			'Deaths_F2',
			'Deaths_F3',
			'Deaths_F4'
		)
		AuxData = matrix(NA,
			nrow = (length(tspan)),
			ncol = length(aux_names)
			)		
		colnames(AuxData) = aux_names
		StockData = matrix(NA,
			nrow = (length(tspan) + 1),
			ncol = length(init))
		colnames(StockData) = names(init)	
		stocks = init
		StockData[1,] = stocks
		for(i in 1:length(tspan)) {
			RegPop_ij = c(
					M1 = as.numeric(stocks['M1']),
					M2 = as.numeric(stocks['M2']),
					M3 = as.numeric(stocks['M3']),
					M4 = as.numeric(stocks['M4']),
					F1 = as.numeric(stocks['F1']),
					F2 = as.numeric(stocks['F2']),
					F3 = as.numeric(stocks['F3']),
					F4 = as.numeric(stocks['F4']))

			# Regional Population System 
			GeneralHealthAccess = c(
				Rich = exog[i,'GeneralHealthAccess_Rich'],
				Poor = exog[i,'GeneralHealthAccess_Poor'])

			NutritionConsumptionPC = c(
				Rich = exog[i,'NutritionConsumptionPC'],
				Poor = exog[i,'NutritionConsumptionPC'])

			PopOut = Population(
				RegPop_ij,
				exog[i,'FemaleEduAttain'],
				exog[i,'FemaleHealthAccess'],
				GeneralHealthAccess,
				NutritionConsumptionPC,
				MinDeath_RichM1,
				MinDeath_PoorM1,
				MinDeath_M2,
				MinDeath_M3,
				MinDeath_M4,
				MinDeath_RichF1,
				MinDeath_PoorF1,
				MinDeath_F2,
				MinDeath_F3,
				MinDeath_F4,
				OmegaH_RichM1,
				OmegaH_PoorM1,
				OmegaH_M2,
				OmegaH_M3,
				OmegaH_M4,
				OmegaH_RichF1,
				OmegaH_PoorF1,
				OmegaH_F2,
				OmegaH_F3,
				OmegaH_F4,
				OmegaF_RichM1,
				OmegaF_PoorM1,
				OmegaF_M2,
				OmegaF_M3,
				OmegaF_M4,
				OmegaF_RichF1,
				OmegaF_PoorF1,
				OmegaF_F2,
				OmegaF_F3,
				OmegaF_F4,
				AlphaGFR,
				BetaE,
				BetaH,
				parms)

			################ STORE OUTPUT

			# AUXILIARY VARIABLES
			aux = c(
				PopOut[['MortRate_ijk']],
				PopOut[['GFR']],
				PopOut[['Births_ij']],
				PopOut[['Deaths_ij']]
			)
			AuxData[i,] = aux

			# STOCK VARIABLES
			dstocks = c(
				PopOut[["dPop_ij"]]
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

PopulationCost = function(p,t0,tf,delta_t,delayyearlength,exog,init,parms,yactual)
{
	whichpar = names(parms)[names(parms) %in% names(p)]
	whichinit = names(init)[names(init) %in% names(p)]
	parms[whichpar] = p[whichpar]
	init[whichinit] = p[whichinit]
	ysim = PopulationMod(t0,tf,delta_t,delayyearlength,exog,init,parms)
	RunCost = modCost(ysim,yactual,x = 'time',y = 'value',err = 'error',
		scaleVar=T)
	return(RunCost)
}

################# PARAMETER ESTIMATION FUNCTION

PopulationFit = function(parvalue,parmin,parmax,yactual,optmethod,control=list(),
	delta_t,delayyearlength,exog,init,parms)
{
	t0 = min(exog$time)
	tf = max(exog$time)
	ptm = proc.time() 
	PopFit = modFit(
		f = PopulationCost,
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
	return(PopFit)
}

################# DEFINE INITIAL PARAMETER VALUES

PopParms_Low = c( 	
	MinDeath_RichM1 = as.numeric(ParameterValue['MinDeath_RichM1Low']),
	MinDeath_PoorM1 = as.numeric(ParameterValue['MinDeath_PoorM1Low']),
	MinDeath_M2 = as.numeric(ParameterValue['MinDeath_M2Low']),
	MinDeath_M3 = as.numeric(ParameterValue['MinDeath_M3Low']),
	MinDeath_M4 = as.numeric(ParameterValue['MinDeath_M4Low']),
	MinDeath_RichF1 = as.numeric(ParameterValue['MinDeath_RichF1Low']),
	MinDeath_PoorF1 = as.numeric(ParameterValue['MinDeath_PoorF1Low']),
	MinDeath_F2 = as.numeric(ParameterValue['MinDeath_F2Low']),
	MinDeath_F3 = as.numeric(ParameterValue['MinDeath_F3Low']),
	MinDeath_F4 = as.numeric(ParameterValue['MinDeath_F4Low']),
	OmegaH_RichM1 = as.numeric(ParameterValue['OmegaH_RichM1Low']),
	OmegaH_PoorM1 = as.numeric(ParameterValue['OmegaH_PoorM1Low']),
	OmegaH_M2 = as.numeric(ParameterValue['OmegaH_M2Low']),
	OmegaH_M3 = as.numeric(ParameterValue['OmegaH_M3Low']),
	OmegaH_M4 = as.numeric(ParameterValue['OmegaH_M4Low']),
	OmegaH_RichF1 = as.numeric(ParameterValue['OmegaH_RichF1Low']),
	OmegaH_PoorF1 = as.numeric(ParameterValue['OmegaH_PoorF1Low']),
	OmegaH_F2 = as.numeric(ParameterValue['OmegaH_F2Low']),
	OmegaH_F3 = as.numeric(ParameterValue['OmegaH_F3Low']),
	OmegaH_F4 = as.numeric(ParameterValue['OmegaH_F4Low']),
	OmegaF_RichM1 = as.numeric(ParameterValue['OmegaF_RichM1Low']),
	OmegaF_PoorM1 = as.numeric(ParameterValue['OmegaF_PoorM1Low']),
	OmegaF_M2 = as.numeric(ParameterValue['OmegaF_M2Low']),
	OmegaF_M3 = as.numeric(ParameterValue['OmegaF_M3Low']),
	OmegaF_M4 = as.numeric(ParameterValue['OmegaF_M4Low']),
	OmegaF_RichF1 = as.numeric(ParameterValue['OmegaF_RichF1Low']),
	OmegaF_PoorF1 = as.numeric(ParameterValue['OmegaF_PoorF1Low']),
	OmegaF_F2 = as.numeric(ParameterValue['OmegaF_F2Low']),
	OmegaF_F3 = as.numeric(ParameterValue['OmegaF_F3Low']),
	OmegaF_F4 = as.numeric(ParameterValue['OmegaF_F4Low']),
	AlphaGFR = as.numeric(ParameterValue['AlphaGFR_Low']),
	BetaE = as.numeric(ParameterValue['BetaE_Low']),
	BetaH = as.numeric(ParameterValue['BetaH_Low']),
	FemaleBirthRatio = as.numeric(ParameterValue['FemaleBirthRatio']),
	NutritionReq = as.numeric(ParameterValue['NutritionReq']),
	PoorFrac = as.numeric(ParameterValue['PoorFrac'])
)

PopParms_Mid = c( 	
	MinDeath_RichM1 = as.numeric(ParameterValue['MinDeath_RichM1Mid']),
	MinDeath_PoorM1 = as.numeric(ParameterValue['MinDeath_PoorM1Mid']),
	MinDeath_M2 = as.numeric(ParameterValue['MinDeath_M2Mid']),
	MinDeath_M3 = as.numeric(ParameterValue['MinDeath_M3Mid']),
	MinDeath_M4 = as.numeric(ParameterValue['MinDeath_M4Mid']),
	MinDeath_RichF1 = as.numeric(ParameterValue['MinDeath_RichF1Mid']),
	MinDeath_PoorF1 = as.numeric(ParameterValue['MinDeath_PoorF1Mid']),
	MinDeath_F2 = as.numeric(ParameterValue['MinDeath_F2Mid']),
	MinDeath_F3 = as.numeric(ParameterValue['MinDeath_F3Mid']),
	MinDeath_F4 = as.numeric(ParameterValue['MinDeath_F4Mid']),
	OmegaH_RichM1 = as.numeric(ParameterValue['OmegaH_RichM1Mid']),
	OmegaH_PoorM1 = as.numeric(ParameterValue['OmegaH_PoorM1Mid']),
	OmegaH_M2 = as.numeric(ParameterValue['OmegaH_M2Mid']),
	OmegaH_M3 = as.numeric(ParameterValue['OmegaH_M3Mid']),
	OmegaH_M4 = as.numeric(ParameterValue['OmegaH_M4Mid']),
	OmegaH_RichF1 = as.numeric(ParameterValue['OmegaH_RichF1Mid']),
	OmegaH_PoorF1 = as.numeric(ParameterValue['OmegaH_PoorF1Mid']),
	OmegaH_F2 = as.numeric(ParameterValue['OmegaH_F2Mid']),
	OmegaH_F3 = as.numeric(ParameterValue['OmegaH_F3Mid']),
	OmegaH_F4 = as.numeric(ParameterValue['OmegaH_F4Mid']),
	OmegaF_RichM1 = as.numeric(ParameterValue['OmegaF_RichM1Mid']),
	OmegaF_PoorM1 = as.numeric(ParameterValue['OmegaF_PoorM1Mid']),
	OmegaF_M2 = as.numeric(ParameterValue['OmegaF_M2Mid']),
	OmegaF_M3 = as.numeric(ParameterValue['OmegaF_M3Mid']),
	OmegaF_M4 = as.numeric(ParameterValue['OmegaF_M4Mid']),
	OmegaF_RichF1 = as.numeric(ParameterValue['OmegaF_RichF1Mid']),
	OmegaF_PoorF1 = as.numeric(ParameterValue['OmegaF_PoorF1Mid']),
	OmegaF_F2 = as.numeric(ParameterValue['OmegaF_F2Mid']),
	OmegaF_F3 = as.numeric(ParameterValue['OmegaF_F3Mid']),
	OmegaF_F4 = as.numeric(ParameterValue['OmegaF_F4Mid']),
	AlphaGFR = as.numeric(ParameterValue['AlphaGFR_Mid']),
	BetaE = as.numeric(ParameterValue['BetaE_Mid']),
	BetaH = as.numeric(ParameterValue['BetaH_Mid']),
	FemaleBirthRatio = as.numeric(ParameterValue['FemaleBirthRatio']),
	NutritionReq = as.numeric(ParameterValue['NutritionReq']),
	PoorFrac = as.numeric(ParameterValue['PoorFrac'])
)

PopParms_High = c( 	
	MinDeath_RichM1 = as.numeric(ParameterValue['MinDeath_RichM1High']),
	MinDeath_PoorM1 = as.numeric(ParameterValue['MinDeath_PoorM1High']),
	MinDeath_M2 = as.numeric(ParameterValue['MinDeath_M2High']),
	MinDeath_M3 = as.numeric(ParameterValue['MinDeath_M3High']),
	MinDeath_M4 = as.numeric(ParameterValue['MinDeath_M4High']),
	MinDeath_RichF1 = as.numeric(ParameterValue['MinDeath_RichF1High']),
	MinDeath_PoorF1 = as.numeric(ParameterValue['MinDeath_PoorF1High']),
	MinDeath_F2 = as.numeric(ParameterValue['MinDeath_F2High']),
	MinDeath_F3 = as.numeric(ParameterValue['MinDeath_F3High']),
	MinDeath_F4 = as.numeric(ParameterValue['MinDeath_F4High']),
	OmegaH_RichM1 = as.numeric(ParameterValue['OmegaH_RichM1High']),
	OmegaH_PoorM1 = as.numeric(ParameterValue['OmegaH_PoorM1High']),
	OmegaH_M2 = as.numeric(ParameterValue['OmegaH_M2High']),
	OmegaH_M3 = as.numeric(ParameterValue['OmegaH_M3High']),
	OmegaH_M4 = as.numeric(ParameterValue['OmegaH_M4High']),
	OmegaH_RichF1 = as.numeric(ParameterValue['OmegaH_RichF1High']),
	OmegaH_PoorF1 = as.numeric(ParameterValue['OmegaH_PoorF1High']),
	OmegaH_F2 = as.numeric(ParameterValue['OmegaH_F2High']),
	OmegaH_F3 = as.numeric(ParameterValue['OmegaH_F3High']),
	OmegaH_F4 = as.numeric(ParameterValue['OmegaH_F4High']),
	OmegaF_RichM1 = as.numeric(ParameterValue['OmegaF_RichM1High']),
	OmegaF_PoorM1 = as.numeric(ParameterValue['OmegaF_PoorM1High']),
	OmegaF_M2 = as.numeric(ParameterValue['OmegaF_M2High']),
	OmegaF_M3 = as.numeric(ParameterValue['OmegaF_M3High']),
	OmegaF_M4 = as.numeric(ParameterValue['OmegaF_M4High']),
	OmegaF_RichF1 = as.numeric(ParameterValue['OmegaF_RichF1High']),
	OmegaF_PoorF1 = as.numeric(ParameterValue['OmegaF_PoorF1High']),
	OmegaF_F2 = as.numeric(ParameterValue['OmegaF_F2High']),
	OmegaF_F3 = as.numeric(ParameterValue['OmegaF_F3High']),
	OmegaF_F4 = as.numeric(ParameterValue['OmegaF_F4High']),
	AlphaGFR = as.numeric(ParameterValue['AlphaGFR_High']),
	BetaE = as.numeric(ParameterValue['BetaE_High']),
	BetaH = as.numeric(ParameterValue['BetaH_High']),
	FemaleBirthRatio = as.numeric(ParameterValue['FemaleBirthRatio']),
	NutritionReq = as.numeric(ParameterValue['NutritionReq']),
	PoorFrac = as.numeric(ParameterValue['PoorFrac'])
)

################# DEFINE INITIAL CONDITIONS

PopInit_Low = c( 	
	M1 = as.numeric(InitValue['Low_M1']),
	M2 = as.numeric(InitValue['Low_M2']),
	M3 = as.numeric(InitValue['Low_M3']),
	M4 = as.numeric(InitValue['Low_M4']),
	F1 = as.numeric(InitValue['Low_F1']),
	F2 = as.numeric(InitValue['Low_F2']),
	F3 = as.numeric(InitValue['Low_F3']),
	F4 = as.numeric(InitValue['Low_F4'])
)

PopInit_Mid = c(
	M1 = as.numeric(InitValue['Mid_M1']),
	M2 = as.numeric(InitValue['Mid_M2']),
	M3 = as.numeric(InitValue['Mid_M3']),
	M4 = as.numeric(InitValue['Mid_M4']),
	F1 = as.numeric(InitValue['Mid_F1']),
	F2 = as.numeric(InitValue['Mid_F2']),
	F3 = as.numeric(InitValue['Mid_F3']),
	F4 = as.numeric(InitValue['Mid_F4'])
)

PopInit_High = c(
	M1 = as.numeric(InitValue['High_M1']),
	M2 = as.numeric(InitValue['High_M2']),
	M3 = as.numeric(InitValue['High_M3']),
	M4 = as.numeric(InitValue['High_M4']),
	F1 = as.numeric(InitValue['High_F1']),
	F2 = as.numeric(InitValue['High_F2']),
	F3 = as.numeric(InitValue['High_F3']),
	F4 = as.numeric(InitValue['High_F4'])
)

################# LOAD EXOGENOUS DATA

PopExog_Low = na.omit(cbind(
	time = CalibData['time'],
	GeneralHealthAccess_Rich = CalibData[,'GeneralHealthAccess_RichLow'],
	GeneralHealthAccess_Poor = CalibData[,'GeneralHealthAccess_PoorLow'],
	FemaleHealthAccess = CalibData[,'FemaleHealthAccess_Low'],
	FemaleEduAttain = CalibData[,'FemaleEduAttain_Low'],
	NutritionConsumptionPC = CalibData[,'NutritionConsumptionPC_Low'],
	EconOutput = CalibData[,'EconOutput_Low'],
	Inequality = CalibData[,'Inequality_Low']
))
PopExog_Mid = na.omit(cbind(
	time = CalibData['time'],
	GeneralHealthAccess_Rich = CalibData[,'GeneralHealthAccess_RichMid'],
	GeneralHealthAccess_Poor = CalibData[,'GeneralHealthAccess_PoorMid'],
	FemaleHealthAccess = CalibData[,'FemaleHealthAccess_Mid'],
	FemaleEduAttain = CalibData[,'FemaleEduAttain_Mid'],
	NutritionConsumptionPC = CalibData[,'NutritionConsumptionPC_Mid'],
	EconOutput = CalibData[,'EconOutput_Mid'],
	Inequality = CalibData[,'Inequality_Mid']
))
PopExog_High = na.omit(cbind(
	time = CalibData['time'],
	GeneralHealthAccess_Rich = CalibData[,'GeneralHealthAccess_RichHigh'],
	GeneralHealthAccess_Poor = CalibData[,'GeneralHealthAccess_PoorHigh'],
	FemaleHealthAccess = CalibData[,'FemaleHealthAccess_High'],
	FemaleEduAttain = CalibData[,'FemaleEduAttain_High'],
	NutritionConsumptionPC = CalibData[,'NutritionConsumptionPC_High'],
	EconOutput = CalibData[,'EconOutput_High'],
	Inequality = CalibData[,'Inequality_High']
))

################# LOAD ACTUAL DATA

PopActual_Low = na.omit(melt(
	data.frame(cbind(
		time = CalibData$time,
		# M1 = CalibData$Low_M1,
		# M3 = CalibData$Low_M3,
		# M2 = CalibData$Low_M2,
		# M4 = CalibData$Low_M4,
		# F1 = CalibData$Low_F1,
		# F2 = CalibData$Low_F2,
		# F3 = CalibData$Low_F3,
		# F4 = CalibData$Low_F4,
		Births = CalibData$Births_Low,
		Deaths_M1 = CalibData$Deaths_LowM1,
		Deaths_M2 = CalibData$Deaths_LowM2,
		Deaths_M3 = CalibData$Deaths_LowM3,
		Deaths_M4 = CalibData$Deaths_LowM4,
		Deaths_F1 = CalibData$Deaths_LowF1,
		Deaths_F2 = CalibData$Deaths_LowF2,
		Deaths_F3 = CalibData$Deaths_LowF3,
		Deaths_F4 = CalibData$Deaths_LowF4
	)),
	id ='time'))
PopActual_Low$variable = as.character(PopActual_Low$variable)
PopActual_Low = PopActual_Low[,c('variable','time','value')]

PopActual_Mid = na.omit(melt(
	data.frame(cbind(
		time = CalibData$time,
		# M1 = CalibData$Mid_M1,
		# M3 = CalibData$Mid_M3,
		# M2 = CalibData$Mid_M2,
		# M4 = CalibData$Mid_M4,
		# F1 = CalibData$Mid_F1,
		# F2 = CalibData$Mid_F2,
		# F3 = CalibData$Mid_F3,
		# F4 = CalibData$Mid_F4,
		Births = CalibData$Births_Mid,
		Deaths_M1 = CalibData$Deaths_MidM1,
		Deaths_M2 = CalibData$Deaths_MidM2,
		Deaths_M3 = CalibData$Deaths_MidM3,
		Deaths_M4 = CalibData$Deaths_MidM4,
		Deaths_F1 = CalibData$Deaths_MidF1,
		Deaths_F2 = CalibData$Deaths_MidF2,
		Deaths_F3 = CalibData$Deaths_MidF3,
		Deaths_F4 = CalibData$Deaths_MidF4
	)),
	id ='time'))
PopActual_Mid$variable = as.character(PopActual_Mid$variable)
PopActual_Mid = PopActual_Mid[,c('variable','time','value')]

PopActual_High = na.omit(melt(
	data.frame(cbind(		
		time = CalibData$time,
		# M1 = CalibData$High_M1,
		# M3 = CalibData$High_M3,
		# M2 = CalibData$High_M2,
		# M4 = CalibData$High_M4,
		# F1 = CalibData$High_F1,
		# F2 = CalibData$High_F2,
		# F3 = CalibData$High_F3,
		# F4 = CalibData$High_F4,					
		Births = CalibData$Births_High,
		Deaths_M1 = CalibData$Deaths_HighM1,
		Deaths_M2 = CalibData$Deaths_HighM2,
		Deaths_M3 = CalibData$Deaths_HighM3,
		Deaths_M4 = CalibData$Deaths_HighM4,
		Deaths_F1 = CalibData$Deaths_HighF1,
		Deaths_F2 = CalibData$Deaths_HighF2,
		Deaths_F3 = CalibData$Deaths_HighF3,
		Deaths_F4 = CalibData$Deaths_HighF4
	)),
	id ='time'))
PopActual_High$variable = as.character(PopActual_High$variable)
PopActual_High = PopActual_High[,c('variable','time','value')]

# PopActual_Low$error[PopActual_Low$variable == 'M1'] =
# 	mean(PopActual_Low$value[PopActual_Low$variable == 'M1']) #* 
# 	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'M1'] - 1979) ^ 2 / 1297)
# PopActual_Low$error[PopActual_Low$variable == 'M2'] =
# 	mean(PopActual_Low$value[PopActual_Low$variable == 'M2']) #* 
# 	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'M2'] - 1979) ^ 2 / 1297)
# PopActual_Low$error[PopActual_Low$variable == 'M3'] =
# 	mean(PopActual_Low$value[PopActual_Low$variable == 'M3']) #* 
# 	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'M3'] - 1979) ^ 2 / 1297)
# PopActual_Low$error[PopActual_Low$variable == 'M4'] =
# 	mean(PopActual_Low$value[PopActual_Low$variable == 'M4']) #* 
# 	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'M4'] - 1979) ^ 2 / 1297)
# PopActual_Low$error[PopActual_Low$variable == 'F1'] =
# 	mean(PopActual_Low$value[PopActual_Low$variable == 'F1']) #* 
# 	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'F1'] - 1979) ^ 2 / 1297)
# PopActual_Low$error[PopActual_Low$variable == 'F2'] =
# 	mean(PopActual_Low$value[PopActual_Low$variable == 'F2']) #* 
# 	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'F2'] - 1979) ^ 2 / 1297)
# PopActual_Low$error[PopActual_Low$variable == 'F3'] =
# 	mean(PopActual_Low$value[PopActual_Low$variable == 'F3']) #* 
# 	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'F3'] - 1979) ^ 2 / 1297)
# PopActual_Low$error[PopActual_Low$variable == 'F4'] =
# 	mean(PopActual_Low$value[PopActual_Low$variable == 'F4']) #* 
# 	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'F4'] - 1979) ^ 2 / 1297)
PopActual_Low$error[PopActual_Low$variable == 'Births'] =
	mean(PopActual_Low$value[PopActual_Low$variable == 'Births']) #* 
	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'Births'] - 1979) ^ 2 / 1297)
PopActual_Low$error[PopActual_Low$variable == 'Deaths_M1'] =
	mean(PopActual_Low$value[PopActual_Low$variable == 'Deaths_M1']) #* 
	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'Deaths_M1'] - 1979) ^ 2 / 1297)
PopActual_Low$error[PopActual_Low$variable == 'Deaths_M2'] =
	mean(PopActual_Low$value[PopActual_Low$variable == 'Deaths_M2']) #* 
	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'Deaths_M2'] - 1979) ^ 2 / 1297)
PopActual_Low$error[PopActual_Low$variable == 'Deaths_M3'] =
	mean(PopActual_Low$value[PopActual_Low$variable == 'Deaths_M3']) #* 
	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'Deaths_M3'] - 1979) ^ 2 / 1297)
PopActual_Low$error[PopActual_Low$variable == 'Deaths_M4'] =
	mean(PopActual_Low$value[PopActual_Low$variable == 'Deaths_M4']) #* 
	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'Deaths_M4'] - 1979) ^ 2 / 1297)
PopActual_Low$error[PopActual_Low$variable == 'Deaths_F1'] =
	mean(PopActual_Low$value[PopActual_Low$variable == 'Deaths_F1']) #* 
	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'Deaths_F1'] - 1979) ^ 2 / 1297)
PopActual_Low$error[PopActual_Low$variable == 'Deaths_F2'] =
	mean(PopActual_Low$value[PopActual_Low$variable == 'Deaths_F2']) #* 
	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'Deaths_F2'] - 1979) ^ 2 / 1297)
PopActual_Low$error[PopActual_Low$variable == 'Deaths_F3'] =
	mean(PopActual_Low$value[PopActual_Low$variable == 'Deaths_F3']) #* 
	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'Deaths_F3'] - 1979) ^ 2 / 1297)
PopActual_Low$error[PopActual_Low$variable == 'Deaths_F4'] =
	mean(PopActual_Low$value[PopActual_Low$variable == 'Deaths_F4']) #* 
	# (1 - (PopActual_Low$time[PopActual_Low$variable == 'Deaths_F4'] - 1979) ^ 2 / 1297)


# PopActual_Mid$error[PopActual_Mid$variable == 'M1'] =
# 	mean(PopActual_Mid$value[PopActual_Mid$variable == 'M1']) #* 
# 	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'M1'] - 1979) ^ 2 / 1297)
# PopActual_Mid$error[PopActual_Mid$variable == 'M2'] =
# 	mean(PopActual_Mid$value[PopActual_Mid$variable == 'M2']) #* 
# 	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'M2'] - 1979) ^ 2 / 1297)
# PopActual_Mid$error[PopActual_Mid$variable == 'M3'] =
# 	mean(PopActual_Mid$value[PopActual_Mid$variable == 'M3']) #* 
# 	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'M3'] - 1979) ^ 2 / 1297)
# PopActual_Mid$error[PopActual_Mid$variable == 'M4'] =
# 	mean(PopActual_Mid$value[PopActual_Mid$variable == 'M4']) #* 
# 	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'M4'] - 1979) ^ 2 / 1297)
# PopActual_Mid$error[PopActual_Mid$variable == 'F1'] =
# 	mean(PopActual_Mid$value[PopActual_Mid$variable == 'F1']) #* 
# 	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'F1'] - 1979) ^ 2 / 1297)
# PopActual_Mid$error[PopActual_Mid$variable == 'F2'] =
# 	mean(PopActual_Mid$value[PopActual_Mid$variable == 'F2']) #* 
# 	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'F2'] - 1979) ^ 2 / 1297)
# PopActual_Mid$error[PopActual_Mid$variable == 'F3'] =
# 	mean(PopActual_Mid$value[PopActual_Mid$variable == 'F3']) #* 
# 	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'F3'] - 1979) ^ 2 / 1297)
# PopActual_Mid$error[PopActual_Mid$variable == 'F4'] =
# 	mean(PopActual_Mid$value[PopActual_Mid$variable == 'F4']) #* 
# 	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'F4'] - 1979) ^ 2 / 1297)
PopActual_Mid$error[PopActual_Mid$variable == 'Births'] =
	mean(PopActual_Mid$value[PopActual_Mid$variable == 'Births']) #* 
	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'Births'] - 1979) ^ 2 / 1297)
PopActual_Mid$error[PopActual_Mid$variable == 'Deaths_M1'] =
	mean(PopActual_Mid$value[PopActual_Mid$variable == 'Deaths_M1']) #* 
	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'Deaths_M1'] - 1979) ^ 2 / 1297)
PopActual_Mid$error[PopActual_Mid$variable == 'Deaths_M2'] =
	mean(PopActual_Mid$value[PopActual_Mid$variable == 'Deaths_M2']) #* 
	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'Deaths_M2'] - 1979) ^ 2 / 1297)
PopActual_Mid$error[PopActual_Mid$variable == 'Deaths_M3'] =
	mean(PopActual_Mid$value[PopActual_Mid$variable == 'Deaths_M3']) #* 
	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'Deaths_M3'] - 1979) ^ 2 / 1297)
PopActual_Mid$error[PopActual_Mid$variable == 'Deaths_M4'] =
	mean(PopActual_Mid$value[PopActual_Mid$variable == 'Deaths_M4']) #* 
	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'Deaths_M4'] - 1979) ^ 2 / 1297)
PopActual_Mid$error[PopActual_Mid$variable == 'Deaths_F1'] =
	mean(PopActual_Mid$value[PopActual_Mid$variable == 'Deaths_F1']) #* 
	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'Deaths_F1'] - 1979) ^ 2 / 1297)
PopActual_Mid$error[PopActual_Mid$variable == 'Deaths_F2'] =
	mean(PopActual_Mid$value[PopActual_Mid$variable == 'Deaths_F2']) #* 
	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'Deaths_F2'] - 1979) ^ 2 / 1297)
PopActual_Mid$error[PopActual_Mid$variable == 'Deaths_F3'] =
	mean(PopActual_Mid$value[PopActual_Mid$variable == 'Deaths_F3']) #* 
	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'Deaths_F3'] - 1979) ^ 2 / 1297)
PopActual_Mid$error[PopActual_Mid$variable == 'Deaths_F4'] =
	mean(PopActual_Mid$value[PopActual_Mid$variable == 'Deaths_F4']) #* 
	# (1 - (PopActual_Mid$time[PopActual_Mid$variable == 'Deaths_F4'] - 1979) ^ 2 / 1297)



# PopActual_High$error[PopActual_High$variable == 'M1'] =
# 	mean(PopActual_High$value[PopActual_High$variable == 'M1']) #* 
# 	# (1 - (PopActual_High$time[PopActual_High$variable == 'M1'] - 1979) ^ 2 / 1297)
# PopActual_High$error[PopActual_High$variable == 'M2'] =
# 	mean(PopActual_High$value[PopActual_High$variable == 'M2']) #* 
# 	# (1 - (PopActual_High$time[PopActual_High$variable == 'M2'] - 1979) ^ 2 / 1297)
# PopActual_High$error[PopActual_High$variable == 'M3'] =
# 	mean(PopActual_High$value[PopActual_High$variable == 'M3']) #* 
# 	# (1 - (PopActual_High$time[PopActual_High$variable == 'M3'] - 1979) ^ 2 / 1297)
# PopActual_High$error[PopActual_High$variable == 'M4'] =
# 	mean(PopActual_High$value[PopActual_High$variable == 'M4']) #* 
# 	# (1 - (PopActual_High$time[PopActual_High$variable == 'M4'] - 1979) ^ 2 / 1297)
# PopActual_High$error[PopActual_High$variable == 'F1'] =
# 	mean(PopActual_High$value[PopActual_High$variable == 'F1']) #* 
# 	# (1 - (PopActual_High$time[PopActual_High$variable == 'F1'] - 1979) ^ 2 / 1297)
# PopActual_High$error[PopActual_High$variable == 'F2'] =
# 	mean(PopActual_High$value[PopActual_High$variable == 'F2']) #* 
# 	# (1 - (PopActual_High$time[PopActual_High$variable == 'F2'] - 1979) ^ 2 / 1297)
# PopActual_High$error[PopActual_High$variable == 'F3'] =
# 	mean(PopActual_High$value[PopActual_High$variable == 'F3']) #* 
# 	# (1 - (PopActual_High$time[PopActual_High$variable == 'F3'] - 1979) ^ 2 / 1297)
# PopActual_High$error[PopActual_High$variable == 'F4'] =
# 	mean(PopActual_High$value[PopActual_High$variable == 'F4']) #* 
# 	# (1 - (PopActual_High$time[PopActual_High$variable == 'F4'] - 1979) ^ 2 / 1297)
PopActual_High$error[PopActual_High$variable == 'Births'] =
	mean(PopActual_High$value[PopActual_High$variable == 'Births']) #* 
	# (1 - (PopActual_High$time[PopActual_High$variable == 'Births'] - 1979) ^ 2 / 1297)
PopActual_High$error[PopActual_High$variable == 'Deaths_M1'] =
	mean(PopActual_High$value[PopActual_High$variable == 'Deaths_M1']) #* 
	# (1 - (PopActual_High$time[PopActual_High$variable == 'Deaths_M1'] - 1979) ^ 2 / 1297)
PopActual_High$error[PopActual_High$variable == 'Deaths_M2'] =
	mean(PopActual_High$value[PopActual_High$variable == 'Deaths_M2']) #* 
	# (1 - (PopActual_High$time[PopActual_High$variable == 'Deaths_M2'] - 1979) ^ 2 / 1297)
PopActual_High$error[PopActual_High$variable == 'Deaths_M3'] =
	mean(PopActual_High$value[PopActual_High$variable == 'Deaths_M3']) #* 
	# (1 - (PopActual_High$time[PopActual_High$variable == 'Deaths_M3'] - 1979) ^ 2 / 1297)
PopActual_High$error[PopActual_High$variable == 'Deaths_M4'] =
	mean(PopActual_High$value[PopActual_High$variable == 'Deaths_M4']) #* 
	# (1 - (PopActual_High$time[PopActual_High$variable == 'Deaths_M4'] - 1979) ^ 2 / 1297)
PopActual_High$error[PopActual_High$variable == 'Deaths_F1'] =
	mean(PopActual_High$value[PopActual_High$variable == 'Deaths_F1']) #* 
	# (1 - (PopActual_High$time[PopActual_High$variable == 'Deaths_F1'] - 1979) ^ 2 / 1297)
PopActual_High$error[PopActual_High$variable == 'Deaths_F2'] =
	mean(PopActual_High$value[PopActual_High$variable == 'Deaths_F2']) #* 
	# (1 - (PopActual_High$time[PopActual_High$variable == 'Deaths_F2'] - 1979) ^ 2 / 1297)
PopActual_High$error[PopActual_High$variable == 'Deaths_F3'] =
	mean(PopActual_High$value[PopActual_High$variable == 'Deaths_F3']) #* 
	# (1 - (PopActual_High$time[PopActual_High$variable == 'Deaths_F3'] - 1979) ^ 2 / 1297)
PopActual_High$error[PopActual_High$variable == 'Deaths_F4'] =
	mean(PopActual_High$value[PopActual_High$variable == 'Deaths_F4']) #* 
	# (1 - (PopActual_High$time[PopActual_High$variable == 'Deaths_F4'] - 1979) ^ 2 / 1297)


################# DEFINE CALIBRATION PARAMETERS

PopCalibPars_Low =  rbind(
	MinDeath_RichM1 = as.numeric(ParameterData['MinDeath_RichM1Low',]),
	MinDeath_PoorM1 = as.numeric(ParameterData['MinDeath_PoorM1Low',]),
	MinDeath_M2 = as.numeric(ParameterData['MinDeath_M2Low',]),
	MinDeath_M3 = as.numeric(ParameterData['MinDeath_M3Low',]),
	MinDeath_M4 = as.numeric(ParameterData['MinDeath_M4Low',]),
	MinDeath_RichF1 = as.numeric(ParameterData['MinDeath_RichF1Low',]),
	MinDeath_PoorF1 = as.numeric(ParameterData['MinDeath_PoorF1Low',]),
	MinDeath_F2 = as.numeric(ParameterData['MinDeath_F2Low',]),
	MinDeath_F3 = as.numeric(ParameterData['MinDeath_F3Low',]),
	MinDeath_F4 = as.numeric(ParameterData['MinDeath_F4Low',]),
	OmegaH_RichM1 = as.numeric(ParameterData['OmegaH_RichM1Low',]),
	OmegaH_PoorM1 = as.numeric(ParameterData['OmegaH_PoorM1Low',]),
	OmegaH_M2 = as.numeric(ParameterData['OmegaH_M2Low',]),
	OmegaH_M3 = as.numeric(ParameterData['OmegaH_M3Low',]),
	OmegaH_M4 = as.numeric(ParameterData['OmegaH_M4Low',]),
	OmegaH_RichF1 = as.numeric(ParameterData['OmegaH_RichF1Low',]),
	OmegaH_PoorF1 = as.numeric(ParameterData['OmegaH_PoorF1Low',]),
	OmegaH_F2 = as.numeric(ParameterData['OmegaH_F2Low',]),
	OmegaH_F3 = as.numeric(ParameterData['OmegaH_F3Low',]),
	OmegaH_F4 = as.numeric(ParameterData['OmegaH_F4Low',]),
	OmegaF_RichM1 = as.numeric(ParameterData['OmegaF_RichM1Low',]),
	OmegaF_PoorM1 = as.numeric(ParameterData['OmegaF_PoorM1Low',]),
	OmegaF_M2 = as.numeric(ParameterData['OmegaF_M2Low',]),
	OmegaF_M3 = as.numeric(ParameterData['OmegaF_M3Low',]),
	OmegaF_M4 = as.numeric(ParameterData['OmegaF_M4Low',]),
	OmegaF_RichF1 = as.numeric(ParameterData['OmegaF_RichF1Low',]),
	OmegaF_PoorF1 = as.numeric(ParameterData['OmegaF_PoorF1Low',]),
	OmegaF_F2 = as.numeric(ParameterData['OmegaF_F2Low',]),
	OmegaF_F3 = as.numeric(ParameterData['OmegaF_F3Low',]),
	OmegaF_F4 = as.numeric(ParameterData['OmegaF_F4Low',]),
	AlphaGFR = as.numeric(ParameterData['AlphaGFR_Low',]),
	BetaE = as.numeric(ParameterData['BetaE_Low',]),
	BetaH = as.numeric(ParameterData['BetaH_Low',])
)
PopParValue_Low = PopCalibPars_Low[,1]
PopParMin_Low =  PopCalibPars_Low[,2]
PopParMax_Low = PopCalibPars_Low[,3]

PopCalibPars_Mid =  rbind(
	MinDeath_RichM1 = as.numeric(ParameterData['MinDeath_RichM1Mid',]),
	MinDeath_PoorM1 = as.numeric(ParameterData['MinDeath_PoorM1Mid',]),
	MinDeath_M2 = as.numeric(ParameterData['MinDeath_M2Mid',]),
	MinDeath_M3 = as.numeric(ParameterData['MinDeath_M3Mid',]),
	MinDeath_M4 = as.numeric(ParameterData['MinDeath_M4Mid',]),
	MinDeath_RichF1 = as.numeric(ParameterData['MinDeath_RichF1Mid',]),
	MinDeath_PoorF1 = as.numeric(ParameterData['MinDeath_PoorF1Mid',]),
	MinDeath_F2 = as.numeric(ParameterData['MinDeath_F2Mid',]),
	MinDeath_F3 = as.numeric(ParameterData['MinDeath_F3Mid',]),
	MinDeath_F4 = as.numeric(ParameterData['MinDeath_F4Mid',]),
	OmegaH_RichM1 = as.numeric(ParameterData['OmegaH_RichM1Mid',]),
	OmegaH_PoorM1 = as.numeric(ParameterData['OmegaH_PoorM1Mid',]),
	OmegaH_M2 = as.numeric(ParameterData['OmegaH_M2Mid',]),
	OmegaH_M3 = as.numeric(ParameterData['OmegaH_M3Mid',]),
	OmegaH_M4 = as.numeric(ParameterData['OmegaH_M4Mid',]),
	OmegaH_RichF1 = as.numeric(ParameterData['OmegaH_RichF1Mid',]),
	OmegaH_PoorF1 = as.numeric(ParameterData['OmegaH_PoorF1Mid',]),
	OmegaH_F2 = as.numeric(ParameterData['OmegaH_F2Mid',]),
	OmegaH_F3 = as.numeric(ParameterData['OmegaH_F3Mid',]),
	OmegaH_F4 = as.numeric(ParameterData['OmegaH_F4Mid',]),
	OmegaF_RichM1 = as.numeric(ParameterData['OmegaF_RichM1Mid',]),
	OmegaF_PoorM1 = as.numeric(ParameterData['OmegaF_PoorM1Mid',]),
	OmegaF_M2 = as.numeric(ParameterData['OmegaF_M2Mid',]),
	OmegaF_M3 = as.numeric(ParameterData['OmegaF_M3Mid',]),
	OmegaF_M4 = as.numeric(ParameterData['OmegaF_M4Mid',]),
	OmegaF_RichF1 = as.numeric(ParameterData['OmegaF_RichF1Mid',]),
	OmegaF_PoorF1 = as.numeric(ParameterData['OmegaF_PoorF1Mid',]),
	OmegaF_F2 = as.numeric(ParameterData['OmegaF_F2Mid',]),
	OmegaF_F3 = as.numeric(ParameterData['OmegaF_F3Mid',]),
	OmegaF_F4 = as.numeric(ParameterData['OmegaF_F4Mid',]),
	AlphaGFR = as.numeric(ParameterData['AlphaGFR_Mid',]),
	BetaE = as.numeric(ParameterData['BetaE_Mid',]),
	BetaH = as.numeric(ParameterData['BetaH_Mid',])
)
PopParValue_Mid = PopCalibPars_Mid[,1]
PopParMin_Mid =  PopCalibPars_Mid[,2]
PopParMax_Mid = PopCalibPars_Mid[,3]

PopCalibPars_High =  rbind(
	MinDeath_RichM1 = as.numeric(ParameterData['MinDeath_RichM1High',]),
	MinDeath_PoorM1 = as.numeric(ParameterData['MinDeath_PoorM1High',]),
	MinDeath_M2 = as.numeric(ParameterData['MinDeath_M2High',]),
	MinDeath_M3 = as.numeric(ParameterData['MinDeath_M3High',]),
	MinDeath_M4 = as.numeric(ParameterData['MinDeath_M4High',]),
	MinDeath_RichF1 = as.numeric(ParameterData['MinDeath_RichF1High',]),
	MinDeath_PoorF1 = as.numeric(ParameterData['MinDeath_PoorF1High',]),
	MinDeath_F2 = as.numeric(ParameterData['MinDeath_F2High',]),
	MinDeath_F3 = as.numeric(ParameterData['MinDeath_F3High',]),
	MinDeath_F4 = as.numeric(ParameterData['MinDeath_F4High',]),
	OmegaH_RichM1 = as.numeric(ParameterData['OmegaH_RichM1High',]),
	OmegaH_PoorM1 = as.numeric(ParameterData['OmegaH_PoorM1High',]),
	OmegaH_M2 = as.numeric(ParameterData['OmegaH_M2High',]),
	OmegaH_M3 = as.numeric(ParameterData['OmegaH_M3High',]),
	OmegaH_M4 = as.numeric(ParameterData['OmegaH_M4High',]),
	OmegaH_RichF1 = as.numeric(ParameterData['OmegaH_RichF1High',]),
	OmegaH_PoorF1 = as.numeric(ParameterData['OmegaH_PoorF1High',]),
	OmegaH_F2 = as.numeric(ParameterData['OmegaH_F2High',]),
	OmegaH_F3 = as.numeric(ParameterData['OmegaH_F3High',]),
	OmegaH_F4 = as.numeric(ParameterData['OmegaH_F4High',]),
	OmegaF_RichM1 = as.numeric(ParameterData['OmegaF_RichM1High',]),
	OmegaF_PoorM1 = as.numeric(ParameterData['OmegaF_PoorM1High',]),
	OmegaF_M2 = as.numeric(ParameterData['OmegaF_M2High',]),
	OmegaF_M3 = as.numeric(ParameterData['OmegaF_M3High',]),
	OmegaF_M4 = as.numeric(ParameterData['OmegaF_M4High',]),
	OmegaF_RichF1 = as.numeric(ParameterData['OmegaF_RichF1High',]),
	OmegaF_PoorF1 = as.numeric(ParameterData['OmegaF_PoorF1High',]),
	OmegaF_F2 = as.numeric(ParameterData['OmegaF_F2High',]),
	OmegaF_F3 = as.numeric(ParameterData['OmegaF_F3High',]),
	OmegaF_F4 = as.numeric(ParameterData['OmegaF_F4High',]),
	AlphaGFR = as.numeric(ParameterData['AlphaGFR_High',]),
	BetaE = as.numeric(ParameterData['BetaE_High',]),
	BetaH = as.numeric(ParameterData['BetaH_High',])
)
PopParValue_High = PopCalibPars_High[,1]
PopParMin_High =  PopCalibPars_High[,2]
PopParMax_High = PopCalibPars_High[,3]

PopFitness_Low = function(p) 
{
	names(p) = names(PopParValue_Low)
	PopCost = -PopulationCost(
		p=p,
		t0 = t0,
		tf = tf,
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		init = PopInit_Low,
		parms = PopParms_Low,
		exog = PopExog_Low,
		yactual = PopActual_Low
	)$model
	return(PopCost)
}
PopFitness_Mid = function(p) 
{
	names(p) = names(PopParValue_Mid)
	PopCost = -PopulationCost(
		p=p,
		t0 = t0,
		tf = tf,
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		init = PopInit_Mid,
		parms = PopParms_Mid,
		exog = PopExog_Mid,
		yactual = PopActual_Mid
	)$model
	return(PopCost)
}
PopFitness_High = function(p) 
{
	names(p) = names(PopParValue_High)
	PopCost = -PopulationCost(
		p=p,
		t0 = t0,
		tf = tf,
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		init = PopInit_High,
		parms = PopParms_High,
		exog = PopExog_High,
		yactual = PopActual_High
	)$model
	return(PopCost)
}

################# Genetic Algo
registerDoParallel(cl)
ptm = proc.time() 
PopResults_Low = gaisl( 
	type = 'real-valued',
	fitness = PopFitness_Low,
	lower = PopParMin_Low,
	upper = PopParMax_Low,
	suggestions = PopParValue_Low,
	numIslands = numcore,
    popSize = 1000,
    run = 400,
    crossover = gareal_blxCrossover, 
    maxiter = 10000)
ptm = proc.time() - ptm
print(ptm)	

ptm = proc.time() 
PopResults_Mid = gaisl( 
	type = 'real-valued',
	fitness = PopFitness_Mid,
	lower = PopParMin_Mid,
	upper = PopParMax_Mid,
	suggestions = PopParValue_Mid,
	numIslands = numcore,
    popSize = 1000,
    run = 400,
    crossover = gareal_blxCrossover, 
    maxiter = 10000)
ptm = proc.time() - ptm
print(ptm)

ptm = proc.time() 
PopResults_High = gaisl( 
	type = 'real-valued',
	fitness = PopFitness_High,
	lower = PopParMin_High,
	upper = PopParMax_High,
	suggestions = PopParValue_High,
	numIslands = numcore,
    popSize = 1000,
    run = 400,
    crossover = gareal_blxCrossover, 
    maxiter = 10000)
ptm = proc.time() - ptm
print(ptm)
stopCluster(cl)

################# PLOT FITTED VALUES

PopulationFitData_Low = CalibPlotFunc(PopResults_Low,PopActual_Low,
	PopParms_Low,PopExog_Low,PopInit_Low,PopulationMod,delta_t,delayyearlength,
	'PopulationSubmodel_LowIncome')

PopulationFitData_Mid = CalibPlotFunc(PopResults_Mid,PopActual_Mid,
	PopParms_Mid,PopExog_Mid,PopInit_Mid,PopulationMod,delta_t,delayyearlength,
	'PopulationSubmodel_MidIncome')

PopulationFitData_High = CalibPlotFunc(PopResults_High,PopActual_High,
	PopParms_High,PopExog_High,PopInit_High,PopulationMod,delta_t,delayyearlength,
	'PopulationSubmodel_HighIncome')



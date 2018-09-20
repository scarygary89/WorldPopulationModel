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
			'MortRate_M1RichLow',
			'MortRate_M2RichLow',
			'MortRate_M3RichLow',
			'MortRate_M4RichLow',
			'MortRate_F1RichLow',
			'MortRate_F2RichLow',
			'MortRate_F3RichLow',
			'MortRate_F4RichLow',
			'MortRate_M1PoorLow',
			'MortRate_M2PoorLow',
			'MortRate_M3PoorLow',
			'MortRate_M4PoorLow',
			'MortRate_F1PoorLow',
			'MortRate_F2PoorLow',
			'MortRate_F3PoorLow',
			'MortRate_F4PoorLow',
			'MortRate_M1RichMid',
			'MortRate_M2RichMid',
			'MortRate_M3RichMid',
			'MortRate_M4RichMid',
			'MortRate_F1RichMid',
			'MortRate_F2RichMid',
			'MortRate_F3RichMid',
			'MortRate_F4RichMid',
			'MortRate_M1PoorMid',
			'MortRate_M2PoorMid',
			'MortRate_M3PoorMid',
			'MortRate_M4PoorMid',
			'MortRate_F1PoorMid',
			'MortRate_F2PoorMid',
			'MortRate_F3PoorMid',
			'MortRate_F4PoorMid',
			'MortRate_M1RichHigh',
			'MortRate_M2RichHigh',
			'MortRate_M3RichHigh',
			'MortRate_M4RichHigh',
			'MortRate_F1RichHigh',
			'MortRate_F2RichHigh',
			'MortRate_F3RichHigh',
			'MortRate_F4RichHigh',
			'MortRate_M1PoorHigh',
			'MortRate_M2PoorHigh',
			'MortRate_M3PoorHigh',
			'MortRate_M4PoorHigh',
			'MortRate_F1PoorHigh',
			'MortRate_F2PoorHigh',
			'MortRate_F3PoorHigh',
			'MortRate_F4PoorHigh',
			'GFR_Low',
			'GFR_Mid',
			'GFR_High'
		)
		AuxData = matrix(NA,nrow = (length(tspan)),ncol = length(aux_names))		
		colnames(AuxData) = aux_names
		StockData = matrix(NA,nrow = (length(tspan) + 1),ncol = length(init))
		colnames(StockData) = names(init)	
		stocks = init
		StockData[1,] = stocks
		for(i in 1:length(tspan)) {
			RegPop_ijr = list( 
				Low = c(
					M1 = as.numeric(stocks['Low_M1']),
					M2 = as.numeric(stocks['Low_M2']),
					M3 = as.numeric(stocks['Low_M3']),
					M4 = as.numeric(stocks['Low_M4']),
					F1 = as.numeric(stocks['Low_F1']),
					F2 = as.numeric(stocks['Low_F2']),
					F3 = as.numeric(stocks['Low_F3']),
					F4 = as.numeric(stocks['Low_F4'])),
				Mid = c(
					M1 = as.numeric(stocks['Mid_M1']),
					M2 = as.numeric(stocks['Mid_M2']),
					M3 = as.numeric(stocks['Mid_M3']),
					M4 = as.numeric(stocks['Mid_M4']),
					F1 = as.numeric(stocks['Mid_F1']),
					F2 = as.numeric(stocks['Mid_F2']),
					F3 = as.numeric(stocks['Mid_F3']),
					F4 = as.numeric(stocks['Mid_F4'])),
				High = c(
					M1 = as.numeric(stocks['High_M1']),
					M2 = as.numeric(stocks['High_M2']),
					M3 = as.numeric(stocks['High_M3']),
					M4 = as.numeric(stocks['High_M4']),
					F1 = as.numeric(stocks['High_F1']),
					F2 = as.numeric(stocks['High_F2']),
					F3 = as.numeric(stocks['High_F3']),
					F4 = as.numeric(stocks['High_F4']))
			)

			# Regional Population System 
			GeneralHealthAccess_Low = c(
				Rich = exog[i,'GeneralHealthAccess_RichLow'],
				Poor = exog[i,'GeneralHealthAccess_PoorLow'])

			GeneralHealthAccess_Mid = c(
				Rich = exog[i,'GeneralHealthAccess_RichMid'],
				Poor = exog[i,'GeneralHealthAccess_PoorMid'])
			
			GeneralHealthAccess_High = c(
				Rich = exog[i,'GeneralHealthAccess_RichHigh'],
				Poor = exog[i,'GeneralHealthAccess_PoorHigh'])

			NutritionConsumptionPC_Low = c(
				Rich = exog[i,'NutritionConsumptionPC_Low'],
				Poor = exog[i,'NutritionConsumptionPC_Low'])

			NutritionConsumptionPC_Mid = c(
				Rich = exog[i,'NutritionConsumptionPC_Mid'],
				Poor = exog[i,'NutritionConsumptionPC_Mid'])

			NutritionConsumptionPC_High = c(
				Rich = exog[i,'NutritionConsumptionPC_High'],
				Poor = exog[i,'NutritionConsumptionPC_High'])

			PopOut_Low = Population(
				RegPop_ijr[['Low']],
				exog[i,'FemaleEduAttain_Low'],
				exog[i,'FemaleHealthAccess_Low'],
				GeneralHealthAccess_Low,
				NutritionConsumptionPC_Low,
				parms)

			PopOut_Mid = Population(
				RegPop_ijr[['Mid']],
				exog[i,'FemaleEduAttain_Mid'],
				exog[i,'FemaleHealthAccess_Mid'],
				GeneralHealthAccess_Mid,
				NutritionConsumptionPC_Mid,
				parms)

			PopOut_High = Population(
				RegPop_ijr[['High']],
				exog[i,'FemaleEduAttain_High'],
				exog[i,'FemaleHealthAccess_High'],
				GeneralHealthAccess_High,
				NutritionConsumptionPC_High,
				parms)

			################ STORE OUTPUT

			# AUXILIARY VARIABLES
			aux = c(
				PopOut_Low[['MortRate_ijk']],
				PopOut_Low[['GFR']]
			)

			AuxData[i,] = aux

			# STOCK VARIABLES
			dstocks = c(
				PopOut_Low[["dPop_ij"]]
			) 
			stocks = stocks + dstocks * delta_t
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

PopParms = c( 	
	MinDeath_RichM1 = as.numeric(ParameterValue['MinDeath_RichM1']),
	MinDeath_PoorM1 = as.numeric(ParameterValue['MinDeath_PoorM1']),
	MinDeath_M2 = as.numeric(ParameterValue['MinDeath_M2']),
	MinDeath_M3 = as.numeric(ParameterValue['MinDeath_M3']),
	MinDeath_M4 = as.numeric(ParameterValue['MinDeath_M4']),
	MinDeath_RichF1 = as.numeric(ParameterValue['MinDeath_RichF1']),
	MinDeath_PoorF1 = as.numeric(ParameterValue['MinDeath_PoorF1']),
	MinDeath_F2 = as.numeric(ParameterValue['MinDeath_F2']),
	MinDeath_F3 = as.numeric(ParameterValue['MinDeath_F3']),
	MinDeath_F4 = as.numeric(ParameterValue['MinDeath_F4']),
	OmegaH_RichM1 = as.numeric(ParameterValue['OmegaH_RichM1']),
	OmegaH_PoorM1 = as.numeric(ParameterValue['OmegaH_PoorM1']),
	OmegaH_M2 = as.numeric(ParameterValue['OmegaH_M2']),
	OmegaH_M3 = as.numeric(ParameterValue['OmegaH_M3']),
	OmegaH_M4 = as.numeric(ParameterValue['OmegaH_M4']),
	OmegaH_RichF1 = as.numeric(ParameterValue['OmegaH_RichF1']),
	OmegaH_PoorF1 = as.numeric(ParameterValue['OmegaH_PoorF1']),
	OmegaH_F2 = as.numeric(ParameterValue['OmegaH_F2']),
	OmegaH_F3 = as.numeric(ParameterValue['OmegaH_F3']),
	OmegaH_F4 = as.numeric(ParameterValue['OmegaH_F4']),
	OmegaF_RichM1 = as.numeric(ParameterValue['OmegaF_RichM1']),
	OmegaF_PoorM1 = as.numeric(ParameterValue['OmegaF_PoorM1']),
	OmegaF_M2 = as.numeric(ParameterValue['OmegaF_M2']),
	OmegaF_M3 = as.numeric(ParameterValue['OmegaF_M3']),
	OmegaF_M4 = as.numeric(ParameterValue['OmegaF_M4']),
	OmegaF_RichF1 = as.numeric(ParameterValue['OmegaF_RichF1']),
	OmegaF_PoorF1 = as.numeric(ParameterValue['OmegaF_PoorF1']),
	OmegaF_F2 = as.numeric(ParameterValue['OmegaF_F2']),
	OmegaF_F3 = as.numeric(ParameterValue['OmegaF_F3']),
	OmegaF_F4 = as.numeric(ParameterValue['OmegaF_F4']),
	AlphaGFR = as.numeric(ParameterValue['AlphaGFR']),
	BetaE = as.numeric(ParameterValue['BetaE']),
	BetaH = as.numeric(ParameterValue['BetaH']),
	FemaleBirthRatio = as.numeric(ParameterValue['FemaleBirthRatio']),
	NutritionReq = as.numeric(ParameterValue['NutritionReq']),
	PoorFrac = as.numeric(ParameterValue['PoorFrac'])
)

################# DEFINE INITIAL CONDITIONS

PopInit = c( 	
	Low_M1 = as.numeric(InitValue['Low_M1']),
	Low_M2 = as.numeric(InitValue['Low_M2']),
	Low_M3 = as.numeric(InitValue['Low_M3']),
	Low_M4 = as.numeric(InitValue['Low_M4']),
	Low_F1 = as.numeric(InitValue['Low_F1']),
	Low_F2 = as.numeric(InitValue['Low_F2']),
	Low_F3 = as.numeric(InitValue['Low_F3']),
	Low_F4 = as.numeric(InitValue['Low_F4']),	
	Mid_M1 = as.numeric(InitValue['Mid_M1']),
	Mid_M2 = as.numeric(InitValue['Mid_M2']),
	Mid_M3 = as.numeric(InitValue['Mid_M3']),
	Mid_M4 = as.numeric(InitValue['Mid_M4']),
	Mid_F1 = as.numeric(InitValue['Mid_F1']),
	Mid_F2 = as.numeric(InitValue['Mid_F2']),
	Mid_F3 = as.numeric(InitValue['Mid_F3']),
	Mid_F4 = as.numeric(InitValue['Mid_F4']),	
	High_M1 = as.numeric(InitValue['High_M1']),
	High_M2 = as.numeric(InitValue['High_M2']),
	High_M3 = as.numeric(InitValue['High_M3']),
	High_M4 = as.numeric(InitValue['High_M4']),
	High_F1 = as.numeric(InitValue['High_F1']),
	High_F2 = as.numeric(InitValue['High_F2']),
	High_F3 = as.numeric(InitValue['High_F3']),
	High_F4 = as.numeric(InitValue['High_F4'])
)

################# LOAD EXOGENOUS DATA

PopExog = na.omit(cbind(
	time = CalibData['time'],
	GeneralHealthAccess_RichLow = CalibData[,'GeneralHealthAccess_RichLow'],
	GeneralHealthAccess_PoorLow = CalibData[,'GeneralHealthAccess_PoorLow'],
	FemaleHealthAccess_Low = CalibData[,'FemaleHealthAccess_Low'],
	FemaleEduAttain_Low = CalibData[,'FemaleEduAttain_Low'],
	NutritionConsumptionPC_Low = CalibData[,'NutritionConsumptionPC_Low'],
	GeneralHealthAccess_RichMid = CalibData[,'GeneralHealthAccess_RichMid'],
	GeneralHealthAccess_PoorMid = CalibData[,'GeneralHealthAccess_PoorMid'],
	FemaleHealthAccess_Mid = CalibData[,'FemaleHealthAccess_Mid'],
	FemaleEduAttain_Mid = CalibData[,'FemaleEduAttain_Mid'],
	NutritionConsumptionPC_Mid = CalibData[,'NutritionConsumptionPC_Mid'],
	GeneralHealthAccess_RichHigh = CalibData[,'GeneralHealthAccess_RichHigh'],
	GeneralHealthAccess_PoorHigh = CalibData[,'GeneralHealthAccess_PoorHigh'],
	FemaleHealthAccess_High = CalibData[,'FemaleHealthAccess_High'],
	FemaleEduAttain_High = CalibData[,'FemaleEduAttain_High'],
	NutritionConsumptionPC_High = CalibData[,'NutritionConsumptionPC_High']
))

################# LOAD ACTUAL DATA

PopActual = na.omit(melt(
	data.frame(cbind(
		time = CalibData$time,
		Low_M1 = CalibData$Low_M1,
		Low_M3 = CalibData$Low_M3,
		Low_M2 = CalibData$Low_M2,
		Low_M4 = CalibData$Low_M4,
		Low_F1 = CalibData$Low_F1,
		Low_F2 = CalibData$Low_F2,
		Low_F3 = CalibData$Low_F3,
		Low_F4 = CalibData$Low_F4,
		Mid_M1 = CalibData$Mid_M1,
		Mid_M3 = CalibData$Mid_M3,
		Mid_M2 = CalibData$Mid_M2,
		Mid_M4 = CalibData$Mid_M4,
		Mid_F1 = CalibData$Mid_F1,
		Mid_F2 = CalibData$Mid_F2,
		Mid_F3 = CalibData$Mid_F3,
		Mid_F4 = CalibData$Mid_F4,
		High_M1 = CalibData$High_M1,
		High_M3 = CalibData$High_M3,
		High_M2 = CalibData$High_M2,
		High_M4 = CalibData$High_M4,
		High_F1 = CalibData$High_F1,
		High_F2 = CalibData$High_F2,
		High_F3 = CalibData$High_F3,
		High_F4 = CalibData$High_F4,					
		GFR_Low = CalibData$GFR_Low,
		GFR_Mid = CalibData$GFR_Mid,
		GFR_High = CalibData$GFR_High
	)),
	id ='time'))
PopActual$variable = as.character(PopActual$variable)
PopActual = PopActual[,c('variable','time','value')]

PopActual$error[PopActual$variable == 'Low_M1'] =
	mean(PopActual$value[PopActual$variable == 'Low_M1']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Low_M1'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'Low_M2'] =
	mean(PopActual$value[PopActual$variable == 'Low_M2']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Low_M2'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'Low_M3'] =
	mean(PopActual$value[PopActual$variable == 'Low_M3']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Low_M3'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'Low_M4'] =
	mean(PopActual$value[PopActual$variable == 'Low_M4']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Low_M4'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'Low_F1'] =
	mean(PopActual$value[PopActual$variable == 'Low_F1']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Low_F1'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'Low_F2'] =
	mean(PopActual$value[PopActual$variable == 'Low_F2']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Low_F2'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'Low_F3'] =
	mean(PopActual$value[PopActual$variable == 'Low_F3']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Low_F3'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'Low_F4'] =
	mean(PopActual$value[PopActual$variable == 'Low_F4']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Low_F4'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'GFR_Low'] =
	mean(PopActual$value[PopActual$variable == 'GFR_Low']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'GFR_Low'] - 1979) ^ 2 / 1297)

PopActual$error[PopActual$variable == 'Mid_M1'] =
	mean(PopActual$value[PopActual$variable == 'Mid_M1']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Mid_M1'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'Mid_M2'] =
	mean(PopActual$value[PopActual$variable == 'Mid_M2']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Mid_M2'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'Mid_M3'] =
	mean(PopActual$value[PopActual$variable == 'Mid_M3']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Mid_M3'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'Mid_M4'] =
	mean(PopActual$value[PopActual$variable == 'Mid_M4']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Mid_M4'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'Mid_F1'] =
	mean(PopActual$value[PopActual$variable == 'Mid_F1']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Mid_F1'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'Mid_F2'] =
	mean(PopActual$value[PopActual$variable == 'Mid_F2']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Mid_F2'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'Mid_F3'] =
	mean(PopActual$value[PopActual$variable == 'Mid_F3']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Mid_F3'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'Mid_F4'] =
	mean(PopActual$value[PopActual$variable == 'Mid_F4']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'Mid_F4'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'GFR_Mid'] =
	mean(PopActual$value[PopActual$variable == 'GFR_Mid']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'GFR_Mid'] - 1979) ^ 2 / 1297)

PopActual$error[PopActual$variable == 'High_M1'] =
	mean(PopActual$value[PopActual$variable == 'High_M1']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'High_M1'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'High_M2'] =
	mean(PopActual$value[PopActual$variable == 'High_M2']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'High_M2'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'High_M3'] =
	mean(PopActual$value[PopActual$variable == 'High_M3']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'High_M3'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'High_M4'] =
	mean(PopActual$value[PopActual$variable == 'High_M4']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'High_M4'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'High_F1'] =
	mean(PopActual$value[PopActual$variable == 'High_F1']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'High_F1'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'High_F2'] =
	mean(PopActual$value[PopActual$variable == 'High_F2']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'High_F2'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'High_F3'] =
	mean(PopActual$value[PopActual$variable == 'High_F3']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'High_F3'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'High_F4'] =
	mean(PopActual$value[PopActual$variable == 'High_F4']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'High_F4'] - 1979) ^ 2 / 1297)
PopActual$error[PopActual$variable == 'GFR_High'] =
	mean(PopActual$value[PopActual$variable == 'GFR_High']) #* 
	# (1 - (PopActual$time[PopActual$variable == 'GFR_High'] - 1979) ^ 2 / 1297)
	
################# DEFINE CALIBRATION PARAMETERS

PopCalibPars =  rbind(
	MinDeath_RichM1 = as.numeric(ParameterData['MinDeath_RichM1',]),
	MinDeath_PoorM1 = as.numeric(ParameterData['MinDeath_PoorM1',]),
	MinDeath_M2 = as.numeric(ParameterData['MinDeath_M2',]),
	MinDeath_M3 = as.numeric(ParameterData['MinDeath_M3',]),
	MinDeath_M4 = as.numeric(ParameterData['MinDeath_M4',]),
	MinDeath_RichF1 = as.numeric(ParameterData['MinDeath_RichF1',]),
	MinDeath_PoorF1 = as.numeric(ParameterData['MinDeath_PoorF1',]),
	MinDeath_F2 = as.numeric(ParameterData['MinDeath_F2',]),
	MinDeath_F3 = as.numeric(ParameterData['MinDeath_F3',]),
	MinDeath_F4 = as.numeric(ParameterData['MinDeath_F4',]),
	OmegaH_RichM1 = as.numeric(ParameterData['OmegaH_RichM1',]),
	OmegaH_PoorM1 = as.numeric(ParameterData['OmegaH_PoorM1',]),
	OmegaH_M2 = as.numeric(ParameterData['OmegaH_M2',]),
	OmegaH_M3 = as.numeric(ParameterData['OmegaH_M3',]),
	OmegaH_M4 = as.numeric(ParameterData['OmegaH_M4',]),
	OmegaH_RichF1 = as.numeric(ParameterData['OmegaH_RichF1',]),
	OmegaH_PoorF1 = as.numeric(ParameterData['OmegaH_PoorF1',]),
	OmegaH_F2 = as.numeric(ParameterData['OmegaH_F2',]),
	OmegaH_F3 = as.numeric(ParameterData['OmegaH_F3',]),
	OmegaH_F4 = as.numeric(ParameterData['OmegaH_F4',]),
	OmegaF_RichM1 = as.numeric(ParameterData['OmegaF_RichM1',]),
	OmegaF_PoorM1 = as.numeric(ParameterData['OmegaF_PoorM1',]),
	OmegaF_M2 = as.numeric(ParameterData['OmegaF_M2',]),
	OmegaF_M3 = as.numeric(ParameterData['OmegaF_M3',]),
	OmegaF_M4 = as.numeric(ParameterData['OmegaF_M4',]),
	OmegaF_RichF1 = as.numeric(ParameterData['OmegaF_RichF1',]),
	OmegaF_PoorF1 = as.numeric(ParameterData['OmegaF_PoorF1',]),
	OmegaF_F2 = as.numeric(ParameterData['OmegaF_F2',]),
	OmegaF_F3 = as.numeric(ParameterData['OmegaF_F3',]),
	OmegaF_F4 = as.numeric(ParameterData['OmegaF_F4',]),
	AlphaGFR = as.numeric(ParameterData['AlphaGFR',]),
	BetaE = as.numeric(ParameterData['BetaE',]),
	BetaH = as.numeric(ParameterData['BetaH',])
)

PopParValue = PopCalibPars[,1]
PopParMin =  PopCalibPars[,2]
PopParMax = PopCalibPars[,3]
PopFitness = function(p) 
{
	names(p) = names(PopParValue)
	PopCost = -PopulationCost(
		p=p,
		t0 = t0,
		tf = tf,
		delta_t = delta_t,
		delayyearlength = delayyearlength,
		init = PopInit,
		parms = PopParms,
		exog = PopExog,
		yactual = PopActual
	)$model
	return(PopCost)
}
################# Genetic Algo
registerDoParallel(cl)
ptm = proc.time() 
PopResults = gaisl( 
	type = 'real-valued',
	fitness = PopFitness,
	lower = PopParMin,
	upper = PopParMax,
	suggestions = PopParValue,
	numIslands = numcore,
    popSize = 1000,
    run = 400,
    crossover = gareal_blxCrossover, 
    maxiter = 10000)
ptm = proc.time() - ptm
print(ptm)	
stopCluster(cl)

################# PLOT FITTED VALUES

PopulationFitData = CalibPlotFunc(PopResults,PopActual,
	PopParms,PopExog,PopInit,PopulationMod,delta_t,delayyearlength,
	'PopulationSubmodel2')



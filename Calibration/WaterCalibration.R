########################                       ########################
########################    WATER SUBMODEL     ########################
########################                       ########################

library(foreach)
library(doParallel)
numcore = detectCores()
cl<-makeCluster(numcore) 
print(cl)

################# SUBMODEL WITH EXOGENOUS INPUTS

WaterMod = function(t0,tf,delta_t,delayyearlength,exog,init,parms) {
	# write.csv(unlist(parms),file='CurrentParms.csv')	
	with(as.list(c(parms)), {	
		tspan = seq(from=t0, to=tf, by=delta_t)
		aux_names = c(
			'MunWaterDemand',
			'IndWaterDemand'
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
  		# Assemble Lists and Vectors
			RegPop_r = c(
				Low = exog[i,'LowPop'],
				Mid = exog[i,'MidPop'],
				High = exog[i,'HighPop'])

			EconOutput_r = 	 c( Low = exog[i,'EconOutput_Low'],
								Mid = exog[i,'EconOutput_Mid'],
								High = exog[i,'EconOutput_High'])

			# Global Water Supply

			WaterOut      	= Water(
								stocks,
								exog[i,'TempAnamoly'],
								EconOutput_r,
								exog[i,'AgriWaterDemand'],
								RegPop_r,
								parms)

			################ STORE OUTPUT

			# AUXILIARY VARIABLES
			aux = c(
				WaterOut[['MunWaterDemand']],
				WaterOut[['IndWaterDemand']]
			)
			AuxData[i,] = aux

			# STOCK VARIABLES
			dstocks = WaterOut[["dFreshwater"]]
			stocks = stocks + dstocks * delta_t
			stocks = max(stocks,1e-16)
			StockData[i+1,1] = stocks
		}
	Output = cbind(tspan, StockData[-(length(tspan)+1),],AuxData)
	colnames(Output)[1] = 'time'
	colnames(Output)[2] = 'Freshwater'
	return(Output)
	})
}

################# COST FUNCTION

WaterCost = function(p,t0,tf,delta_t,delayyearlength,exog,init,parms,yactual)
{
    whichpar = names(parms)[names(parms) %in% names(p)]
    whichinit = names(init)[names(init) %in% names(p)]
    parms[whichpar] = p[whichpar]
    init[whichinit] = p[whichinit]
    ysim = WaterMod(t0,tf,delta_t,delayyearlength,exog,init,parms)
    RunCost = modCost(ysim,yactual,x = 'time',y = 'value',err = 'error',
        scaleVar=T)
    # print(RunCost)
    return(RunCost)
}

################# PARAMETER ESTIMATION FUNCTION

WaterFit = function(parvalue,parmin,parmax,yactual,optmethod,control=list(),
    delta_t,delayyearlength,exog,init,parms)
{
    t0 = min(exog$time)
    tf = max(exog$time)
    ptm = proc.time() 
    WatFit = modFit(
        f = WaterCost,
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
    return(WatFit)
}

################# DEFINE INITIAL PARAMETER VALUES

WaterParms = c(  
    ZetaI1_Low = as.numeric(ParameterValue['ZetaI1_Low']),
    ZetaI1_Mid = as.numeric(ParameterValue['ZetaI1_Mid']),
    ZetaI1_High = as.numeric(ParameterValue['ZetaI1_High']),
    ZetaI2_Low = as.numeric(ParameterValue['ZetaI2_Low']),
    ZetaI2_Mid = as.numeric(ParameterValue['ZetaI2_Mid']),
    ZetaI2_High = as.numeric(ParameterValue['ZetaI2_High']),
    WaterDemandPC_Low = as.numeric(ParameterValue['WaterDemandPC_Low']),
    WaterDemandPC_Mid = as.numeric(ParameterValue['WaterDemandPC_Mid']),
    WaterDemandPC_High = as.numeric(ParameterValue['WaterDemandPC_High']),
    WaterReplDelay = as.numeric(ParameterValue['WaterReplDelay']),
    WaterReplMax = as.numeric(ParameterValue['WaterReplMax']),
    DeltaW = as.numeric(ParameterValue['DeltaW'])
)

################# DEFINE INITIAL CONDITIONS

WaterInit = c(   
    Freshwater = as.numeric(InitValue['Freshwater'])
)

################# LOAD EXOGENOUS DATA

WaterExog = na.omit(cbind(
    time = CalibData['time'],
    LowPop = CalibData[,'LowPop'],
    MidPop = CalibData[,'MidPop'],
    HighPop = CalibData[,'HighPop'],
    EconOutput_Low = CalibData[,'EconOutput_Low'],
    EconOutput_Mid = CalibData[,'EconOutput_Mid'],
    EconOutput_High = CalibData[,'EconOutput_High'],
    AgriWaterDemand = CalibData[,'SmoothAgriculturalWaterConsumption'],
    TempAnamoly = CalibData[,'TempAnamoly']
))

################# LOAD ACTUAL DATA

WaterActual = na.omit(melt(
    data.frame(cbind(
        time = CalibData$time,
        MunWaterDemand = CalibData$SmoothMunicipalWaterConsumption,
        IndWaterDemand = CalibData$SmoothIndustrialWaterConsumption,
        Freshwater = CalibData$Freshwater)),
    id ='time'))
WaterActual$variable = as.character(WaterActual$variable)
WaterActual = WaterActual[,c('variable','time','value')]
WaterActual$error[WaterActual$variable == 'MunWaterDemand'] = 
    mean(WaterActual$value[WaterActual$variable == 'MunWaterDemand']) #/ 
    # (WaterActual$time[WaterActual$variable == 'MunWaterDemand'] - 1979) ^ 2
WaterActual$error[WaterActual$variable == 'IndWaterDemand'] = 
    mean(WaterActual$value[WaterActual$variable == 'IndWaterDemand']) #/ 
    # (WaterActual$time[WaterActual$variable == 'IndWaterDemand'] - 1979) ^ 2
WaterActual$error[WaterActual$variable == 'Freshwater'] = 
    mean(WaterActual$value[WaterActual$variable == 'Freshwater']) #/ 
    # (WaterActual$time[WaterActual$variable == 'Freshwater'] - 1979) ^ 2

################# DEFINE CALIBRATION PARAMETERS

WaterCalibPars =  rbind(
	ZetaI1_Low = as.numeric(ParameterData['ZetaI1_Low',]),
	ZetaI1_Mid = as.numeric(ParameterData['ZetaI1_Mid',]),
	ZetaI1_High = as.numeric(ParameterData['ZetaI1_High',]),
	ZetaI2_Low = as.numeric(ParameterData['ZetaI2_Low',]),
	ZetaI2_Mid = as.numeric(ParameterData['ZetaI2_Mid',]),
	ZetaI2_High = as.numeric(ParameterData['ZetaI2_High',]),
	WaterDemandPC_Low = as.numeric(ParameterData['WaterDemandPC_Low',]),
	WaterDemandPC_Mid = as.numeric(ParameterData['WaterDemandPC_Mid',]),
	WaterDemandPC_High = as.numeric(ParameterData['WaterDemandPC_High',]),
	WaterReplDelay = as.numeric(ParameterData['WaterReplDelay',])
)

WaterParValue = WaterCalibPars[,1]
WaterParMin =  WaterCalibPars[,2]
WaterParMax = WaterCalibPars[,3]
WaterParRange = data.frame(min = WaterParMin,max = WaterParMax)

################# 2-STAGE FIT PARAMETERS

N = 1000
WaterParStart = Latinhyper(WaterParRange,N)
registerDoParallel(cl)
ptm = proc.time() 
WaterResults = foreach(i=1:N,.packages='FME') %dopar%
{
    WaterParValue = WaterParStart[i,]
    
    WaterFitPseudo = WaterFit(
        parvalue = WaterParValue,
        parmin = WaterParMin,
        parmax = WaterParMax,
        yactual = WaterActual,
        optmethod = 'Pseudo',
        # control = list(numiter = 100000),
        delta_t = delta_t,
        delayyearlength = delayyearlength,
        exog = WaterExog,
        init = WaterInit,
        parms = WaterParms)

    WaterFitMod = WaterFit(
        parvalue = coef(WaterFitPseudo),
        parmin = WaterParMin,
        parmax = WaterParMax,
        yactual = WaterActual,
        optmethod = 'Marq',
        delta_t = delta_t,
        delayyearlength = delayyearlength,
        exog = WaterExog,
        init = WaterInit,
        parms = WaterParms)
    
    return(WaterFitMod)
}
ptm = proc.time() - ptm
print(ptm)
stopCluster(cl)

################# PLOT FITTED VALUES

WaterFitData = CalibPlotFunc(WaterResults,WaterActual,WaterParms,
    WaterExog,WaterInit,WaterMod,delta_t,delayyearlength,
    'WaterSubmodel')

SSRCoefPlot(WaterResults,WaterParStart,'WaterSubmodel')

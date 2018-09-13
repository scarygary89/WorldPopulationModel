########################                       ########################
########################   CLIMATE SUBMODEL    ########################
########################                       ########################

library(foreach)
library(doParallel)
numcore = detectCores() 
cl<-makeCluster(numcore) 
print(cl)

################# SUBMODEL WITH EXOGENOUS INPUTS

ClimateMod = function(t0,tf,delta_t,delayyearlength,exog,init,parms) {
	# write.csv(unlist(parms),file='CurrentParms.csv')	
	with(as.list(c(parms)), {	
		tspan = seq(from=t0, to=tf, by=delta_t)
		aux_names = c(
			'TempAnamoly',
			'CO2EmissionPC_Low',
			'CO2EmissionPC_Mid',
			'CO2EmissionPC_High'
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

			RegPop_r = c(
				Low = exog[i,'LowPop'],
				Mid = exog[i,'MidPop'],
				High = exog[i,'HighPop'])

			EconOutputPC_r = c(
				Low = 	exog[i,'EconOutput_Low'] / (RegPop_r['Low']*1000),
				Mid = 	exog[i,'EconOutput_Mid'] / (RegPop_r['Mid']*1000),
				High = 	exog[i,'EconOutput_High'] / (RegPop_r['High']*1000)) 

			# Global Climate
			ClimateOut    	= Climate(
								stocks,
								EconOutputPC_r,
								RegPop_r,
								parms)

			################ STORE OUTPUT

			# AUXILIARY VARIABLES
			aux = c(
				ClimateOut[['TempAnamoly']],
				ClimateOut[['CO2EmissionPC']]
			)
			AuxData[i,] = aux

			# STOCK VARIABLES
			dstocks = ClimateOut[["dCO2Conc"]]
			stocks = stocks + dstocks * delta_t
			stocks = pmax(stocks,1e-16)
			StockData[i+1,] = stocks
		}
	Output = cbind(tspan, StockData[-(length(tspan)+1),],AuxData)
	colnames(Output)[1] = 'time'
	colnames(Output)[2] = 'CO2Conc'
	return(Output)
	})
}

################# COST FUNCTION

ClimateCost = function(p,t0,tf,delta_t,delayyearlength,exog,init,parms,yactual)
{
    whichpar = names(parms)[names(parms) %in% names(p)]
    whichinit = names(init)[names(init) %in% names(p)]
    parms[whichpar] = p[whichpar]
    init[whichinit] = p[whichinit]
    ysim = ClimateMod(t0,tf,delta_t,delayyearlength,exog,init,parms)
    RunCost = modCost(ysim,yactual,x = 'time',y = 'value',err = 'error',
        scaleVar=T)
    # print(RunCost)
    return(RunCost)
}

################# PARAMETER ESTIMATION FUNCTION

ClimateFit = function(parvalue,parmin,parmax,yactual,optmethod,control=list(),
    delta_t,delayyearlength,exog,init,parms)
{
    t0 = min(exog$time)
    tf = max(exog$time)
    ptm = proc.time() 
    WatFit = modFit(
        f = ClimateCost,
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

ClimateParms = c(  
	Lambda = as.numeric(ParameterValue['Lambda']),
	RefTemp = as.numeric(ParameterValue['RefTemp']),
	PsiE1_Low = as.numeric(ParameterValue['PsiE1_Low']),
	PsiE1_Mid = as.numeric(ParameterValue['PsiE1_Mid']),
	PsiE1_High = as.numeric(ParameterValue['PsiE1_High']),
	PsiE2_Low = as.numeric(ParameterValue['PsiE2_Low']),
	PsiE2_Mid = as.numeric(ParameterValue['PsiE2_Mid']),
	PsiE2_High = as.numeric(ParameterValue['PsiE2_High']),
	Gamma = as.numeric(ParameterValue['Gamma']),
	RefCO2Conc = as.numeric(ParameterValue['RefCO2Conc']),
	OtherRadForce = as.numeric(ParameterValue['OtherRadForce'])
)

################# DEFINE INITIAL CONDITIONS

ClimateInit = c(   
    CO2Conc = as.numeric(InitValue['CO2Conc'])
)

################# LOAD EXOGENOUS DATA

ClimateExog = na.omit(cbind(
    time = CalibData['time'],
    LowPop = CalibData[,'LowPop'],
    MidPop = CalibData[,'MidPop'],
    HighPop = CalibData[,'HighPop'],
    EconOutput_Low = CalibData[,'EconOutput_Low'],
    EconOutput_Mid = CalibData[,'EconOutput_Mid'],
    EconOutput_High = CalibData[,'EconOutput_High']
))

################# LOAD ACTUAL DATA

ClimateActual = na.omit(melt(
    data.frame(cbind(
        time = CalibData$time,
        CO2Conc = CalibData$CO2Conc,
        TempAnamoly = CalibData$TempAnamoly,
        CO2EmissionPC_Low = CalibData$CO2EmissionPC_Low,
        CO2EmissionPC_Mid = CalibData$CO2EmissionPC_Mid,
        CO2EmissionPC_High = CalibData$CO2EmissionPC_High)),
    id ='time'))
ClimateActual$variable = as.character(ClimateActual$variable)
ClimateActual = ClimateActual[,c('variable','time','value')]
ClimateActual$error[ClimateActual$variable == 'CO2Conc'] = 
    mean(ClimateActual$value[ClimateActual$variable == 'CO2Conc']) #/ 
    # (ClimateActual$time[ClimateActual$variable == 'CO2Conc'] - 1979) ^ 2
ClimateActual$error[ClimateActual$variable == 'TempAnamoly'] = 
    mean(ClimateActual$value[ClimateActual$variable == 'TempAnamoly']) #/ 
    # (ClimateActual$time[ClimateActual$variable == 'TempAnamoly'] - 1979) ^ 2
ClimateActual$error[ClimateActual$variable == 'CO2EmissionPC_Low'] = 
    mean(ClimateActual$value[ClimateActual$variable == 'CO2EmissionPC_Low']) #/ 
    # (ClimateActual$time[ClimateActual$variable == 'CO2EmissionPC_Low'] - 1979) ^ 2
ClimateActual$error[ClimateActual$variable == 'CO2EmissionPC_Mid'] = 
    mean(ClimateActual$value[ClimateActual$variable == 'CO2EmissionPC_Mid']) #/ 
    # (ClimateActual$time[ClimateActual$variable == 'CO2EmissionPC_Mid'] - 1979) ^ 2
    ClimateActual$error[ClimateActual$variable == 'CO2EmissionPC_High'] = 
    mean(ClimateActual$value[ClimateActual$variable == 'CO2EmissionPC_High']) #/ 
    # (ClimateActual$time[ClimateActual$variable == 'CO2EmissionPC_High'] - 1979) ^ 2

################# DEFINE CALIBRATION PARAMETERS

ClimateCalibPars =  rbind(
	Lambda = as.numeric(ParameterData['Lambda',]),
	# RefTemp = as.numeric(ParameterData['RefTemp',]),
	PsiE1_Low = as.numeric(ParameterData['PsiE1_Low',]),
	PsiE1_Mid = as.numeric(ParameterData['PsiE1_Mid',]),
	PsiE1_High = as.numeric(ParameterData['PsiE1_High',]),
	PsiE2_Low = as.numeric(ParameterData['PsiE2_Low',]),
	PsiE2_Mid = as.numeric(ParameterData['PsiE2_Mid',]),
	PsiE2_High = as.numeric(ParameterData['PsiE2_High',]),
	Gamma = as.numeric(ParameterData['Gamma',]),
	# RefCO2Conc = as.numeric(ParameterData['RefCO2Conc',]),
	OtherRadForce = as.numeric(ParameterData['OtherRadForce',])
)

ClimateParValue = ClimateCalibPars[,1]
ClimateParMin =  ClimateCalibPars[,2]
ClimateParMax = ClimateCalibPars[,3]
ClimateParRange = data.frame(min = ClimateParMin,max = ClimateParMax)

################# 2-STAGE FIT PARAMETERS

N = 1000
ClimateParStart = Latinhyper(ClimateParRange,N)
registerDoParallel(cl)
ptm = proc.time() 
ClimateResults = foreach(i=1:N,.packages='FME') %dopar%
{
    ClimateParValue = ClimateParStart[i,]
    
    ClimateFitPseudo = ClimateFit(
        parvalue = ClimateParValue,
        parmin = ClimateParMin,
        parmax = ClimateParMax,
        yactual = ClimateActual,
        optmethod = 'Pseudo',
        # control = list(numiter = 100000),
        delta_t = delta_t,
        delayyearlength = delayyearlength,
        exog = ClimateExog,
        init = ClimateInit,
        parms = ClimateParms)

    ClimateFitMod = ClimateFit(
        parvalue = coef(ClimateFitPseudo),
        parmin = ClimateParMin,
        parmax = ClimateParMax,
        yactual = ClimateActual,
        optmethod = 'Marq',
        delta_t = delta_t,
        delayyearlength = delayyearlength,
        exog = ClimateExog,
        init = ClimateInit,
        parms = ClimateParms)
    
    return(ClimateFitMod)
}
ptm = proc.time() - ptm
print(ptm)
stopCluster(cl)

################# PLOT FITTED VALUES

ClimateFitData = CalibPlotFunc(ClimateResults,ClimateActual,ClimateParms,ClimateExog,
    ClimateInit,ClimateMod,delta_t,delayyearlength,'ClimateSubmodel')

SSRCoefPlot(ClimateResults,ClimateParStart,'ClimateSubmodel')
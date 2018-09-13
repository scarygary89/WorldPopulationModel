########################                       ########################
########################   RESOURCE SUBMODEL   ########################
########################                       ########################

library(foreach)
library(doParallel)
numcore = detectCores()
cl<-makeCluster(numcore) 
print(cl)

################# SUBMODEL WITH EXOGENOUS INPUTS
ResourceMod = function(t0,tf,delta_t,delayyearlength,exog,init,parms) {
	with(as.list(c(parms)), {	
		tspan = seq(from=t0, to=tf, by=delta_t)
		StockData = matrix(NA,
			nrow = (length(tspan) + 1),
			ncol = length(init)
			)
		colnames(StockData) = names(init)	
		stocks = init
		StockData[1,] = stocks
		for(i in 1:length(tspan)) {
  		# Assemble Lists and Vectors
			EconOutput_r = 	 c( Low = exog[i,'EconOutput_Low'],
								Mid = exog[i,'EconOutput_Mid'],
								High = exog[i,'EconOutput_High'])
			# Global Resources
			ResourceOut  	= Resource(
								stocks['CoalReserves'],
								stocks['OilReserves'],
								stocks['GasReserves'],
								EconOutput_r,
								parms)
	
			# STOCK VARIABLES
			dstocks = c(
				# Resource Stocks (Global)		
				ResourceOut[["dCoalReserves"]],
				ResourceOut[["dOilReserves"]],
				ResourceOut[["dGasReserves"]]
			)
			stocks = stocks + dstocks * delta_t
			stocks = pmax(stocks,1e-16)
			StockData[i+1,] = stocks
		}
	Output = cbind(tspan, StockData[-(length(tspan)+1),])
	colnames(Output)[1] = 'time'
	return(Output)
	})
}

################# COST FUNCTION

ResourceCost = function(p,t0,tf,delta_t,delayyearlength,exog,init,parms,yactual)
{
	whichpar = names(parms)[names(parms) %in% names(p)]
	whichinit = names(init)[names(init) %in% names(p)]
	parms[whichpar] = p[whichpar]
	init[whichinit] = p[whichinit]
	ysim = ResourceMod(t0,tf,delta_t,delayyearlength,exog,init,parms)
	RunCost = modCost(ysim,yactual,x = 'time',y = 'value',err = 'error',
		scaleVar=T)
	# print(RunCost)
	return(RunCost)
}

################# PARAMETER ESTIMATION FUNCTION

ResourceFit = function(parvalue,parmin,parmax,yactual,optmethod,control=list(),
	delta_t,delayyearlength,exog,init,parms)
{
	t0 = min(exog$time)
	tf = max(exog$time)
	ptm = proc.time() 
	ResourceFit = modFit(
		f = ResourceCost,
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
	return(ResourceFit)
}

################# PARAMETER ESTIMATION FUNCTION

ResourceParms = c( 
	CoalConsIntensity_Low = as.numeric(ParameterValue['CoalConsIntensity_Low']),	
	CoalConsIntensity_Mid = as.numeric(ParameterValue['CoalConsIntensity_Mid']),
	CoalConsIntensity_High = as.numeric(ParameterValue['CoalConsIntensity_High']),
	OilConsIntensity_Low = as.numeric(ParameterValue['OilConsIntensity_Low']),	
	OilConsIntensity_Mid = as.numeric(ParameterValue['OilConsIntensity_Mid']),
	OilConsIntensity_High = as.numeric(ParameterValue['OilConsIntensity_High']),
	GasConsIntensity_Low = as.numeric(ParameterValue['GasConsIntensity_Low']),	
	GasConsIntensity_Mid = as.numeric(ParameterValue['GasConsIntensity_Mid']),
	GasConsIntensity_High = as.numeric(ParameterValue['GasConsIntensity_High'])
)

################# DEFINE INITIAL PARAMETER VALUES

ResourceInit = c( 	
	CoalReserves = as.numeric(InitValue['CoalReserves']),
	OilReserves = as.numeric(InitValue['OilReserves']),
	GasReserves = as.numeric(InitValue['GasReserves'])
)


################# LOAD EXOGENOUS DATA

ResourceExog = na.omit(cbind(
    time = CalibData['time'],
    EconOutput_Low = CalibData[,'EconOutput_Low'],
    EconOutput_Mid = CalibData[,'EconOutput_Mid'],
    EconOutput_High = CalibData[,'EconOutput_High']
))

################# LOAD ACTUAL DATA

ResourceActual = na.omit(melt(
    data.frame(cbind(
        time = CalibData$time,
        CoalReserves = CalibData$CoalReserves,
        OilReserves = CalibData$OilReserves,
        GasReserves = CalibData$GasReserves)),
    id ='time'))
ResourceActual$variable = as.character(ResourceActual$variable)
ResourceActual = ResourceActual[,c('variable','time','value')]

ResourceActual$error[ResourceActual$variable == 'CoalReserves'] = 
    mean(ResourceActual$value[ResourceActual$variable == 'CoalReserves']) #/ 
    # (ResourceActual$time[ResourceActual$variable == 'CoalReserves'] - 1979) ^ 2
ResourceActual$error[ResourceActual$variable == 'OilReserves'] = 
    mean(ResourceActual$value[ResourceActual$variable == 'OilReserves']) #/ 
    # (ResourceActual$time[ResourceActual$variable == 'OilReserves'] - 1979) ^ 2
ResourceActual$error[ResourceActual$variable == 'GasReserves'] = 
    mean(ResourceActual$value[ResourceActual$variable == 'GasReserves']) #/ 
    # (ResourceActual$time[ResourceActual$variable == 'GasReserves'] - 1979) ^ 2


################# DEFINE CALIBRATION PARAMETERS

ResourceCalibPars =  rbind(
	CoalConsIntensity_Low = as.numeric(ParameterData['CoalConsIntensity_Low',]),
	CoalConsIntensity_Mid = as.numeric(ParameterData['CoalConsIntensity_Mid',]),
	CoalConsIntensity_High = as.numeric(ParameterData['CoalConsIntensity_High',]),
	OilConsIntensity_Low = as.numeric(ParameterData['OilConsIntensity_Low',]),
	OilConsIntensity_Mid = as.numeric(ParameterData['OilConsIntensity_Mid',]),
	OilConsIntensity_High = as.numeric(ParameterData['OilConsIntensity_High',]),
	GasConsIntensity_Low = as.numeric(ParameterData['GasConsIntensity_Low',]),
	GasConsIntensity_Mid = as.numeric(ParameterData['GasConsIntensity_Mid',]),
	GasConsIntensity_High = as.numeric(ParameterData['GasConsIntensity_High',])
)

ResourceParValue = ResourceCalibPars[,1]
ResourceParMin =  ResourceCalibPars[,2]
ResourceParMax = ResourceCalibPars[,3]
ResourceParRange = data.frame(min = ResourceParMin,max = ResourceParMax)

################# 2-STAGE FIT PARAMETERS

N = 1000
ResourceParStart = Latinhyper(ResourceParRange,N)
registerDoParallel(cl)
ptm = proc.time() 
ResourceResults = foreach(i=1:N,.packages='FME') %dopar%
{
    ResourceParValue = ResourceParStart[i,]
    
    ResourceFitPseudo = ResourceFit(
        parvalue = ResourceParValue,
        parmin = ResourceParMin,
        parmax = ResourceParMax,
        yactual = ResourceActual,
        optmethod = 'Pseudo',
        # control = list(numiter = 100000),
        delta_t = delta_t,
        delayyearlength = delayyearlength,
        exog = ResourceExog,
        init = ResourceInit,
        parms = ResourceParms)

    ResourceFitMod = ResourceFit(
        parvalue = coef(ResourceFitPseudo),
        parmin = ResourceParMin,
        parmax = ResourceParMax,
        yactual = ResourceActual,
        optmethod = 'Marq',
        delta_t = delta_t,
        delayyearlength = delayyearlength,
        exog = ResourceExog,
        init = ResourceInit,
        parms = ResourceParms)
    
    return(ResourceFitMod)
}
ptm = proc.time() - ptm
print(ptm)
stopCluster(cl)

################# PLOT FITTED VALUES

ResourceFitData = CalibPlotFunc(ResourceResults,ResourceActual,
	ResourceParms,ResourceExog,ResourceInit,ResourceMod,delta_t,
	delayyearlength,'ResourceSubmodel')

SSRCoefPlot(ResourceResults,ResourceParStart,'ResourceSubmodel')

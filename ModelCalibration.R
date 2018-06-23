print('----------------- CALIBRATE MODEL   ---------------------')

library(FME)

# IMPORT DATA


CalibData_Low = read.csv(  file = './DataInput/LowCountries.csv',sep =',',
						header = T, fileEncoding="UTF-8-BOM")

CalibData_Mid = read.csv(  file = './DataInput/MiddleCountries.csv',sep =',',
						header = T, fileEncoding="UTF-8-BOM")

CalibData_High = read.csv(  file = './DataInput/HighCountries.csv',sep =',',
						header = T, fileEncoding="UTF-8-BOM")

RPRatio = c(
	RM1 = .4,
	RM2 = .4,
	RM3 = .4,
	RM4 = .4,
	RF1 = .4,
	RF2 = .4,
	RF3 = .4,
	RF4 = .4,
	PM1 = .6,
	PM2 = .6,
	PM3 = .6,
	PM4 = .6,
	PF1 = .6,
	PF2 = .6,
	PF3 = .6,
	PF4 = .6)

PopDataLow = CalibData_Low[c('Pop.1.Male','Pop.2.Male','Pop.3.Male','Pop.4.Male',
					 'Pop.1.Female','Pop.2.Female','Pop.3.Female','Pop.4.Female',	
					 'Pop.1.Male','Pop.2.Male','Pop.3.Male','Pop.4.Male',
					 'Pop.1.Female','Pop.2.Female','Pop.3.Female','Pop.4.Female')]

PopDataMid = CalibData_Mid[c('Pop.1.Male','Pop.2.Male','Pop.3.Male','Pop.4.Male',
					 'Pop.1.Female','Pop.2.Female','Pop.3.Female','Pop.4.Female',	
					 'Pop.1.Male','Pop.2.Male','Pop.3.Male','Pop.4.Male',
					 'Pop.1.Female','Pop.2.Female','Pop.3.Female','Pop.4.Female')]


PopDataHigh = CalibData_High[c('Pop.1.Male','Pop.2.Male','Pop.3.Male','Pop.4.Male',
					 'Pop.1.Female','Pop.2.Female','Pop.3.Female','Pop.4.Female',	
					 'Pop.1.Male','Pop.2.Male','Pop.3.Male','Pop.4.Male',
					 'Pop.1.Female','Pop.2.Female','Pop.3.Female','Pop.4.Female')]

PopDataLow = t(RPRatio * t(PopDataLow))
PopDataMid = t(RPRatio * t(PopDataMid))
PopDataHigh = t(RPRatio * t(PopDataHigh))

PopData = cbind(CalibData_Low$Time,PopDataLow,PopDataMid,PopDataHigh)
colnames(PopData) = c('time','Low_RM1','Low_RM2','Low_RM3','Low_RM4','Low_RF1','Low_RF2','Low_RF3','Low_RF4',
					'Low_PM1','Low_PM2','Low_PM3','Low_PM4','Low_PF1','Low_PF2','Low_PF3','Low_PF4',
					'Mid_RM1','Mid_RM2','Mid_RM3','Mid_RM4','Mid_RF1','Mid_RF2','Mid_RF3','Mid_RF4',
					'Mid_PM1','Mid_PM2','Mid_PM3','Mid_PM4','Mid_PF1','Mid_PF2','Mid_PF3','Mid_PF4',
					'High_RM1','High_RM2','High_RM3','High_RM4','High_RF1','High_RF2','High_RF3','High_RF4',
					'High_PM1','High_PM2','High_PM3','High_PM4','High_PF1','High_PF2','High_PF3','High_PF4')


OtherData = cbind(CalibData_Low[c('Time','GDP','Total.Pop')],
				CalibData_Mid[c('GDP','Total.Pop')],
				CalibData_High[c('GDP','Total.Pop')])
colnames(OtherData) = c('time','EconOutput_Low','LowPop','EconOutput_Mid',
						'MidPop','EconOutput_High','HighPop')

ObsData = merge(PopData,OtherData)

yobs = na.omit(melt(ObsData,id='time'))
yobs$variable = as.character(yobs$variable)
yobs = yobs[,c('variable','time','value')]
obstime = sort(unique(yobs[,'time']))


ObjCost = function(p, simObj, obstime, yobs){
	whichpar = names(p)
	parms(simObj)[whichpar] = p
	times(simObj) = obstime
	ysim = out(sim(simObj))
	modCost(ysim,yobs,x = 'time',y = 'value')
}

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
	'OmegaH_F4'
)

ParValue = setNames(ParameterData[CalibParms,'value'],CalibParms)
ParMin = setNames(ParameterData[CalibParms,'min'],CalibParms)
ParMax = setNames(ParameterData[CalibParms,'max'],CalibParms)


ptm = proc.time() 
Fit = modFit(p = ParValue, lower = ParMin, upper = ParMax, f = ObjCost, 
		simObj=WorldMod, obstime=obstime, yobs=yobs, method="Pseudo") 
		# control=list(trace=T))
ptm = proc.time() - ptm
print(ptm)
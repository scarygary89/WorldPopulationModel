print('----------------- CALIBRATE MODEL   ---------------------')

library(FME)

# IMPORT DATA

orgdir = getwd()
setwd('./DataInput')

CalibData_Low = read.csv(  file = 'LowCountries.csv',sep =',',
						header = T, fileEncoding="UTF-8-BOM")

CalibData_Mid = read.csv(  file = 'MiddleCountries.csv',sep =',',
						header = T, fileEncoding="UTF-8-BOM")

CalibData_High = read.csv(  file = 'HighCountries.csv',sep =',',
						header = T, fileEncoding="UTF-8-BOM")

RPRatio = c(
	RM1 = .1,
	RM2 = .1,
	RM3 = .1,
	RM4 = .1,
	RF1 = .1,
	RF2 = .1,
	RF3 = .1,
	RF4 = .1,
	PM1 = .9,
	PM2 = .9,
	PM3 = .9,
	PM4 = .9,
	PF1 = .9,
	PF2 = .9,
	PF3 = .9,
	PF4 = .9)

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



# parms(WorldMod) = c(parms(WorldMod), init(WorldMod))
# initfunc(WorldMod) = function(obj){
# 	init(obj) = parms(obj)[c('RM1','RM2','RM3','RM4','RF1','RF2','RF3','RF4',
# 							 'PM1','PM2','PM3','PM4','PF1','PF2','PF3','PF4')]
# 	obj
# }

parRanges = ParameterData[,c('min','max')]
yobs = na.omit(PopData)
obstime = yobs[,'time']
# obstime = times(WorldMod)

ObjCost = function(p, simObj, obstime, yobs){
	whichpar = names(p)
	parms(simObj)[whichpar] = p
	times(simObj) = obstime
	ysim = out(sim(simObj))
	modCost(ysim,yobs)
}


Fit = modFit(p = c(OmegaF_PF1 = 10,OmegaF_PF2 = 10,OmegaF_PF3 = 10), 
		f = ObjCost, simObj=WorldMod,
		obstime=obstime, yobs=yobs, method="Nelder", control=list(trace=T))

setwd(orgdir)
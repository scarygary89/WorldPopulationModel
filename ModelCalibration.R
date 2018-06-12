print('----------------- CALIBRATE MODEL   ---------------------')

library(FME)

# IMPORT DATA

orgdir = getwd()
setwd('~/../Dropbox/HumanPopDynModel/Model/R/DataInput')

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

PopData = CalibData_Low[c('Pop.1.Male','Pop.2.Male','Pop.3.Male','Pop.4.Male',
					 'Pop.1.Female','Pop.2.Female','Pop.3.Female','Pop.4.Female',	
					 'Pop.1.Male','Pop.2.Male','Pop.3.Male','Pop.4.Male',
					 'Pop.1.Female','Pop.2.Female','Pop.3.Female','Pop.4.Female')]

PopData = t(RPRatio * t(PopData))
PopData = cbind(CalibData_Low$Time,PopData)
colnames(PopData) = c('time','RM1','RM2','RM3','RM4','RF1','RF2','RF3','RF4',
					'PM1','PM2','PM3','PM4','PF1','PF2','PF3','PF4')



# parms(WorldMod) = c(parms(WorldMod), init(WorldMod))
# initfunc(WorldMod) = function(obj){
# 	init(obj) = parms(obj)[c('RM1','RM2','RM3','RM4','RF1','RF2','RF3','RF4',
# 							 'PM1','PM2','PM3','PM4','PF1','PF2','PF3','PF4')]
# 	obj
# }

parRanges = read.csv(  file = 'ParameterInput.csv',sep ='|',
						header = T, fileEncoding="UTF-8-BOM")

# obstime = PopData[,'time']
obstime = times(WorldMod)

ObjCost = function(p, simObj, obstime, yobs){
	whichpar = names(p)
	parms(simObj)[whichpar] = p
	times(simObj) = obstime
	ysim = out(sim(simObj))
	modCost(ysim,yobs)
}

yobs = PopData

Fit = modFit(p = c(OmegaF_PF1 = 10,OmegaF_PF2 = 10,OmegaF_PF3 = 10), 
		f = ObjCost, simObj=WorldMod,
		obstime=obstime, yobs=yobs, method="Nelder", control=list(trace=T))

setwd(orgdir)
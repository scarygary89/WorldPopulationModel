########################                       ########################
########################  PLOTTING FUNCTION    ########################
########################                       ########################

library(ggplot2)
library(reshape)
library(gridExtra)

MultiTimePlot = function(time,y,ybos,xtit,ytit,tit){
	dat = data.frame(cbind(time,y))
	colnames(dat)[1] = 'time' 
	meltdat = melt(dat,id = 'time')
	obsdata = yobs[yobs$variable %in% meltdat$variable,]
	ggplot(data=meltdat, aes(x = time,y=value, col = variable)) + 
		geom_line() + geom_point(data = obsdata) + theme_bw() + 
		labs(title = tit, x = xtit, y = ytit) 

}


PlotFuncWithObs = function(OutputData){
	# Plot Results
	LowMalePlot = MultiTimePlot(OutputData[,'time'],OutputData[,c('Low_M1','Low_M2','Low_M3','Low_M4')],
		yobs, "Year", "Population","Male -- Low Income")

	LowFemalePlot = MultiTimePlot(OutputData[,'time'],OutputData[,c('Low_F1','Low_F2','Low_F3','Low_F4')],
		yobs,"Year", "Population","Female -- Low Income")

	MidMalePlot = MultiTimePlot(OutputData[,'time'],OutputData[,c('Mid_M1','Mid_M2','Mid_M3','Mid_M4')],
		yobs,"Year", "Population","Male -- Middle Income")

	MidFemalePlot = MultiTimePlot(OutputData[,'time'],OutputData[,c('Mid_F1','Mid_F2','Mid_F3','Mid_F4')],
		yobs,"Year", "Population","Female -- Middle Income")

	HighMalePlot = MultiTimePlot(OutputData[,'time'],OutputData[,c('High_M1','High_M2','High_M3','High_M4')],
		yobs,"Year", "Population","Male -- High Income")

	HighFemalePlot = MultiTimePlot(OutputData[,'time'],OutputData[,c('High_F1','High_F2','High_F3','High_F4')],
		yobs,"Year", "Population","Female -- High Income")

	TotalPopPlot = MultiTimePlot(OutputData[,'time'],OutputData[,c('LowPop','MidPop','HighPop')],
		yobs,"Year","Population","Regional Population")

	EconOutPlot = MultiTimePlot(OutputData[,'time'],OutputData[,c('EconOutput_Low','EconOutput_Mid','EconOutput_High')],
		yobs,"Year","GDP","Economic Output")

	HealthPlot = MultiTimePlot(OutputData[,'time'],OutputData[,c('HealthServices_Low','HealthServices_Mid','HealthServices_High')],
		yobs,"Year","Services","Health Services")

	EducationPlot = MultiTimePlot(OutputData[,'time'],OutputData[,c('EducationServices_Low','EducationServices_Mid','EducationServices_High')],
		yobs,"Year","Services","Education Services")

	LandPlot = MultiTimePlot(OutputData[,'time'],OutputData[,c('CropLand','GrazeLand')],
		yobs,"Year","Land Area","Land")

	FoodPlot = MultiTimePlot(OutputData[,'time'],OutputData[,c('Fishstock','Livestock','Crops')],
		yobs,"Year","Weight","Food")

	FishPlot = MultiTimePlot(OutputData[,'time'],OutputData[,'Fisheries'],
		yobs,"Year","Area","Fisheries")

	WaterPlot = MultiTimePlot(OutputData[,'time'],OutputData[,'Freshwater'],
		yobs,"Year","Volume","Freshwater")

	FemaleHealthAccessPlot = MultiTimePlot(OutputData[,'time'],
		OutputData[,c( 	'FemaleHealthAccess_Low',
						'FemaleHealthAccess_Mid',
						'FemaleHealthAccess_High')],
		yobs,"Year", "Access to health facility during delivery (Percent)",
		"female health access")

	RichHealthAccessPlot = MultiTimePlot(OutputData[,'time'],
		OutputData[,c( 	'GeneralHealthAccess_RichLow',
						'GeneralHealthAccess_RichMid',
						'GeneralHealthAccess_RichHigh')],
		yobs,"Year", "Health Access and Quality Index",
		"Rich general health access")

	PoorHealthAccessPlot = MultiTimePlot(OutputData[,'time'],
		OutputData[,c( 	'GeneralHealthAccess_PoorLow',
						'GeneralHealthAccess_PoorMid',
						'GeneralHealthAccess_PoorHigh')],
		yobs,"Year", "Health Access and Quality Index",
		"Poor general health access")

	GFRPlot = MultiTimePlot(OutputData[,'time'],
		OutputData[,c( 	'GFR_Low',
						'GFR_Mid',
						'GFR_High')],
		yobs,"Year", "Births per 1000 people/year",
		"General Fertility Rate")

	TempAnamPlot = MultiTimePlot(OutputData[,'time'],
		OutputData[,c('TempAnamoly')],
		yobs,"Year", "Celsius",
		"Global Land and Ocean Temperature Anomalies")

	
	CO2EmissionPlot = MultiTimePlot(OutputData[,'time'],
		OutputData[,c('CO2Emission_Low','CO2Emission_Mid','CO2Emission_High')],
		yobs,"Year", "PPM",
		"CO2 Emissions per capita")
	
	pdf(file='OutputFiles/LowIncomePop-OUT.pdf')
	print(grid.arrange(LowMalePlot,LowFemalePlot,nrow = 2,ncol = 1))
	dev.off()

	pdf(file='OutputFiles/MidIncomePop-OUT.pdf')
	print(grid.arrange(MidMalePlot,MidFemalePlot,nrow = 2,ncol = 1))
	dev.off()

	pdf(file='OutputFiles/HighIncomePop-OUT.pdf')
	print(grid.arrange(HighMalePlot,HighFemalePlot,nrow = 2,ncol = 1))
	dev.off()

	pdf(file='OutputFiles/TotPopEconHealthEdu-OUT.pdf')
	print(grid.arrange(TotalPopPlot,EconOutPlot,HealthPlot,EducationPlot,nrow = 2,ncol = 2))
	dev.off()

	pdf(file='OutputFiles/TotPopEconHealthEdu-OUT.pdf')
	print(grid.arrange(LandPlot,FoodPlot,FishPlot,WaterPlot,nrow = 2,ncol = 2))
	dev.off()

	pdf(file='OutputFiles/HealthAccess-OUT.pdf')
	print(grid.arrange(FemaleHealthAccessPlot,RichHealthAccessPlot,
		PoorHealthAccessPlot,nrow = 2,ncol = 2))
	dev.off()

	pdf(file='OutputFiles/ClimateSys-OUT.pdf')
	print(grid.arrange(TempAnamPlot,CO2EmissionPlot,nrow = 2,ncol = 1))
	dev.off()

	pdf(file='OutputFiles/GFR-OUT.pdf')
	print(GFRPlot)
	dev.off()
}

CalibPlotFunc = function(CalibModResults,ObsData,Parms,Exog,Init,
	CalibMod,delta_t,delayyearlength,tit) {
	if (class(CalibModResults) == 'ga'|class(CalibModResults) == 'gaisl')
	{
		BestParms = CalibModResults@solution[1,] 
	}
	else 
	{
		SSR = sapply(CalibModResults,function(x) x$ssr)
		BestFit = CalibModResults[[which.min(SSR[which(SSR != 0)])]]
		BestParms = coef(BestFit)
	}
	FitParm  = Parms
	FitParm[names(BestParms)] = BestParms
	FitData = CalibMod(min(Exog$time),max(Exog$time),delta_t,
		delayyearlength,Exog,Init,FitParm)
	FitData = data.frame(FitData)
	meltdat = melt(FitData, id.vars = 'time')
    Vars = unique(ObsData$variable)
	for(i in 1: length(Vars)) {
		obsdata = ObsData[ObsData$variable %in% Vars[i],]
		actdat = meltdat[meltdat$variable %in% Vars[i],]
		obsdata$variable = 'Target'
		actdat$variable = 'Simulated'
		CalibPlot = ggplot(data=actdat, aes(x = time,y=value, col = variable)) + 
			geom_line() + geom_point(data = obsdata) + theme_bw() + 
			labs(title = tit, y = Vars[i], x = 'Year') + expand_limits(y = 0)
		pdf(file=paste('Calibration/CalibrationOutput/Plots/',Vars[i],tit,'.pdf',sep=''))
			# height = 10,width=10)
		print(CalibPlot)
		dev.off()
	}
	return(FitData)
} 



SSRCoefPlot = function(CalibModResults,ParStart,tit) {
	coefvalues = sapply(CalibModResults, function(x) coef(x))
	SSR = sapply(CalibModResults,function(x) x$ssr)
	for(i in 1:dim(coefvalues)[1]) {
		plotdat = data.frame(cbind(
			as.numeric(coefvalues[i,]),
			as.numeric(SSR),
			as.numeric(ParStart[,i])))
		names(plotdat) = c('FitCoefValue','SSR','StartCoefValue')
		plotdat$logSSR = log(plotdat$SSR)
		plotdat = plotdat[which(plotdat[,'logSSR'] != Inf),]
		SSRPlot = ggplot(data=plotdat, aes(x = StartCoefValue,
			y=FitCoefValue, col = logSSR)) + geom_point() + 
			labs(title = paste(rownames(coefvalues)[i],tit)) + theme_bw()
		pdf(file=paste('Calibration/CalibrationOutput/SSRPlots/',rownames(coefvalues)[i],
			tit,'.pdf',sep=''))
		print(SSRPlot)
		dev.off()
	}
}

CostPlot = function(PopGlobalSearch,ParStart,tit) {
	Cost = sapply(PopGlobalSearch,function(x) x$model)
	for(i in 1:dim(ParStart)[2]) {
		plotdat = data.frame(cbind(
			as.numeric(ParStart[,i]),
			as.numeric(Cost)))
		names(plotdat) = c('CoefValue','Cost')
		plotdat$logCost = log(plotdat$Cost)
		plotdat = plotdat[which(plotdat[,'logCost'] != Inf),]
		CostPlot = ggplot(data=plotdat, aes(x = CoefValue,
			y=logCost, col = logCost)) + geom_point() + 
			labs(title = paste(colnames(ParStart)[i],tit)) + theme_bw()
		pdf(file=paste('Calibration/CalibrationOutput/CostPlots/',colnames(ParStart)[i],
			tit,'.pdf',sep=''))
		print(CostPlot)
		dev.off()
	}
}
########################                       ########################
########################  PLOTTING FUNCTION    ########################
########################                       ########################

library(ggplot2)
library(reshape)
library(gridExtra)

MultiTimePlot = function(time,y,xtit,ytit,tit){
	dat = cbind(time,y)
	meltdat = melt(dat,id = 'time')
	ggplot(data=meltdat, aes(x = time,y=value, col = variable)) + 
		geom_line() + theme_bw() + labs(title = tit, x = xtit, y = ytit) 

}

PlotFunc = function(OutputData){
	# Plot Results
	LowRichMalePlot = MultiTimePlot(OutputData['time'],OutputData[c('Low_RM1','Low_RM2','Low_RM3','Low_RM4')],
		"Year", "Population","Rich Male -- Low Income")

	LowRichFemalePlot = MultiTimePlot(OutputData['time'],OutputData[c('Low_RF1','Low_RF2','Low_RF3','Low_RF4')],
		"Year", "Population","Rich Female -- Low Income")

	LowPoorMalePlot = MultiTimePlot(OutputData['time'],OutputData[c('Low_PM1','Low_PM2','Low_PM3','Low_PM4')],
		"Year", "Population","Poor Male -- Low Income")

	LowPoorFemalePlot = MultiTimePlot(OutputData['time'],OutputData[c('Low_PF1','Low_PF2','Low_PF3','Low_PF4')],
		"Year", "Population","Poor Female -- Low Income")

	MidRichMalePlot = MultiTimePlot(OutputData['time'],OutputData[c('Mid_RM1','Mid_RM2','Mid_RM3','Mid_RM4')],
		"Year", "Population","Rich Male -- Middle Income")

	MidRichFemalePlot = MultiTimePlot(OutputData['time'],OutputData[c('Mid_RF1','Mid_RF2','Mid_RF3','Mid_RF4')],
		"Year", "Population","Rich Female -- Middle Income")

	MidPoorMalePlot = MultiTimePlot(OutputData['time'],OutputData[c('Mid_PM1','Mid_PM2','Mid_PM3','Mid_PM4')],
		"Year", "Population","Poor Male -- Middle Income")

	MidPoorFemalePlot = MultiTimePlot(OutputData['time'],OutputData[c('Mid_PF1','Mid_PF2','Mid_PF3','Mid_PF4')],
		"Year", "Population","Poor Female -- Middle Income")

	HighRichMalePlot = MultiTimePlot(OutputData['time'],OutputData[c('High_RM1','High_RM2','High_RM3','High_RM4')],
		"Year", "Population","Rich Male -- High Income")

	HighRichFemalePlot = MultiTimePlot(OutputData['time'],OutputData[c('High_RF1','High_RF2','High_RF3','High_RF4')],
		"Year", "Population","Rich Female -- High Income")

	HighPoorMalePlot = MultiTimePlot(OutputData['time'],OutputData[c('High_PM1','High_PM2','High_PM3','High_PM4')],
		"Year", "Population","Poor Male -- High Income")

	HighPoorFemalePlot = MultiTimePlot(OutputData['time'],OutputData[c('High_PF1','High_PF2','High_PF3','High_PF4')],
		"Year", "Population","Poor Female -- High Income")

	TotalPopPlot = MultiTimePlot(OutputData['time'],OutputData[c('LowPop','MidPop','HighPop')],
		"Year","Population","Regional Population")

	EconOutPlot = MultiTimePlot(OutputData['time'],OutputData[c('EconOutput_Low','EconOutput_Mid','EconOutput_High')],
		"Year","US Dollars (GDP)","Economic Output")

	HealthPlot = MultiTimePlot(OutputData['time'],OutputData[c('HealthServices_Low','HealthServices_Mid','HealthServices_High')],
		"Year","Services","Health Services")

	EducationPlot = MultiTimePlot(OutputData['time'],OutputData[c('EducationServices_Low','EducationServices_Mid','EducationServices_High')],
		"Year","Services","Education Services")

	LandPlot = MultiTimePlot(OutputData['time'],OutputData[c('CropLand','GrazeLand')],
		"Year","Land Area","Land")

	FoodPlot = MultiTimePlot(OutputData['time'],OutputData[c('Fishstock','Livestock','Crops')],
		"Year","Weight","Food")

	FishPlot = MultiTimePlot(OutputData['time'],OutputData['Fisheries'],
		"Year","Area","Fisheries")

	WaterPlot = MultiTimePlot(OutputData['time'],OutputData['Freshwater'],
		"Year","Volume","Freshwater")

	RichFemaleHealthAccessPlot = MultiTimePlot(OutputData['time'],
		OutputData[c( 	'FemaleHealthAccess_RichLow',
						'FemaleHealthAccess_RichMid',
						'FemaleHealthAccess_RichHigh')],
		"Year", "Access to health facility during delivery (Percent)",
		"Rich female health access")

	PoorFemaleHealthAccessPlot = MultiTimePlot(OutputData['time'],
		OutputData[c( 	'FemaleHealthAccess_PoorLow',
						'FemaleHealthAccess_PoorMid',
						'FemaleHealthAccess_PoorHigh')],
		"Year", "Access to health facility during delivery (Percent)",
		"Poor female health access")

	RichHealthAccessPlot = MultiTimePlot(OutputData['time'],
		OutputData[c( 	'GeneralHealthAccess_RichLow',
						'GeneralHealthAccess_RichMid',
						'GeneralHealthAccess_RichHigh')],
		"Year", "Health Access and Quality Index",
		"Rich general health access")

	PoorHealthAccessPlot = MultiTimePlot(OutputData['time'],
		OutputData[c( 	'GeneralHealthAccess_PoorHigh',
						'GeneralHealthAccess_PoorMid',
						'GeneralHealthAccess_PoorHigh')],
		"Year", "Health Access and Quality Index",
		"Poor general health access")

	dev.new()
	print(grid.arrange(LowRichMalePlot,LowRichFemalePlot,LowPoorMalePlot,LowPoorFemalePlot,nrow = 2,ncol = 2))

	dev.new()
	print(grid.arrange(MidRichMalePlot,MidRichFemalePlot,MidPoorMalePlot,MidPoorFemalePlot,nrow = 2,ncol = 2))

	dev.new()
	print(grid.arrange(HighRichMalePlot,HighRichFemalePlot,HighPoorMalePlot,HighPoorFemalePlot,nrow = 2,ncol = 2))

	dev.new()
	print(grid.arrange(TotalPopPlot,EconOutPlot,HealthPlot,EducationPlot,nrow = 2,ncol = 2))

	dev.new()
	print(grid.arrange(LandPlot,FoodPlot,FishPlot,WaterPlot,nrow = 2,ncol = 2))

	dev.new()
	print(grid.arrange(RichFemaleHealthAccessPlot,PoorFemaleHealthAccessPlot,RichHealthAccessPlot,PoorHealthAccessPlot,nrow = 2,ncol = 2))
}


MultiTimePlot2 = function(time,y,ybos,xtit,ytit,tit){
	dat = cbind(time,y)
	meltdat = melt(dat,id = 'time')
	obsdata = yobs[yobs$variable %in% meltdat$variable,]
	ggplot(data=meltdat, aes(x = time,y=value, col = variable)) + 
		geom_line() + geom_point(data = obsdata) + theme_bw() + labs(title = tit, x = xtit, y = ytit) 

}


PlotFuncWithObs = function(OutputData){
	# Plot Results
	LowRichMalePlot = MultiTimePlot2(OutputData['time'],OutputData[c('Low_RM1','Low_RM2','Low_RM3','Low_RM4')],
		yobs, "Year", "Population","Rich Male -- Low Income")

	LowRichFemalePlot = MultiTimePlot2(OutputData['time'],OutputData[c('Low_RF1','Low_RF2','Low_RF3','Low_RF4')],
		yobs,"Year", "Population","Rich Female -- Low Income")

	LowPoorMalePlot = MultiTimePlot2(OutputData['time'],OutputData[c('Low_PM1','Low_PM2','Low_PM3','Low_PM4')],
		yobs,"Year", "Population","Poor Male -- Low Income")

	LowPoorFemalePlot = MultiTimePlot2(OutputData['time'],OutputData[c('Low_PF1','Low_PF2','Low_PF3','Low_PF4')],
		yobs,"Year", "Population","Poor Female -- Low Income")

	MidRichMalePlot = MultiTimePlot2(OutputData['time'],OutputData[c('Mid_RM1','Mid_RM2','Mid_RM3','Mid_RM4')],
		yobs,"Year", "Population","Rich Male -- Middle Income")

	MidRichFemalePlot = MultiTimePlot2(OutputData['time'],OutputData[c('Mid_RF1','Mid_RF2','Mid_RF3','Mid_RF4')],
		yobs,"Year", "Population","Rich Female -- Middle Income")

	MidPoorMalePlot = MultiTimePlot2(OutputData['time'],OutputData[c('Mid_PM1','Mid_PM2','Mid_PM3','Mid_PM4')],
		yobs,"Year", "Population","Poor Male -- Middle Income")

	MidPoorFemalePlot = MultiTimePlot2(OutputData['time'],OutputData[c('Mid_PF1','Mid_PF2','Mid_PF3','Mid_PF4')],
		yobs,"Year", "Population","Poor Female -- Middle Income")

	HighRichMalePlot = MultiTimePlot2(OutputData['time'],OutputData[c('High_RM1','High_RM2','High_RM3','High_RM4')],
		yobs,"Year", "Population","Rich Male -- High Income")

	HighRichFemalePlot = MultiTimePlot2(OutputData['time'],OutputData[c('High_RF1','High_RF2','High_RF3','High_RF4')],
		yobs,"Year", "Population","Rich Female -- High Income")

	HighPoorMalePlot = MultiTimePlot2(OutputData['time'],OutputData[c('High_PM1','High_PM2','High_PM3','High_PM4')],
		yobs,"Year", "Population","Poor Male -- High Income")

	HighPoorFemalePlot = MultiTimePlot2(OutputData['time'],OutputData[c('High_PF1','High_PF2','High_PF3','High_PF4')],
		yobs,"Year", "Population","Poor Female -- High Income")

	TotalPopPlot = MultiTimePlot2(OutputData['time'],OutputData[c('LowPop','MidPop','HighPop')],
		yobs,"Year","Population","Regional Population")

	EconOutPlot = MultiTimePlot2(OutputData['time'],OutputData[c('EconOutput_Low','EconOutput_Mid','EconOutput_High')],
		yobs,"Year","GDP","Economic Output")

	HealthPlot = MultiTimePlot2(OutputData['time'],OutputData[c('HealthServices_Low','HealthServices_Mid','HealthServices_High')],
		yobs,"Year","Services","Health Services")

	EducationPlot = MultiTimePlot2(OutputData['time'],OutputData[c('EducationServices_Low','EducationServices_Mid','EducationServices_High')],
		yobs,"Year","Services","Education Services")

	LandPlot = MultiTimePlot2(OutputData['time'],OutputData[c('CropLand','GrazeLand')],
		yobs,"Year","Land Area","Land")

	FoodPlot = MultiTimePlot2(OutputData['time'],OutputData[c('Fishstock','Livestock','Crops')],
		yobs,"Year","Weight","Food")

	FishPlot = MultiTimePlot2(OutputData['time'],OutputData['Fisheries'],
		yobs,"Year","Area","Fisheries")

	WaterPlot = MultiTimePlot2(OutputData['time'],OutputData['Freshwater'],
		yobs,"Year","Volume","Freshwater")

	RichFemaleHealthAccessPlot = MultiTimePlot2(OutputData['time'],
		OutputData[c( 	'FemaleHealthAccess_RichLow',
						'FemaleHealthAccess_RichMid',
						'FemaleHealthAccess_RichHigh')],
		yobs,"Year", "Access to health facility during delivery (Percent)",
		"Rich female health access")

	PoorFemaleHealthAccessPlot = MultiTimePlot2(OutputData['time'],
		OutputData[c( 	'FemaleHealthAccess_PoorLow',
						'FemaleHealthAccess_PoorMid',
						'FemaleHealthAccess_PoorHigh')],
		yobs,"Year", "Access to health facility during delivery (Percent)",
		"Poor female health access")

		RichHealthAccessPlot = MultiTimePlot2(OutputData['time'],
		OutputData[c( 	'GeneralHealthAccess_RichLow',
						'GeneralHealthAccess_RichMid',
						'GeneralHealthAccess_RichHigh')],
		yobs,"Year", "Health Access and Quality Index",
		"Rich general health access")

	PoorHealthAccessPlot = MultiTimePlot2(OutputData['time'],
		OutputData[c( 	'GeneralHealthAccess_PoorHigh',
						'GeneralHealthAccess_PoorMid',
						'GeneralHealthAccess_PoorHigh')],
		yobs,"Year", "Health Access and Quality Index",
		"Poor general health access")



	dev.new()
	print(grid.arrange(LowRichMalePlot,LowRichFemalePlot,LowPoorMalePlot,LowPoorFemalePlot,nrow = 2,ncol = 2))

	dev.new()
	print(grid.arrange(MidRichMalePlot,MidRichFemalePlot,MidPoorMalePlot,MidPoorFemalePlot,nrow = 2,ncol = 2))

	dev.new()
	print(grid.arrange(HighRichMalePlot,HighRichFemalePlot,HighPoorMalePlot,HighPoorFemalePlot,nrow = 2,ncol = 2))

	dev.new()
	print(grid.arrange(TotalPopPlot,EconOutPlot,HealthPlot,EducationPlot,nrow = 2,ncol = 2))

	dev.new()
	print(grid.arrange(LandPlot,FoodPlot,FishPlot,WaterPlot,nrow = 2,ncol = 2))

	dev.new()
	print(grid.arrange(RichFemaleHealthAccessPlot,PoorFemaleHealthAccessPlot,RichHealthAccessPlot,PoorHealthAccessPlot,nrow = 2,ncol = 2))
}
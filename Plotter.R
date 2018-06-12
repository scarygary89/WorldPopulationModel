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
	"Year","GDP","Economic Output")

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
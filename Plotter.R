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
RichMalePlot = MultiTimePlot(OutputData['time'],OutputData[c('RM1','RM2','RM3','RM4')],
	"Year", "Population","Rich Male")

RichFemalePlot = MultiTimePlot(OutputData['time'],OutputData[c('RF1','RF2','RF3','RF4')],
	"Year", "Population","Rich Female")

PoorMalePlot = MultiTimePlot(OutputData['time'],OutputData[c('PM1','PM2','PM3','PM4')],
	"Year", "Population","Poor Male")

PoorFemalePlot = MultiTimePlot(OutputData['time'],OutputData[c('PF1','PF2','PF3','PF4')],
	"Year", "Population","Poor Female")

TotalPopPlot = MultiTimePlot(OutputData['time'],OutputData['TotalPop'],
	"Year","Population","Total Population")

EconOutPlot = MultiTimePlot(OutputData['time'],OutputData['EconOutput'],
	"Year","GDP","Economic Output")

HealthPlot = MultiTimePlot(OutputData['time'],OutputData['HealthServices'],
	"Year","Services","Health Services")

EducationPlot = MultiTimePlot(OutputData['time'],OutputData['EducationServices'],
	"Year","Services","Education Services")

dev.new()
print(grid.arrange(RichMalePlot,RichFemalePlot,PoorMalePlot,PoorFemalePlot,nrow = 2,ncol = 2))

dev.new()
print(grid.arrange(TotalPopPlot,EconOutPlot,HealthPlot,EducationPlot,nrow = 2,ncol = 2))



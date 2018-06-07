

# IMPORT DATA

orgdir = getwd()
setwd('~/../Dropbox/HumanPopDynModel/Model/R/DataInput')

InitialData = read.csv(  file = 'InitialValueInput.csv',sep ='|',row.names = 1,
						header = T, fileEncoding="UTF-8-BOM")

ParameterData = read.csv(  file = 'ParameterInput.csv',sep ='|',row.names = 1,
						header = T, fileEncoding="UTF-8-BOM",)

LowPop_RM1 = InitialData['LowPop_RM1','value']
LowPop_RM2 = InitialData['LowPop_RM2','value']
LowPop_RM3 = InitialData['LowPop_RM3','value']
LowPop_RM4 = InitialData['LowPop_RM4','value']
LowPop_RF1 = InitialData['LowPop_RF1','value']
LowPop_RF2 = InitialData['LowPop_RF2','value']
LowPop_RF3 = InitialData['LowPop_RF3','value']
LowPop_RF4 = InitialData['LowPop_RF4','value']         
LowPop_PM1 = InitialData['LowPop_PM1','value']
LowPop_PM2 = InitialData['LowPop_PM2','value']
LowPop_PM3 = InitialData['LowPop_PM3','value']
LowPop_PM4 = InitialData['LowPop_PM4','value']
LowPop_PF1 = InitialData['LowPop_PF1','value']
LowPop_PF2 = InitialData['LowPop_PF2','value']
LowPop_PF3 = InitialData['LowPop_PF3','value']
LowPop_PF4 = InitialData['LowPop_PF4','value']

MidPop_RM1 = InitialData['MidPop_RM1','value']
MidPop_RM2 = InitialData['MidPop_RM2','value']
MidPop_RM3 = InitialData['MidPop_RM3','value']
MidPop_RM4 = InitialData['MidPop_RM4','value']
MidPop_RF1 = InitialData['MidPop_RF1','value']
MidPop_RF2 = InitialData['MidPop_RF2','value']
MidPop_RF3 = InitialData['MidPop_RF3','value']
MidPop_RF4 = InitialData['MidPop_RF4','value']         
MidPop_PM1 = InitialData['MidPop_PM1','value']
MidPop_PM2 = InitialData['MidPop_PM2','value']
MidPop_PM3 = InitialData['MidPop_PM3','value']
MidPop_PM4 = InitialData['MidPop_PM4','value']
MidPop_PF1 = InitialData['MidPop_PF1','value']
MidPop_PF2 = InitialData['MidPop_PF2','value']
MidPop_PF3 = InitialData['MidPop_PF3','value']
MidPop_PF4 = InitialData['MidPop_PF4','value']

HighPop_RM1 = InitialData['HighPop_RM1','value']
HighPop_RM2 = InitialData['HighPop_RM2','value']
HighPop_RM3 = InitialData['HighPop_RM3','value']
HighPop_RM4 = InitialData['HighPop_RM4','value']
HighPop_RF1 = InitialData['HighPop_RF1','value']
HighPop_RF2 = InitialData['HighPop_RF2','value']
HighPop_RF3 = InitialData['HighPop_RF3','value']
HighPop_RF4 = InitialData['HighPop_RF4','value']         
HighPop_PM1 = InitialData['HighPop_PM1','value']
HighPop_PM2 = InitialData['HighPop_PM2','value']
HighPop_PM3 = InitialData['HighPop_PM3','value']
HighPop_PM4 = InitialData['HighPop_PM4','value']
HighPop_PF1 = InitialData['HighPop_PF1','value']
HighPop_PF2 = InitialData['HighPop_PF2','value']
HighPop_PF3 = InitialData['HighPop_PF3','value']
HighPop_PF4 = InitialData['HighPop_PF4','value']

InitLowTotPop = sum(
		LowPop_RM1,
		LowPop_RM2,
		LowPop_RM3,
		LowPop_RM4,
		LowPop_RF1,
		LowPop_RF2,
		LowPop_RF3,
		LowPop_RF4, 
		LowPop_PM1,
		LowPop_PM2,
		LowPop_PM3,
		LowPop_PM4,
		LowPop_PF1,
		LowPop_PF2,
		LowPop_PF3,
		LowPop_PF4)

InitMidTotPop = sum(
		MidPop_RM1,
		MidPop_RM2,
		MidPop_RM3,
		MidPop_RM4,
		MidPop_RF1,
		MidPop_RF2,
		MidPop_RF3,
		MidPop_RF4, 
		MidPop_PM1,
		MidPop_PM2,
		MidPop_PM3,
		MidPop_PM4,
		MidPop_PF1,
		MidPop_PF2,
		MidPop_PF3,
		MidPop_PF4)

InitHighTotPop = sum(
		HighPop_RM1,
		HighPop_RM2,
		HighPop_RM3,
		HighPop_RM4,
		HighPop_RF1,
		HighPop_RF2,
		HighPop_RF3,
		HighPop_RF4, 
		HighPop_PM1,
		HighPop_PM2,
		HighPop_PM3,
		HighPop_PM4,
		HighPop_PF1,
		HighPop_PF2,
		HighPop_PF3,
		HighPop_PF4)

RenewableInputElast_Low = ParameterData['RenewableInputElast_Low','value']
RenewableInputElast_Mid = ParameterData['RenewableInputElast_Mid','value']
RenewableInputElast_High = ParameterData['RenewableInputElast_High','value']

NonrenewableInputElast_Low = ParameterData['NonrenewableInputElast_Low','value']
NonrenewableInputElast_Mid = ParameterData['NonrenewableInputElast_Mid','value']
NonrenewableInputElast_High = ParameterData['NonrenewableInputElast_High','value']

CapitalInputElast_Low = ParameterData['CapitalInputElast_Low','value']
CapitalInputElast_Mid = ParameterData['CapitalInputElast_Mid','value']
CapitalInputElast_High = ParameterData['CapitalInputElast_High','value']

LaborInputElast_Low = ParameterData['LaborInputElast_Low','value']
LaborInputElast_Mid = ParameterData['LaborInputElast_Mid','value']
LaborInputElast_High = ParameterData['LaborInputElast_High','value']

LowEmployedWorkRatio_RM1 = ParameterData['LowEmployedWorkRatio_RM1','value']
LowEmployedWorkRatio_RM2 = ParameterData['LowEmployedWorkRatio_RM2','value']
LowEmployedWorkRatio_RM3 = ParameterData['LowEmployedWorkRatio_RM3','value']
LowEmployedWorkRatio_RM4 = ParameterData['LowEmployedWorkRatio_RM4','value']
LowEmployedWorkRatio_RF1 = ParameterData['LowEmployedWorkRatio_RF1','value']
LowEmployedWorkRatio_RF2 = ParameterData['LowEmployedWorkRatio_RF2','value']
LowEmployedWorkRatio_RF3 = ParameterData['LowEmployedWorkRatio_RF3','value']
LowEmployedWorkRatio_RF4 = ParameterData['LowEmployedWorkRatio_RF4','value']             
LowEmployedWorkRatio_PM1 = ParameterData['LowEmployedWorkRatio_PM1','value']
LowEmployedWorkRatio_PM2 = ParameterData['LowEmployedWorkRatio_PM2','value']
LowEmployedWorkRatio_PM3 = ParameterData['LowEmployedWorkRatio_PM3','value']
LowEmployedWorkRatio_PM4 = ParameterData['LowEmployedWorkRatio_PM4','value']
LowEmployedWorkRatio_PF1 = ParameterData['LowEmployedWorkRatio_PF1','value']
LowEmployedWorkRatio_PF2 = ParameterData['LowEmployedWorkRatio_PF2','value']
LowEmployedWorkRatio_PF3 = ParameterData['LowEmployedWorkRatio_PF3','value']
LowEmployedWorkRatio_PF4 = ParameterData['LowEmployedWorkRatio_PF4','value']

MidEmployedWorkRatio_RM1 = ParameterData['MidEmployedWorkRatio_RM1','value']
MidEmployedWorkRatio_RM2 = ParameterData['MidEmployedWorkRatio_RM2','value']
MidEmployedWorkRatio_RM3 = ParameterData['MidEmployedWorkRatio_RM3','value']
MidEmployedWorkRatio_RM4 = ParameterData['MidEmployedWorkRatio_RM4','value']
MidEmployedWorkRatio_RF1 = ParameterData['MidEmployedWorkRatio_RF1','value']
MidEmployedWorkRatio_RF2 = ParameterData['MidEmployedWorkRatio_RF2','value']
MidEmployedWorkRatio_RF3 = ParameterData['MidEmployedWorkRatio_RF3','value']
MidEmployedWorkRatio_RF4 = ParameterData['MidEmployedWorkRatio_RF4','value']             
MidEmployedWorkRatio_PM1 = ParameterData['MidEmployedWorkRatio_PM1','value']
MidEmployedWorkRatio_PM2 = ParameterData['MidEmployedWorkRatio_PM2','value']
MidEmployedWorkRatio_PM3 = ParameterData['MidEmployedWorkRatio_PM3','value']
MidEmployedWorkRatio_PM4 = ParameterData['MidEmployedWorkRatio_PM4','value']
MidEmployedWorkRatio_PF1 = ParameterData['MidEmployedWorkRatio_PF1','value']
MidEmployedWorkRatio_PF2 = ParameterData['MidEmployedWorkRatio_PF2','value']
MidEmployedWorkRatio_PF3 = ParameterData['MidEmployedWorkRatio_PF3','value']
MidEmployedWorkRatio_PF4 = ParameterData['MidEmployedWorkRatio_PF4','value']

HighEmployedWorkRatio_RM1 = ParameterData['HighEmployedWorkRatio_RM1','value']
HighEmployedWorkRatio_RM2 = ParameterData['HighEmployedWorkRatio_RM2','value']
HighEmployedWorkRatio_RM3 = ParameterData['HighEmployedWorkRatio_RM3','value']
HighEmployedWorkRatio_RM4 = ParameterData['HighEmployedWorkRatio_RM4','value']
HighEmployedWorkRatio_RF1 = ParameterData['HighEmployedWorkRatio_RF1','value']
HighEmployedWorkRatio_RF2 = ParameterData['HighEmployedWorkRatio_RF2','value']
HighEmployedWorkRatio_RF3 = ParameterData['HighEmployedWorkRatio_RF3','value']
HighEmployedWorkRatio_RF4 = ParameterData['HighEmployedWorkRatio_RF4','value']             
HighEmployedWorkRatio_PM1 = ParameterData['HighEmployedWorkRatio_PM1','value']
HighEmployedWorkRatio_PM2 = ParameterData['HighEmployedWorkRatio_PM2','value']
HighEmployedWorkRatio_PM3 = ParameterData['HighEmployedWorkRatio_PM3','value']
HighEmployedWorkRatio_PM4 = ParameterData['HighEmployedWorkRatio_PM4','value']
HighEmployedWorkRatio_PF1 = ParameterData['HighEmployedWorkRatio_PF1','value']
HighEmployedWorkRatio_PF2 = ParameterData['HighEmployedWorkRatio_PF2','value']
HighEmployedWorkRatio_PF3 = ParameterData['HighEmployedWorkRatio_PF3','value']
HighEmployedWorkRatio_PF4 = ParameterData['HighEmployedWorkRatio_PF4','value']

LowEmployedWorkRatio_ijk = c(
	LowEmployedWorkRatio_RM1,
	LowEmployedWorkRatio_RM2,
	LowEmployedWorkRatio_RM3,
	LowEmployedWorkRatio_RM4,
	LowEmployedWorkRatio_RF1,
	LowEmployedWorkRatio_RF2,
	LowEmployedWorkRatio_RF3,
	LowEmployedWorkRatio_RF4,
	LowEmployedWorkRatio_PM1,
	LowEmployedWorkRatio_PM2,
	LowEmployedWorkRatio_PM3,
	LowEmployedWorkRatio_PM4,
	LowEmployedWorkRatio_PF1,
	LowEmployedWorkRatio_PF2,
	LowEmployedWorkRatio_PF3,
	LowEmployedWorkRatio_PF4)

MidEmployedWorkRatio_ijk = c(
	MidEmployedWorkRatio_RM1,
	MidEmployedWorkRatio_RM2,
	MidEmployedWorkRatio_RM3,
	MidEmployedWorkRatio_RM4,
	MidEmployedWorkRatio_RF1,
	MidEmployedWorkRatio_RF2,
	MidEmployedWorkRatio_RF3,
	MidEmployedWorkRatio_RF4,
	MidEmployedWorkRatio_PM1,
	MidEmployedWorkRatio_PM2,
	MidEmployedWorkRatio_PM3,
	MidEmployedWorkRatio_PM4,
	MidEmployedWorkRatio_PF1,
	MidEmployedWorkRatio_PF2,
	MidEmployedWorkRatio_PF3,
	MidEmployedWorkRatio_PF4)

HighEmployedWorkRatio_ijk = c(
	HighEmployedWorkRatio_RM1,
	HighEmployedWorkRatio_RM2,
	HighEmployedWorkRatio_RM3,
	HighEmployedWorkRatio_RM4,
	HighEmployedWorkRatio_RF1,
	HighEmployedWorkRatio_RF2,
	HighEmployedWorkRatio_RF3,
	HighEmployedWorkRatio_RF4,
	HighEmployedWorkRatio_PM1,
	HighEmployedWorkRatio_PM2,
	HighEmployedWorkRatio_PM3,
	HighEmployedWorkRatio_PM4,
	HighEmployedWorkRatio_PF1,
	HighEmployedWorkRatio_PF2,
	HighEmployedWorkRatio_PF3,
	HighEmployedWorkRatio_PF4)


InitCapital_Low = InitialData['Capital_Low','value']
InitCapital_Mid = InitialData['Capital_Mid','value']
InitCapital_High = InitialData['Capital_High','value']

InitLabor_Low = sum(
	c(	LowPop_RM1,
		LowPop_RM2,
		LowPop_RM3,
		LowPop_RM4,
		LowPop_RF1,
		LowPop_RF2,
		LowPop_RF3,
		LowPop_RF4, 
		LowPop_PM1,
		LowPop_PM2,
		LowPop_PM3,
		LowPop_PM4,
		LowPop_PF1,
		LowPop_PF2,
		LowPop_PF3,
		LowPop_PF4) * LowEmployedWorkRatio_ijk)

InitLabor_Mid = sum(
	c(	MidPop_RM1,
		MidPop_RM2,
		MidPop_RM3,
		MidPop_RM4,
		MidPop_RF1,
		MidPop_RF2,
		MidPop_RF3,
		MidPop_RF4, 
		MidPop_PM1,
		MidPop_PM2,
		MidPop_PM3,
		MidPop_PM4,
		MidPop_PF1,
		MidPop_PF2,
		MidPop_PF3,
		MidPop_PF4) * MidEmployedWorkRatio_ijk)

InitLabor_High = sum(
	c(	HighPop_RM1,
		HighPop_RM2,
		HighPop_RM3,
		HighPop_RM4,
		HighPop_RF1,
		HighPop_RF2,
		HighPop_RF3,
		HighPop_RF4, 
		HighPop_PM1,
		HighPop_PM2,
		HighPop_PM3,
		HighPop_PM4,
		HighPop_PF1,
		HighPop_PF2,
		HighPop_PF3,
		HighPop_PF4) * HighEmployedWorkRatio_ijk)

InitRenewableResources = InitialData['RenewableResources','value']
InitNonrenewableResources = InitialData['NonrenewableResources','value']

TechMult_Low = InitialData['TechMult_Low','value']
TechMult_Mid = InitialData['TechMult_Mid','value']
TechMult_High = InitialData['TechMult_High','value']

RenewableAccess_Low = ParameterData['RenewableAccess_Low','value']
RenewableAccess_Mid = ParameterData['RenewableAccess_Mid','value']
RenewableAccess_High = ParameterData['RenewableAccess_High','value']

NonrenewableAccess_Low = ParameterData['NonrenewableAccess_Low','value']
NonrenewableAccess_Mid = ParameterData['NonrenewableAccess_Mid','value']
NonrenewableAccess_High = ParameterData['NonrenewableAccess_High','value']

InitRenewableResources_Low = RenewableAccess_Low * InitRenewableResources
InitRenewableResources_Mid = RenewableAccess_Mid * InitRenewableResources
InitRenewableResources_High = RenewableAccess_High * InitRenewableResources

InitNonrenewableResources_Low = NonrenewableAccess_Low * InitNonrenewableResources
InitNonrenewableResources_Mid = NonrenewableAccess_Mid * InitNonrenewableResources
InitNonrenewableResources_High = NonrenewableAccess_High * InitNonrenewableResources

InitEconOutput_Low = TechMult_Low*InitLabor_Low^LaborInputElast_Low * 
				InitCapital_Low^CapitalInputElast_Low *
			 	InitNonrenewableResources_Low^NonrenewableInputElast_Low * 
			 	InitRenewableResources_Low^RenewableInputElast_Low 

InitEconOutput_Mid = TechMult_Mid*InitLabor_Mid^LaborInputElast_Mid * 
				InitCapital_Mid^CapitalInputElast_Mid *
			 	InitNonrenewableResources_Mid^NonrenewableInputElast_Mid * 
			 	InitRenewableResources_Mid^RenewableInputElast_Mid 

InitEconOutput_High = TechMult_High*InitLabor_High^LaborInputElast_High * 
				InitCapital_High^CapitalInputElast_High *
			 	InitNonrenewableResources_High^NonrenewableInputElast_High * 
			 	InitRenewableResources_High^RenewableInputElast_High 

InitCO2Concentration = InitialData['CO2Conc','value']
InitTemp = InitialData['GlobalTemp','value']

################# STOCK DATA #################

InitValue = c( 
  # Economic Stocks (Regional)
		Capital_Low = InitCapital_Low,
		Capital_Mid = InitCapital_Mid,
		Capital_High = InitCapital_High,
		EconOutput_Low = InitEconOutput_Low,
		EconOutput_Mid = InitEconOutput_Mid,
		EconOutput_High = InitEconOutput_High,

  # Resource Stocks (Global)		
		RenewableResources = InitRenewableResources,
		NonrenewableResources = InitNonrenewableResources,

  # Climate Stocks (Global)
		CO2Conc = InitCO2Concentration,
		GlobalTemp = InitTemp,

  # Food Stocks (Global)
		Fisheries = InitialData['Fisheries','value'],  
		Fishstock = InitialData['Fishstock','value'],   
		Livestock = InitialData['Livestock','value'],        
		Crops = InitialData['Crops','value'],
		FoodDemandPC_Low = InitialData['FoodDemandPC_Low','value'],
		FoodDemandPC_Mid = InitialData['FoodDemandPC_Mid','value'],
		FoodDemandPC_High = InitialData['FoodDemandPC_High','value'],
		GrazeLand = InitialData['GrazeLand','value'],
		CropLand = InitialData['CropLand','value'],

  # Health and Education Stocks (Regions)
		EducationServices_Low = InitialData['EducationServices_Low','value'],
		EducationServices_Mid = InitialData['EducationServices_Mid','value'],
		EducationServices_High = InitialData['EducationServices_High','value'],
		HealthServices_Low = InitialData['HealthServices_Low','value'],
		HealthServices_Mid = InitialData['HealthServices_Mid','value'],
        HealthServices_High = InitialData['HealthServices_High','value'],

  # Population Stocks (Regions)        
		Low_RM1 = LowPop_RM1,
		Low_RM2 = LowPop_RM2,
		Low_RM3 = LowPop_RM3,
		Low_RM4 = LowPop_RM4,
		Low_RF1 = LowPop_RF1,
		Low_RF2 = LowPop_RF2,
		Low_RF3 = LowPop_RF3,
		Low_RF4 = LowPop_RF4,
		Low_PM1 = LowPop_PM1,
		Low_PM2 = LowPop_PM2,
		Low_PM3 = LowPop_PM3,
		Low_PM4 = LowPop_PM4,
		Low_PF1 = LowPop_PF1,
		Low_PF2 = LowPop_PF2,
		Low_PF3 = LowPop_PF3,
		Low_PF4 = LowPop_PF4,

		Mid_RM1 = MidPop_RM1,
		Mid_RM2 = MidPop_RM2,
		Mid_RM3 = MidPop_RM3,
		Mid_RM4 = MidPop_RM4,
		Mid_RF1 = MidPop_RF1,
		Mid_RF2 = MidPop_RF2,
		Mid_RF3 = MidPop_RF3,
		Mid_RF4 = MidPop_RF4,
		Mid_PM1 = MidPop_PM1,
		Mid_PM2 = MidPop_PM2,
		Mid_PM3 = MidPop_PM3,
		Mid_PM4 = MidPop_PM4,
		Mid_PF1 = MidPop_PF1,
		Mid_PF2 = MidPop_PF2,
		Mid_PF3 = MidPop_PF3,
		Mid_PF4 = MidPop_PF4,

		High_RM1 = HighPop_RM1,
		High_RM2 = HighPop_RM2,
		High_RM3 = HighPop_RM3,
		High_RM4 = HighPop_RM4,
		High_RF1 = HighPop_RF1,
		High_RF2 = HighPop_RF2,
		High_RF3 = HighPop_RF3,
		High_RF4 = HighPop_RF4,
		High_PM1 = HighPop_PM1,
		High_PM2 = HighPop_PM2,
		High_PM3 = HighPop_PM3,
		High_PM4 = HighPop_PM4,
		High_PF1 = HighPop_PF1,
		High_PF2 = HighPop_PF2,
		High_PF3 = HighPop_PF3,
		High_PF4 = HighPop_PF4,

		LowPop = InitLowTotPop,
		MidPop = InitMidTotPop,
		HighPop = InitHighTotPop,

  # Water Stocks (Global)
		Freshwater = InitialData['Freshwater','value'])


################# PARAMETER DATA #################

ParameterValue = list(
  # Population Coefficients
		MinDeath = ParameterData['MinDeath','value'],
		TNMFR = ParameterData['TNMFR','value'],
		AlphaC = ParameterData['AlphaC','value'],
		BetaEC = ParameterData['BetaEC','value'],
		BetaHC = ParameterData['BetaHC','value'],
		AlphaM = ParameterData['AlphaM','value'],
		BetaEM = ParameterData['BetaEM','value'],
		TotalFecundity = ParameterData['TotalFecundity','value'],
		Beta1 = ParameterData['Beta1','value'],
		FemaleBirthRatio = ParameterData['FemaleBirthRatio','value'],

		OmegaF_RM1 = ParameterData['OmegaF_RM1','value'],
		OmegaF_RM2 = ParameterData['OmegaF_RM2','value'],
		OmegaF_RM3 = ParameterData['OmegaF_RM3','value'],
		OmegaF_RM4 = ParameterData['OmegaF_RM4','value'],
		OmegaF_RF1 = ParameterData['OmegaF_RF1','value'],
		OmegaF_RF2 = ParameterData['OmegaF_RF2','value'],
		OmegaF_RF3 = ParameterData['OmegaF_RF3','value'],
		OmegaF_RF4 = ParameterData['OmegaF_RF4','value'],
		OmegaF_PM1 = ParameterData['OmegaF_PM1','value'],
		OmegaF_PM2 = ParameterData['OmegaF_PM2','value'],
		OmegaF_PM3 = ParameterData['OmegaF_PM3','value'],
		OmegaF_PM4 = ParameterData['OmegaF_PM4','value'],
		OmegaF_PF1 = ParameterData['OmegaF_PF1','value'],
		OmegaF_PF2 = ParameterData['OmegaF_PF2','value'],
		OmegaF_PF3 = ParameterData['OmegaF_PF3','value'],
		OmegaF_PF4 = ParameterData['OmegaF_PF4','value'],

		OmegaH_RM1 = ParameterData['OmegaH_RM1','value'],
		OmegaH_RM2 = ParameterData['OmegaH_RM2','value'],
		OmegaH_RM3 = ParameterData['OmegaH_RM3','value'],
		OmegaH_RM4 = ParameterData['OmegaH_RM4','value'],
		OmegaH_RF1 = ParameterData['OmegaH_RF1','value'],
		OmegaH_RF2 = ParameterData['OmegaH_RF2','value'],
		OmegaH_RF3 = ParameterData['OmegaH_RF3','value'],
		OmegaH_RF4 = ParameterData['OmegaH_RF4','value'],	
		OmegaH_PM1 = ParameterData['OmegaH_PM1','value'],
		OmegaH_PM2 = ParameterData['OmegaH_PM2','value'],
		OmegaH_PM3 = ParameterData['OmegaH_PM3','value'],
		OmegaH_PM4 = ParameterData['OmegaH_PM4','value'],
		OmegaH_PF1 = ParameterData['OmegaH_PF1','value'],
		OmegaH_PF2 = ParameterData['OmegaH_PF2','value'],
		OmegaH_PF3 = ParameterData['OmegaH_PF3','value'],
		OmegaH_PF4 = ParameterData['OmegaH_PF4','value'],
		NutritionReq = ParameterData['NutritionReq','value'],

  # Food Coefficients
		ThetaU = ParameterData['ThetaU','value'],
		ZetaU = ParameterData['ZetaU','value'],
		GrazeLandGrowthRate = ParameterData['GrazeLandGrowthRate','value'],
		GrazeLandLossRate = ParameterData['GrazeLandLossRate','value'],
		CropLandGrowthRate = ParameterData['CropLandGrowthRate','value'],
		CropLandLossRate = ParameterData['CropLandLossRate','value'],
		LandProdElastLivestock = ParameterData['LandProdElastLivestock','value'],
		WaterProdElastLivestock = ParameterData['WaterProdElastLivestock','value'],
		LivestockTechMult = ParameterData['LivestockTechMult','value'],
		WaterProdEastCrops = ParameterData['WaterProdEastCrops','value'],
		LandProdElastCrops = ParameterData['LandProdElastCrops','value'],
		CropsTechMult = ParameterData['CropsTechMult','value'],

		FoodNutrConv_Fish = ParameterData['FoodNutrConv_Fish','value'],
		FoodNutrConv_Livestock = ParameterData['FoodNutrConv_Livestock','value'],
		FoodNutrConv_Crops = ParameterData['FoodNutrConv_Crops','value'],
		
		FishConsFrac_Fish = ParameterData['FishConsFrac_Fish','value'],
		FishConsFrac_Livestock = ParameterData['FishConsFrac_Livestock','value'],
		FishConsFrac_Crops = ParameterData['FishConsFrac_Crops','value'],
		FishProdDelay = ParameterData['FishProdDelay','value'],
		
		LivestockProdDelay = ParameterData['LivestockProdDelay','value'],
		CropsProdDelay = ParameterData['CropsProdDelay','value'],
		FoodIncomeElasticity_Low = ParameterData['FoodIncomeElasticity_Low','value'],
		FoodIncomeElasticity_Mid = ParameterData['FoodIncomeElasticity_Mid','value'],
		FoodIncomeElasticity_High = ParameterData['FoodIncomeElasticity_High','value'],
		FoodAccess_RichLow = ParameterData['FoodAccess_RichLow','value'],
		FoodAccess_RichMid = ParameterData['FoodAccess_RichMid','value'],
		FoodAccess_RichHigh = ParameterData['FoodAccess_RichHigh','value'],
		FoodAccess_PoorLow = ParameterData['FoodAccess_PoorLow','value'],
		FoodAccess_PoorMid = ParameterData['FoodAccess_PoorMid','value'],
		FoodAccess_PoorHigh = ParameterData['FoodAccess_PoorHigh','value'],
		FishWasteFrac = ParameterData['FishWasteFrac','value'],
		LivestockWasteFrac = ParameterData['LivestockWasteFrac','value'],
		CropsWasteFrac = ParameterData['CropsWasteFrac','value'],

    # Climate Coefficients
		Lambda = ParameterData['Lambda','value'],
		RefTemp = InitTemp,
		PsiE = ParameterData['PsiE','value'],
		PsiL = ParameterData['PsiL','value'],
		Gamma = ParameterData['Gamma','value'],
		RefCO2Conc = InitCO2Concentration,
		OtherRadForce = ParameterData['OtherRadForce','value'],

    # Economy Coefficients
		RenewableInputElast_Low = RenewableInputElast_Low,
		RenewableInputElast_Mid = RenewableInputElast_Mid,
		RenewableInputElast_High = RenewableInputElast_High,
		NonrenewableInputElast_Low = NonrenewableInputElast_Low,
		NonrenewableInputElast_Mid = NonrenewableInputElast_Mid,
		NonrenewableInputElast_High = NonrenewableInputElast_High,
		CapitalInputElast_Low = CapitalInputElast_Low,
		CapitalInputElast_Mid = CapitalInputElast_Mid,
		CapitalInputElast_High = CapitalInputElast_High,
		LaborInputElast_Low = LaborInputElast_Low,
		LaborInputElast_Mid = LaborInputElast_Mid,
		LaborInputElast_High = LaborInputElast_High,
		TechMult_Low = TechMult_Low,
		TechMult_Mid = TechMult_Mid,
		TechMult_High = TechMult_High,
		RenewableAccess_Low = RenewableAccess_Low,
		RenewableAccess_Mid = RenewableAccess_Mid,
		RenewableAccess_High = RenewableAccess_High,
		NonrenewableAccess_Low = NonrenewableAccess_Low,
		NonrenewableAccess_Mid = NonrenewableAccess_Mid,
		NonrenewableAccess_High = NonrenewableAccess_High,
		LowEmployedWorkRatio_RM1 = LowEmployedWorkRatio_RM1, 
		LowEmployedWorkRatio_RM2 = LowEmployedWorkRatio_RM2, 
		LowEmployedWorkRatio_RM3 = LowEmployedWorkRatio_RM3, 
		LowEmployedWorkRatio_RM4 = LowEmployedWorkRatio_RM4, 
		LowEmployedWorkRatio_RF1 = LowEmployedWorkRatio_RF1, 
		LowEmployedWorkRatio_RF2 = LowEmployedWorkRatio_RF2, 
		LowEmployedWorkRatio_RF3 = LowEmployedWorkRatio_RF3, 
		LowEmployedWorkRatio_RF4 = LowEmployedWorkRatio_RF4,            
		LowEmployedWorkRatio_PM1 = LowEmployedWorkRatio_PM1, 
		LowEmployedWorkRatio_PM2 = LowEmployedWorkRatio_PM2, 
		LowEmployedWorkRatio_PM3 = LowEmployedWorkRatio_PM3, 
		LowEmployedWorkRatio_PM4 = LowEmployedWorkRatio_PM4, 
		LowEmployedWorkRatio_PF1 = LowEmployedWorkRatio_PF1, 
		LowEmployedWorkRatio_PF2 = LowEmployedWorkRatio_PF2, 
		LowEmployedWorkRatio_PF3 = LowEmployedWorkRatio_PF3, 
		LowEmployedWorkRatio_PF4 = LowEmployedWorkRatio_PF4, 	
		MidEmployedWorkRatio_RM1 = MidEmployedWorkRatio_RM1, 
		MidEmployedWorkRatio_RM2 = MidEmployedWorkRatio_RM2, 
		MidEmployedWorkRatio_RM3 = MidEmployedWorkRatio_RM3, 
		MidEmployedWorkRatio_RM4 = MidEmployedWorkRatio_RM4, 
		MidEmployedWorkRatio_RF1 = MidEmployedWorkRatio_RF1, 
		MidEmployedWorkRatio_RF2 = MidEmployedWorkRatio_RF2, 
		MidEmployedWorkRatio_RF3 = MidEmployedWorkRatio_RF3, 
		MidEmployedWorkRatio_RF4 = MidEmployedWorkRatio_RF4,            
		MidEmployedWorkRatio_PM1 = MidEmployedWorkRatio_PM1, 
		MidEmployedWorkRatio_PM2 = MidEmployedWorkRatio_PM2, 
		MidEmployedWorkRatio_PM3 = MidEmployedWorkRatio_PM3, 
		MidEmployedWorkRatio_PM4 = MidEmployedWorkRatio_PM4, 
		MidEmployedWorkRatio_PF1 = MidEmployedWorkRatio_PF1, 
		MidEmployedWorkRatio_PF2 = MidEmployedWorkRatio_PF2, 
		MidEmployedWorkRatio_PF3 = MidEmployedWorkRatio_PF3, 
		MidEmployedWorkRatio_PF4 = MidEmployedWorkRatio_PF4,
		HighEmployedWorkRatio_RM1 = HighEmployedWorkRatio_RM1, 
		HighEmployedWorkRatio_RM2 = HighEmployedWorkRatio_RM2, 
		HighEmployedWorkRatio_RM3 = HighEmployedWorkRatio_RM3, 
		HighEmployedWorkRatio_RM4 = HighEmployedWorkRatio_RM4, 
		HighEmployedWorkRatio_RF1 = HighEmployedWorkRatio_RF1, 
		HighEmployedWorkRatio_RF2 = HighEmployedWorkRatio_RF2, 
		HighEmployedWorkRatio_RF3 = HighEmployedWorkRatio_RF3, 
		HighEmployedWorkRatio_RF4 = HighEmployedWorkRatio_RF4,            
		HighEmployedWorkRatio_PM1 = HighEmployedWorkRatio_PM1, 
		HighEmployedWorkRatio_PM2 = HighEmployedWorkRatio_PM2, 
		HighEmployedWorkRatio_PM3 = HighEmployedWorkRatio_PM3, 
		HighEmployedWorkRatio_PM4 = HighEmployedWorkRatio_PM4, 
		HighEmployedWorkRatio_PF1 = HighEmployedWorkRatio_PF1, 
		HighEmployedWorkRatio_PF2 = HighEmployedWorkRatio_PF2, 
		HighEmployedWorkRatio_PF3 = HighEmployedWorkRatio_PF3, 
		HighEmployedWorkRatio_PF4 = HighEmployedWorkRatio_PF4,
		SavingsRate_Low = ParameterData['SavingsRate_Low','value'],
		SavingsRate_Mid = ParameterData['SavingsRate_Mid','value'],
		SavingsRate_High = ParameterData['SavingsRate_High','value'],
		DeprecRate_Low = ParameterData['DeprecRate_Low','value'],
		DeprecRate_Mid = ParameterData['DeprecRate_Mid','value'],
		DeprecRate_High = ParameterData['DeprecRate_High','value'],
		IneqMult_Low = ParameterData['IneqMult_Low','value'],
		IneqMult_Mid = ParameterData['IneqMult_ Mid','value'],
		IneqMult_High = ParameterData['IneqMult_High','value'],

	# Resource Coefficients	
		ReplRateRenewable = ParameterData['ReplRateRenewable','value'],
		RenewableConsIntensity_Low = ParameterData['RenewableConsIntensity_Low','value'],
		RenewableConsIntensity_Mid = ParameterData['RenewableConsIntensity_Mid','value'],
		RenewableConsIntensity_High = ParameterData['RenewableConsIntensity_High','value'],
		NonrenewableConsIntensity_Low = ParameterData['NonrenewableConsIntensity_Low','value'],		
		NonrenewableConsIntensity_Mid = ParameterData['NonrenewableConsIntensity_Mid','value'],
		NonrenewableConsIntensity_High = ParameterData['NonrenewableConsIntensity_High','value'],

    # Water Coefficients
		DeltaW = ParameterData['DeltaW','value'],
		WaterReplRate = ParameterData['WaterReplRate','value'],
		ZetaI = ParameterData['ZetaI','value'],
		WaterDemandPC = ParameterData['WaterDemandPC','value'],

    # Health and Education Coefficients
		EducationInvestFrac_Low = ParameterData['EducationInvestFrac_Low','value'],
		EducationInvestFrac_Mid = ParameterData['EducationInvestFrac_Mid','value'],
		EducationInvestFrac_High = ParameterData['EducationInvestFrac_High','value'],
		HealthInvestFrac_Low = ParameterData['HealthInvestFrac_Low','value'],
		HealthInvestFrac_Mid = ParameterData['HealthInvestFrac_Mid','value'],
		HealthInvestFrac_High = ParameterData['HealthInvestFrac_High','value'],
		ZetaE_Low = ParameterData['ZetaE_Low','value'],
		ZetaE_Mid = ParameterData['ZetaE_Mid','value'],
		ZetaE_High = ParameterData['ZetaE_High','value'],
		ZetaH_Low = ParameterData['ZetaH_Low','value'],
		ZetaH_Mid = ParameterData['ZetaH_Mid','value'],
		ZetaH_High = ParameterData['ZetaH_High','value'],
		LambdaE_Low = ParameterData['LambdaE_Low','value'],
		LambdaE_Mid = ParameterData['LambdaE_Mid','value'],
		LambdaE_High = ParameterData['LambdaE_High','value'],
		LambdaH_Low = ParameterData['LambdaH_Low','value'],
		LambdaH_Mid = ParameterData['LambdaH_Mid','value'],
		LambdaH_High = ParameterData['LambdaH_High','value'],
		ChiEF_RichLow = ParameterData['ChiEF_RichLow','value'],
		ChiEF_RichMid = ParameterData['ChiEF_RichMid','value'],
		ChiEF_RichHigh = ParameterData['ChiEF_RichHigh','value'], 
		ChiEF_PoorLow = ParameterData['ChiEF_PoorLow','value'],
		ChiEF_PoorMid = ParameterData['ChiEF_PoorMid','value'],
		ChiEF_PoorHigh = ParameterData['ChiEF_PoorHigh','value'],
		ChiHF_RichLow = ParameterData['ChiHF_RichLow','value'],
		ChiHF_RichMid = ParameterData['ChiHF_RichMid','value'],
		ChiHF_RichHigh = ParameterData['ChiHF_RichHigh','value'], 
		ChiHF_PoorLow = ParameterData['ChiHF_PoorLow','value'],
		ChiHF_PoorMid = ParameterData['ChiHF_PoorMid','value'],
		ChiHF_PoorHigh = ParameterData['ChiHF_PoorHigh','value'],
		ChiHA_RichLow = ParameterData['ChiHA_RichLow','value'],
		ChiHA_RichMid = ParameterData['ChiHA_RichMid','value'],
		ChiHA_RichHigh = ParameterData['ChiHA_RichHigh','value'], 
		ChiHA_PoorLow = ParameterData['ChiHA_PoorLow','value'],
		ChiHA_PoorMid = ParameterData['ChiHA_PoorMid','value'],
		ChiHA_PoorHigh = ParameterData['ChiHA_PoorHigh','value'])

setwd(orgdir)

########################                       ########################
########################   WORLD SYSTEM MODEL  ########################
########################                       ########################


WorldMod = function(t0, tf, delta_t, delayyearlength, init, parms) {
	# write.csv(unlist(parms),file='CurrentParms.csv')	
	with(as.list(c(parms)), {	
		tspan = seq(from=t0, to=tf, by=delta_t)
		aux_names = c(
			'EconOutput_Low',
			'EconOutput_Mid',
			'EconOutput_High',
			'logEconOutput_Low',
			'logEconOutput_Mid',
			'logEconOutput_High',
			'Inequality_Low',
			'Inequality_Mid',
			'Inequality_High',
			'LowPop',
			'MidPop',
			'HighPop',
			'FemaleHealthAccess_Low',
			'FemaleHealthAccess_Mid',
			'FemaleHealthAccess_High',
			'GeneralHealthAccess_RichLow',
			'GeneralHealthAccess_RichMid',
			'GeneralHealthAccess_RichHigh',
			'GeneralHealthAccess_PoorLow',
			'GeneralHealthAccess_PoorMid',
			'GeneralHealthAccess_PoorHigh',
			'GFR_Low',
			'GFR_Mid',
			'GFR_High',
			'MortRate_LowRM1',
			'MortRate_LowRM2',
			'MortRate_LowRM3',
			'MortRate_LowRM4',
			'MortRate_LowRF1',
			'MortRate_LowRF2',
			'MortRate_LowRF3',
			'MortRate_LowRF4',
			'MortRate_LowPM1',
			'MortRate_LowPM2',
			'MortRate_LowPM3',
			'MortRate_LowPM4',
			'MortRate_LowPF1',
			'MortRate_LowPF2',
			'MortRate_LowPF3',
			'MortRate_LowPF4',
			'MortRate_MidRM1',
			'MortRate_MidRM2',
			'MortRate_MidRM3',
			'MortRate_MidRM4',
			'MortRate_MidRF1',
			'MortRate_MidRF2',
			'MortRate_MidRF3',
			'MortRate_MidRF4',
			'MortRate_MidPM1',
			'MortRate_MidPM2',
			'MortRate_MidPM3',
			'MortRate_MidPM4',
			'MortRate_MidPF1',
			'MortRate_MidPF2',
			'MortRate_MidPF3',
			'MortRate_MidPF4',
			'MortRate_HighRM1',
			'MortRate_HighRM2',
			'MortRate_HighRM3',
			'MortRate_HighRM4',
			'MortRate_HighRF1',
			'MortRate_HighRF2',
			'MortRate_HighRF3',
			'MortRate_HighRF4',
			'MortRate_HighPM1',
			'MortRate_HighPM2',
			'MortRate_HighPM3',
			'MortRate_HighPM4',
			'MortRate_HighPF1',
			'MortRate_HighPF2',
			'MortRate_HighPF3',
			'MortRate_HighPF4',
			'TempAnamoly',
			'CO2EmissionPC_Low',
			'CO2EmissionPC_Mid',
			'CO2EmissionPC_High',
			'FishProduction',
			'LivestockProduction',
			'CropProduction',
			'FishWaste',
			'LivestockWaste',
			'CropWaste',
			'FishConsumption',
			'LivestockConsumption',
			'CropConsumption'
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
  		# Assemble Lists and Vectors
			RegPop_ijr = list( 
				Low = c(
					M1 = as.numeric(stocks['Low_M1']),
					M2 = as.numeric(stocks['Low_M2']),
					M3 = as.numeric(stocks['Low_M3']),
					M4 = as.numeric(stocks['Low_M4']),
					F1 = as.numeric(stocks['Low_F1']),
					F2 = as.numeric(stocks['Low_F2']),
					F3 = as.numeric(stocks['Low_F3']),
					F4 = as.numeric(stocks['Low_F4'])),
				Mid = c(
					M1 = as.numeric(stocks['Mid_M1']),
					M2 = as.numeric(stocks['Mid_M2']),
					M3 = as.numeric(stocks['Mid_M3']),
					M4 = as.numeric(stocks['Mid_M4']),
					F1 = as.numeric(stocks['Mid_F1']),
					F2 = as.numeric(stocks['Mid_F2']),
					F3 = as.numeric(stocks['Mid_F3']),
					F4 = as.numeric(stocks['Mid_F4'])),
				High = c(
					M1 = as.numeric(stocks['High_M1']),
					M2 = as.numeric(stocks['High_M2']),
					M3 = as.numeric(stocks['High_M3']),
					M4 = as.numeric(stocks['High_M4']),
					F1 = as.numeric(stocks['High_F1']),
					F2 = as.numeric(stocks['High_F2']),
					F3 = as.numeric(stocks['High_F3']),
					F4 = as.numeric(stocks['High_F4']))
			)

			EmployedWorkRatio_ijr = list(
				Low = c(
					M1 = LowEmployedWorkRatio_M1,
					M2 = LowEmployedWorkRatio_M2,
					M3 = LowEmployedWorkRatio_M3,
					M4 = LowEmployedWorkRatio_M4,
					F1 = LowEmployedWorkRatio_F1,
					F2 = LowEmployedWorkRatio_F2,
					F3 = LowEmployedWorkRatio_F3,
					F4 = LowEmployedWorkRatio_F4),
				Mid = c(
					M1 = MidEmployedWorkRatio_M1,
					M2 = MidEmployedWorkRatio_M2,
					M3 = MidEmployedWorkRatio_M3,
					M4 = MidEmployedWorkRatio_M4,
					F1 = MidEmployedWorkRatio_F1,
					F2 = MidEmployedWorkRatio_F2,
					F3 = MidEmployedWorkRatio_F3,
					F4 = MidEmployedWorkRatio_F4),
				High = c(
					M1 = HighEmployedWorkRatio_M1,
					M2 = HighEmployedWorkRatio_M2,
					M3 = HighEmployedWorkRatio_M3,
					M4 = HighEmployedWorkRatio_M4,
					F1 = HighEmployedWorkRatio_F1,
					F2 = HighEmployedWorkRatio_F2,
					F3 = HighEmployedWorkRatio_F3,
					F4 = HighEmployedWorkRatio_F4)
			)
			ChiEF1_r = c(
				Low = ChiEF1_Low,
				Mid = ChiEF1_Mid,
				High = ChiEF1_High)
			ChiEF2_r = c(
				Low = ChiEF2_Low,
				Mid = ChiEF2_Mid,
				High = ChiEF2_High)
			ChiHF1_r = c(
				Low = ChiHF1_Low,
				Mid = ChiHF1_Mid,
				High = ChiHF1_High)
			ChiHF2_r = c(
				Low = ChiHF2_Low,
				Mid = ChiHF2_Mid,
				High = ChiHF2_High)
			ChiHA1_kr = list(
				Low = c(Rich = ChiHA1_RichLow, Poor = ChiHA1_PoorLow),
				Mid = c(Rich = ChiHA1_RichMid, Poor = ChiHA1_PoorMid),
				High = c(Rich = ChiHA1_RichHigh, Poor = ChiHA1_PoorHigh))
			ChiHA2_kr = list(
				Low = c(Rich = ChiHA2_RichLow, Poor = ChiHA2_PoorLow),
				Mid = c(Rich = ChiHA2_RichMid, Poor = ChiHA2_PoorMid),
				High = c(Rich = ChiHA2_RichHigh, Poor = ChiHA2_PoorHigh))
			ChiHA3_kr = list(
				Low = c(Rich = ChiHA3_RichLow, Poor = ChiHA3_PoorLow),
				Mid = c(Rich = ChiHA3_RichMid, Poor = ChiHA3_PoorMid),
				High = c(Rich = ChiHA3_RichHigh, Poor = ChiHA3_PoorHigh))
			FoodStock_l = c(
				Fishstock = as.numeric(stocks['Fishstock']),
				Livestock = as.numeric(stocks['Livestock']),
				Crops = as.numeric(stocks['Crops']))
			FoodDemandPC_r = c( 
				Low = as.numeric(stocks['FoodDemandPC_Low']),
				Mid = as.numeric(stocks['FoodDemandPC_Mid']),
				High = as.numeric(stocks['FoodDemandPC_High']))
			RegPop_r = c(
				Low = sum(RegPop_ijr[['Low']]),
				Mid = sum(RegPop_ijr[['Mid']]),
				High = sum(RegPop_ijr[['High']]))

	# Combine Submodels

			# Regional Economies
			EconOut_Low    	= Economy(
								stocks['CoalReserves'],
								stocks['OilReserves'],
								stocks['GasReserves'],
								RegPop_r['Low'],
								stocks['Capital_Low'],
								CoalAccess_Low,
								OilAccess_Low,
								GasAccess_Low,
								stocks['TechMult_Low'],
								TechGrowth_Low,
								LaborInputElast_Low,
								CapitalInputElast_Low,
								CoalInputElast_Low,
								OilInputElast_Low,
								GasInputElast_Low,
								SavingsRate_Low,
								DeprecRate_Low,
								EmployedWorkRatio_ijr[['Low']],
								RegPop_ijr[['Low']],
								IneqMult_Low,
								IneqInt_Low,
								parms)  

			EconOut_Mid    	= Economy(
								stocks['CoalReserves'],
								stocks['OilReserves'],
								stocks['GasReserves'],
								RegPop_r['Mid'],
								stocks['Capital_Mid'],
								CoalAccess_Mid,
								OilAccess_Mid,
								GasAccess_Mid,
								stocks['TechMult_Mid'],
								TechGrowth_Mid,								
								LaborInputElast_Mid,
								CapitalInputElast_Mid,
								CoalInputElast_Mid,
								OilInputElast_Mid,
								GasInputElast_Mid,
								SavingsRate_Mid,
								DeprecRate_Mid,
								EmployedWorkRatio_ijr[['Mid']],
								RegPop_ijr[['Mid']],
								IneqMult_Mid,
								IneqInt_Mid,
								parms)
			
			EconOut_High   	= Economy(
								stocks['CoalReserves'],
								stocks['OilReserves'],
								stocks['GasReserves'],
								RegPop_r['High'],
								stocks['Capital_High'],
								CoalAccess_High,
								OilAccess_High,
								GasAccess_High,
								stocks['TechMult_High'],
								TechGrowth_High,								
								LaborInputElast_High,
								CapitalInputElast_High,
								CoalInputElast_High,
								OilInputElast_High,
								GasInputElast_High,
								SavingsRate_High,
								DeprecRate_High,
								EmployedWorkRatio_ijr[['High']],
								RegPop_ijr[['High']],
								IneqMult_High,
								IneqInt_High,
								parms)  

			EconOutput_r = 	 c( Low = EconOut_Low[['EconOutput']],
								Mid = EconOut_Mid[['EconOutput']],
								High = EconOut_High[['EconOutput']])

			EconOutputPC_r = c( Low = EconOut_Low[['EconOutputPC']], 
								Mid = EconOut_Mid[['EconOutputPC']], 
								High = EconOut_High[['EconOutputPC']])
		# Extract Delayed Values
			if (i < (1 + delayyearlength / delta_t)) {
				PrevFoodDemandPC_Low = stocks['FoodDemandPC_Low'] 
				PrevFoodDemandPC_Mid = stocks['FoodDemandPC_Mid']
				PrevFoodDemandPC_High = stocks['FoodDemandPC_High']
				PrevEconOutput_Low = EconOut_Low[['EconOutput']] / (1 + InitEconOutputGrowth_Low) 
				PrevEconOutput_Mid = EconOut_Mid[['EconOutput']] / (1 + InitEconOutputGrowth_Mid)
				PrevEconOutput_High = EconOut_High[['EconOutput']] / (1 + InitEconOutputGrowth_High)
				PrevEconOutputPC_Low =  PrevEconOutput_Low / (RegPop_r['Low'] * 1000)
				PrevEconOutputPC_Mid =  PrevEconOutput_Mid / (RegPop_r['Mid'] * 1000)
				PrevEconOutputPC_High = PrevEconOutput_High / (RegPop_r['High'] * 1000)
			}
			if (i >= (1 + delayyearlength / delta_t)) { 
				PrevFoodDemandPC_Low = StockData[i - delayyearlength / delta_t,"FoodDemandPC_Low"]
				PrevFoodDemandPC_Mid = StockData[i - delayyearlength / delta_t, "FoodDemandPC_Mid"]
				PrevFoodDemandPC_High = StockData[i - delayyearlength / delta_t, "FoodDemandPC_High"]
				PrevEconOutput_Low = AuxData[i - delayyearlength / delta_t, "EconOutput_Low"]
				PrevEconOutput_Mid = AuxData[i - delayyearlength / delta_t, "EconOutput_Mid"]
				PrevEconOutput_High = AuxData[i - delayyearlength / delta_t, "EconOutput_High"]
				PrevEconOutputPC_Low =  PrevEconOutput_Low / 
									(AuxData[i - delayyearlength / delta_t, "LowPop"]*1000)
				PrevEconOutputPC_Mid =  PrevEconOutput_Mid / 
									(AuxData[i - delayyearlength / delta_t, "MidPop"]*1000)
				PrevEconOutputPC_High =  PrevEconOutput_High / 
									(AuxData[i - delayyearlength / delta_t, "HighPop"]*1000)
			} 
			PrevFoodDemandPC_r = c( 	
				Low =  PrevFoodDemandPC_Low, 
				Mid =  PrevFoodDemandPC_Mid, 
				High = PrevFoodDemandPC_High)
			PrevEconOutputPC_r = c(		
				Low = 	PrevEconOutputPC_Low, 
				Mid = 	PrevEconOutputPC_Mid, 
				High =	PrevEconOutputPC_High)
			PrevEconOutput_r = c(		
				Low = 	PrevEconOutput_Low, 
				Mid = 	PrevEconOutput_Mid, 
				High =	PrevEconOutput_High)		

			ChangeEconOutput_r = EconOutput_r - PrevEconOutput_r
			names(PrevEconOutput_r) = c('Low','Mid','High')
			names(ChangeEconOutput_r) = c('Low','Mid','High')

			# Global Resources
			ResourceOut  	= Resource(
								stocks['CoalReserves'],
								stocks['OilReserves'],
								stocks['GasReserves'],
								EconOutput_r,
								parms)
			
			# Global Climate
			ClimateOut    	= Climate(
								stocks['CO2Conc'],
								EconOutputPC_r,
								RegPop_r,
								parms)

			# Global Food System 
			FoodOut       	= Food(
								FoodStock_l,
								stocks['Fisheries'],
								FoodDemandPC_r,
								stocks['GrazeLand'],
								stocks['CropLand'],
								stocks['GlobalTemp'],
								RegPop_r,
								EconOutputPC_r,
								PrevEconOutputPC_r,
								PrevFoodDemandPC_r,
								stocks['Freshwater'],
								parms)

			# Regional Health and Education System
			HealthEduOut_Low = HealthEducation(
								stocks['HealthServices_Low'],
								stocks['EducationServices_Low'],
								ChangeEconOutput_r['Low'],
								EconOut_Low[['EconOutput']],
								RegPop_r['Low'],
								ZetaE_Low,
								ZetaH_Low,
								LambdaE_Low,
								LambdaH_Low,
								ChiEF1_r[['Low']],
								ChiHF1_r[['Low']],
								ChiHA1_kr[['Low']],
								ChiEF2_r[['Low']],
								ChiHF2_r[['Low']],
								ChiHA2_kr[['Low']],
								ChiHA3_kr[['Low']],
								EconOut_Low[['Inequality']],
								parms)
			HealthEduOut_Mid = HealthEducation(
								stocks['HealthServices_Mid'],
								stocks['EducationServices_Mid'],
								ChangeEconOutput_r['Mid'],
								EconOut_Mid[['EconOutput']],
								RegPop_r['Mid'],
								ZetaE_Mid,
								ZetaH_Mid,
								LambdaE_Mid,
								LambdaH_Mid,
								ChiEF1_r[['Mid']],
								ChiHF1_r[['Mid']],
								ChiHA1_kr[['Mid']],
								ChiEF2_r[['Mid']],
								ChiHF2_r[['Mid']],
								ChiHA2_kr[['Mid']],
								ChiHA3_kr[['Mid']],
								EconOut_Mid[['Inequality']],
								parms)
			HealthEduOut_High = HealthEducation(
								stocks['HealthServices_High'],
								stocks['EducationServices_High'],
								ChangeEconOutput_r['High'],
								EconOut_High[['EconOutput']],
								RegPop_r['High'],
				 				ZetaE_High,
								ZetaH_High,
								LambdaE_High,
								LambdaH_High,
								ChiEF1_r[['High']],
								ChiHF1_r[['High']],
								ChiHA1_kr[['High']],
								ChiEF2_r[['High']],
								ChiHF2_r[['High']],
								ChiHA2_kr[['High']],
								ChiHA3_kr[['High']],
								EconOut_Mid[['Inequality']],
								parms)

			# Regional Population System 
			PopOut_Low      = Population(
								RegPop_ijr[['Low']],
								HealthEduOut_Low[['FemaleEduAttain']],
								HealthEduOut_Low[['FemaleHealthAccess']],
								HealthEduOut_Low[['GeneralHealthAccess_k']],
								FoodOut[['NutritionConsPC_kr']][,'Low'],
								parms)

			PopOut_Mid      = Population(
								RegPop_ijr[['Mid']],
								HealthEduOut_Mid[['FemaleEduAttain']],
								HealthEduOut_Mid[['FemaleHealthAccess']],
								HealthEduOut_Mid[['GeneralHealthAccess_k']],
								FoodOut[['NutritionConsPC_kr']][,'Mid'],
								parms)

			PopOut_High      = Population(
								RegPop_ijr[['High']],
								HealthEduOut_High[['FemaleEduAttain']],
								HealthEduOut_High[['FemaleHealthAccess']],
								HealthEduOut_High[['GeneralHealthAccess_k']],
								FoodOut[['NutritionConsPC_kr']][,'High'],
								parms)

			# Global Water Supply

			WaterOut      	= Water(stocks['Freshwater'],ClimateOut[['TempAnamoly']],EconOutput_r,
								FoodOut[['AgriWaterDemand']],RegPop_r,parms)

			################ STORE OUTPUT

			# AUXILIARY VARIABLES
			aux = c(
				EconOut_Low[['EconOutput']],
				EconOut_Mid[['EconOutput']],
				EconOut_High[['EconOutput']],
				EconOut_Low[['logEconOutput']],
				EconOut_Mid[['logEconOutput']],
				EconOut_High[['logEconOutput']],
				EconOut_Low[['Inequality']],
				EconOut_Mid[['Inequality']],
				EconOut_High[['Inequality']],
				RegPop_r['Low'],
				RegPop_r['Mid'],
				RegPop_r['High'],
				HealthEduOut_Low[['FemaleHealthAccess']],
				HealthEduOut_Mid[['FemaleHealthAccess']],
				HealthEduOut_High[['FemaleHealthAccess']],
				HealthEduOut_Low[['GeneralHealthAccess_k']]['Rich'],
				HealthEduOut_Mid[['GeneralHealthAccess_k']]['Rich'],
				HealthEduOut_High[['GeneralHealthAccess_k']]['Rich'],
				HealthEduOut_Low[['GeneralHealthAccess_k']]['Poor'],
				HealthEduOut_Mid[['GeneralHealthAccess_k']]['Poor'],
				HealthEduOut_High[['GeneralHealthAccess_k']]['Poor'],
				PopOut_Low[['GFR']],
				PopOut_Mid[['GFR']],
				PopOut_High[['GFR']],
				PopOut_Low[['MortRate_ijk']],
				PopOut_Mid[['MortRate_ijk']],
				PopOut_High[['MortRate_ijk']],
				ClimateOut[['TempAnamoly']],
				ClimateOut[['CO2EmissionPC']],
				FoodOut[['FoodProd_l']],
				FoodOut[['FoodWaste_l']],
				FoodOut[['FoodCons_l']]
			)
			AuxData[i,] = aux

			# STOCK VARIABLES
			dstocks = c(
				# Economic Stocks (Regional)
				EconOut_Low[["dCapital"]],
				EconOut_Mid[["dCapital"]],
				EconOut_High[["dCapital"]],
				EconOut_Low[["dTechMult"]],
				EconOut_Mid[["dTechMult"]],
				EconOut_High[["dTechMult"]],
				
				# Resource Stocks (Global)		
				ResourceOut[["dCoalReserves"]],
				ResourceOut[["dOilReserves"]],
				ResourceOut[["dGasReserves"]],

				# Climate Stocks (Global)
				ClimateOut[["dCO2Conc"]],

				# Food Stocks (Global)
				FoodOut[["dFisheries"]], 
				FoodOut[["dFoodStock_l"]],
				FoodOut[["dFoodDemandPC_r"]],
				FoodOut[["dGrazeLand"]], 
				FoodOut[["dCropLand"]],

				# Health and Education Stocks (Regions)
				HealthEduOut_Low[["dEducationServices"]],
				HealthEduOut_Mid[["dEducationServices"]],
				HealthEduOut_High[["dEducationServices"]],
				HealthEduOut_Low[["dHealthServices"]],
				HealthEduOut_Mid[["dHealthServices"]],
				HealthEduOut_High[["dHealthServices"]],

				# Population Stocks (Regions)        
				PopOut_Low[["dPop_ij"]],
				PopOut_Mid[["dPop_ij"]],
				PopOut_High[["dPop_ij"]],

				# Water Stocks (Global)
				WaterOut[["dFreshwater"]]
			) 
			stocks = stocks + dstocks * delta_t
			stocks = pmax(stocks,1e-16)
			StockData[i+1,] = stocks
		}
	Output = cbind(tspan, StockData[-(length(tspan)+1),],AuxData)
	colnames(Output)[1] = 'time'
	return(Output)
	})
}

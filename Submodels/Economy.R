########################                       ########################
########################  ECONOMY SUBSYSTEM    ########################
########################                       ########################

Economy = function(
	CoalReserves,
	OilReserves,
	GasReserves,
	TotalPop,
	Capital,
	CoalAccess,
	OilAccess,
	GasAccess,
	TechMult,
	TechGrowth,
	LaborInputElast,
	CapitalInputElast,
	CoalInputElast,
	OilInputElast,
	GasInputElast,
	SavingsRate,
	DeprecRate,
	EmployedWorkRatio_ij,
	Pop_ij,
	IneqMult,
	IneqInt,
	parms) 
{
	with(as.list(c(parms)),{

        # Auxiliary Variables
        LocalCoalReserves = CoalAccess * CoalReserves
        LocalOilReserves = OilAccess * OilReserves 
        LocalGasReserves = GasAccess * GasReserves
		EmployedLabor = sum(Pop_ij * EmployedWorkRatio_ij)
		EconOutput 	= TechMult * (EmployedLabor ^ LaborInputElast) * 
						(Capital ^ CapitalInputElast) * 
						(LocalCoalReserves ^ CoalInputElast) * 
						(LocalOilReserves ^ OilInputElast) *
						(LocalGasReserves ^ GasInputElast)
		logEconOutput = log(TechMult) + LaborInputElast * log(EmployedLabor) +
						CapitalInputElast * log(Capital) +
						CoalInputElast * log(LocalCoalReserves) +  
						OilInputElast * log(LocalOilReserves) +  
						GasInputElast * log(LocalGasReserves) 
		EconOutputPC = EconOutput / (TotalPop * 1000)
		CapitalInvest = EconOutput  * SavingsRate
		CapitalDeprec = Capital * DeprecRate 
		Inequality =    IneqInt + IneqMult * log(Capital) / logEconOutput

        # Stock and Flow Variables
		dCapital = CapitalInvest - CapitalDeprec
		dTechMult = TechMult * TechGrowth

        # Output
		list( 	dCapital = dCapital,
				dTechMult = dTechMult,
				EconOutput = EconOutput,
				logEconOutput = logEconOutput,
				EconOutputPC = EconOutputPC,
				Inequality = Inequality)
	})
}
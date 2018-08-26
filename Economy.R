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
	LaborInputElast,
	CapitalInputElast,
	CoalCapitalReturn,
	OilCapitalReturn,
	GasCapitalReturn,
	SavingsRate,
	DeprecRate,
	EmployedWorkRatio_ijk,
	Pop_ijk,
	parms) 
{
	with(as.list(c(parms)),{

        # Auxiliary Variables
        LocalCoalReserves = CoalAccess * CoalReserves
        LocalOilReserves = OilAccess * OilReserves 
        LocalGasReserves = GasAccess * GasReserves
		EmployedLabor = sum(Pop_ijk * EmployedWorkRatio_ijk)
		EconOutput 	= TechMult * (EmployedLabor ^ LaborInputElast) * 
					(Capital ^ CapitalInputElast) 
		EconOutputPC = EconOutput / (TotalPop * 1000)
		CapitalInvest = EconOutput * SavingsRate + 
			CoalCapitalReturn * LocalCoalReserves + 
			OilCapitalReturn * LocalOilReserves +
			GasCapitalReturn * LocalGasReserves
		CapitalDeprec = Capital * DeprecRate 

        # Stock and Flow Variables
		dCapital = CapitalInvest - CapitalDeprec

        # Output
		list( 	dCapital = dCapital,
				EconOutput = EconOutput,
				EconOutputPC = EconOutputPC)
	})
}
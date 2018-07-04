########################                       ########################
########################  ECONOMY SUBSYSTEM    ########################
########################                       ########################

Economy = function(
	RenewableResources,
	NonrenewableResources,
	TotalPop,
	Capital,
	RRAccess,
	NRAccess,
	TechMult,
	LaborInputElast,
	CapitalInputElast,
	RenewableCapitalReturn,
	NonrenewableCapitalReturn,
	IneqMult,
	SavingsRate,
	DeprecRate,
	EmployedWorkRatio_ijk,
	Pop_ijk,
	parms) 
{
	with(as.list(c(parms)),{

        # Auxiliary Variables
        NR = NRAccess * NonrenewableResources
        RR = RRAccess * RenewableResources
		EmployedLabor = sum(Pop_ijk * EmployedWorkRatio_ijk)
		EconOutput 	= TechMult * (EmployedLabor ^ LaborInputElast) * 
					(Capital ^ CapitalInputElast) 
		EconOutputPC = EconOutput / (TotalPop * 1000)
		CapitalInvest = EconOutput * SavingsRate 
			+ NonrenewableCapitalReturn * NR +  RenewableCapitalReturn * RR
		CapitalDeprec = Capital * DeprecRate 
		Inequality =  log(IneqMult * CapitalInvest / EconOutputPC )

        # Stock and Flow Variables
		dCapital = CapitalInvest - CapitalDeprec

        # Output
		list( 	dCapital = dCapital,
				EconOutput = EconOutput,
				EconOutputPC = EconOutputPC,
				Inequality = Inequality)
	})
	
}
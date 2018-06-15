########################                       ########################
########################  ECONOMY SUBSYSTEM    ########################
########################                       ########################

Economy = function(RenewableResources,NonrenewableResources,
	EconOutput,PrevEconOutput,TotalPop,Capital,
	RRAccess,NRAccess,TechMult,LaborInputElast,CapitalInputElast,
	RenewableCapitalReturn,NonrenewableCapitalReturn,IneqMult,SavingsRate,
	DeprecRate,EmployedWorkRatio_ijk,Pop_ijk,
	parms) 
{
	with(parms,{

        # Auxiliary Variables
        NR = NRAccess * NonrenewableResources
        RR = RRAccess * RenewableResources
		EmployedLabor = sum(Pop_ijk * EmployedWorkRatio_ijk)
		Output 	= TechMult * (EmployedLabor ^ LaborInputElast) * 
					(Capital ^ CapitalInputElast) 
		ChangeEconOutput = Output - PrevEconOutput 
		EconOutputPC = Output / (TotalPop * 1000)
		CapitalInvest = EconOutput * SavingsRate 
			+ NonrenewableCapitalReturn * NR +  RenewableCapitalReturn * RR
		CapitalDeprec = Capital * DeprecRate 
		Inequality =  IneqMult * CapitalInvest / EconOutputPC 

        # Stock and Flow Variables
		dCapital = CapitalInvest - CapitalDeprec
		dEconOutput = ChangeEconOutput

        # Output
		list( 	dCapital = dCapital,
				dEconOutput = dEconOutput,
				EconOutputPC = EconOutputPC,
				Inequality = Inequality)

	})
	
}
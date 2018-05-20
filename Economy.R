########################                       ########################
########################  ECONOMY SUBSYSTEM    ########################
########################                       ########################

print('------------- LOAD ECONOMY SUBSYSTEM  -------------------')


Economy = function(Capital,RenewableResources,NonrenewableResources,
	EconOutput,Pop_ijk,TotalPop,PrevEconOutput,
	parms) 
{
	with(parms,{
		# Input Conversion
		EmployedWorkRatio_ijk = c(
			EmployedWorkRatio_RM1,
			EmployedWorkRatio_RM2,
			EmployedWorkRatio_RM3,
			EmployedWorkRatio_RM4,
			EmployedWorkRatio_RF1,
			EmployedWorkRatio_RF2,
			EmployedWorkRatio_RF3,
			EmployedWorkRatio_RF4,
			EmployedWorkRatio_PM1,
			EmployedWorkRatio_PM2,
			EmployedWorkRatio_PM3,
			EmployedWorkRatio_PM4,
			EmployedWorkRatio_PF1,
			EmployedWorkRatio_PF2,
			EmployedWorkRatio_PF3,
			EmployedWorkRatio_PF4)

        # Auxiliary Variables
		EmployedLabor = sum(Pop_ijk * EmployedWorkRatio_ijk)
		Output 	= TechMult*(EmployedLabor^LaborInputElast) * 
					(Capital^CapitalInputElast) *
					(NonrenewableResources^NonrenewableInputElast) * 
					(RenewableResources^RenewableInputElast)
		ChangeEconOutput = Output - PrevEconOutput 
		EconOutputPC = Output / (TotalPop * 1000)
		CapitalInvest = Capital * SavingsRate
		CapitalDeprec = Capital * DeprecRate 
		RenewableRepl = RenewableResources * ReplRateRenewable
		RenewableCons = Output * RenewableConsIntensity
		NonrenewableCons = Output * NonrenewableConsIntensity
		Inequality =  IneqMult * exp( Capital / EconOutputPC )

        # Stock and Flow Variables
		dCapital = CapitalInvest - CapitalDeprec
		dRenewableResources = RenewableRepl - RenewableCons
		dNonrenewableResources = - NonrenewableCons
		dEconOutput = ChangeEconOutput

        # Output
		list( 	dCapital = dCapital,
				dRenewableResources = dRenewableResources,
				dNonrenewableResources = dNonrenewableResources,
				dEconOutput = dEconOutput,
				EconOutputPC = EconOutputPC,
				Inequality = Inequality)

	})
	
}
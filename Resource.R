########################                       ########################
########################  RESOURCE SUBSYSTEM   ########################
########################                       ########################

Resource = function(
	RenewableResources,
	NonrenewableResources,
	EconOutput_r,
	parms)
{	
	with(as.list(c(parms)),{
		# Assemble Inputs
		RenewableConsIntensity_r = c( 	
			Low = RenewableConsIntensity_Low,
			Mid = RenewableConsIntensity_Mid,
			High = RenewableConsIntensity_High)

		NonrenewableConsIntensity_r = c(
			Low = NonrenewableConsIntensity_Low,
			Mid = NonrenewableConsIntensity_Mid, 
			High = NonrenewableConsIntensity_High)

		# Auxiliary Variables
		RenewableRepl = RenewableResources * ReplRateRenewable
		RenewableCons = sum(EconOutput_r * RenewableConsIntensity_r)
		NonrenewableCons = sum(EconOutput_r * NonrenewableConsIntensity_r)

		# Stock and Flow Variables
		dRenewableResources = RenewableRepl - RenewableCons
		dNonrenewableResources = - NonrenewableCons

		# Output
		list( 	dRenewableResources = dRenewableResources,
				dNonrenewableResources = dNonrenewableResources)

	})
}
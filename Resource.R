########################                       ########################
########################  RESOURCE SUBSYSTEM   ########################
########################                       ########################

Resource = function(RenewableResources,NonrenewableResources,EconOutput,
	parms)
{	
	with(parms,{
		# Auxiliary Variables
		RenewableRepl = RenewableResources * ReplRateRenewable
		RenewableCons = EconOutput * RenewableConsIntensity
		NonrenewableCons = EconOutput * NonrenewableConsIntensity

		# Stock and Flow Variables
		dRenewableResources = RenewableRepl - RenewableCons
		dNonrenewableResources = - NonrenewableCons

		# Output
		list( 	dRenewableResources = dRenewableResources,
				dNonrenewableResources = dNonrenewableResources)

	})
}
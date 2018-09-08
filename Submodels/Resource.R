########################                       ########################
########################  RESOURCE SUBSYSTEM   ########################
########################                       ########################

Resource = function(
	CoalReserves,
	OilResources,
	GasResources,
	EconOutput_r,
	parms)
{	
	with(as.list(c(parms)),{
		# Assemble Inputs
		CoalConsIntensity_r = c( 	
			Low = CoalConsIntensity_Low,
			Mid = CoalConsIntensity_Mid,
			High = CoalConsIntensity_High)

		OilConsIntensity_r = c(
			Low = OilConsIntensity_Low,
			Mid = OilConsIntensity_Mid, 
			High = OilConsIntensity_High)

		GasConsIntensity_r = c(
			Low = GasConsIntensity_Low,
			Mid = GasConsIntensity_Mid, 
			High = GasConsIntensity_High)

		# Auxiliary Variables
		CoalCons = sum(log(EconOutput_r) * CoalConsIntensity_r)
		OilCons = sum(log(EconOutput_r) * OilConsIntensity_r)
		GasCons = sum(log(EconOutput_r) * GasConsIntensity_r)

		# Stock and Flow Variables
		dCoalReserves = - CoalCons
		dOilReserves = - OilCons
		dGasReserves = - GasCons

		# Output
		list( 	dCoalReserves = dCoalReserves,
				dOilReserves = dOilReserves,
				dGasReserves = dGasReserves)

	})
}
########################                       ########################
######################## HEALTH AND EDUCATION  ########################
########################    SUBSYSTEM          ########################


HealthEducation = function(
	HealthServices,
	EducationServices,
	dEconOutput,
	EconOutput,
	TotalPop,
	ZetaE,
	ZetaH,
	LambdaE,
	LambdaH,
	ChiEF1,
	ChiHF1,
	ChiHA1_k,
	ChiEF2,
	ChiHF2,
	ChiHA2_k,
	ChiHA3_k,
	Inequality,
	parms) 
{
	with(as.list(c(parms)), {
		# Auxiliary Variables
		DeprecEducationServices = EducationServices * ZetaE
		DeprecHealthServices = HealthServices * ZetaH
		GrowthEducationServices = dEconOutput * LambdaE
		GrowthHealthServices = dEconOutput * LambdaH
		FemaleEduAttain =  ChiEF1 + ChiEF2 * log(EducationServices)
		FemaleHealthAccess = ChiHF1 + ChiHF2 * log(HealthServices) 
		EconOutputPC = c(Rich = EconOutput * (1 - Inequality) / (TotalPop * 4 / 5),
						 Poor = EconOutput * Inequality / (TotalPop * 1 / 5))
		names(EconOutputPC) = c('Rich','Poor')
		GeneralHealthAccess_k = sapply(c('Rich','Poor'),
			function(x) {ChiHA1_k[x] +  ChiHA2_k[x] * log(HealthServices) +  
				EconOutputPC[x] * ChiHA3_k[x]})
		names(GeneralHealthAccess_k) = c('Rich','Poor')

		# Stock and Flow Variables
		dEducationServices = GrowthEducationServices - DeprecEducationServices
		dHealthServices = GrowthHealthServices - DeprecHealthServices

 		# Output
		list( 	dEducationServices = dEducationServices,
				dHealthServices = dHealthServices,
				FemaleHealthAccess = FemaleHealthAccess,
				FemaleEduAttain = FemaleEduAttain,
				GeneralHealthAccess_k = GeneralHealthAccess_k)
	})
}
########################                       ########################
######################## HEALTH AND EDUCATION  ########################
########################    SUBSYSTEM          ########################


HealthEducation = function(
	HealthServices,
	EducationServices,
	dEconOutput,
	TotalFemale_k,
	TotalPop_k,
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
		FemaleEduAttain =  ChiEF1 + EducationServices ^ ChiEF2 
		FemaleHealthAccess = ChiHF1 + HealthServices ^ ChiHF2
		GeneralHealthAccess_k = sapply(c('Rich','Poor'),
			function(x) {ChiHA1_k[x] +  HealthServices ^ ChiHA2_k[x] 
		+ Inequality ^ ChiHA3_k[x]})
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
########################                       ########################
######################## HEALTH AND EDUCATION  ########################
########################    SUBSYSTEM          ########################


HealthEducation = function(
	HealthServices,
	EducationServices,
	dEconOutput,
	TotalFemale_k,
	TotalPop_k,
	EducationInvestFrac,
	HealthInvestFrac,
	ZetaE,
	ZetaH,
	LambdaE,
	LambdaH,
	ChiEF_k,
	ChiHF_k,
	ChiHA_k,
	Inequality,
	parms) 
{
	with(parms, {
		# Auxiliary Variables
		DeprecEducationServices = EducationServices * ZetaE
		DeprecHealthServices = HealthServices * ZetaH
		GrowthEducationServices = dEconOutput * EducationInvestFrac / LambdaE
		GrowthHealthServices = dEconOutput * HealthInvestFrac / LambdaH
		FemaleEduAttain_k = 1/(1 + exp(-ChiEF_k * Inequality * EducationServices / TotalFemale_k))
		FemaleHealthAccess_k = 1/(1 + exp(-ChiHF_k * Inequality * HealthServices / TotalFemale_k)) 
		GeneralHealthAccess_k = 1/(1 + exp(-ChiHA_k * Inequality * HealthServices / TotalPop_k))

		# Stock and Flow Variables
		dEducationServices = GrowthEducationServices - DeprecEducationServices
		dHealthServices = GrowthHealthServices - DeprecHealthServices

 		# Output
		list( 	dEducationServices = dEducationServices,
				dHealthServices = dHealthServices,
				FemaleHealthAccess_k = FemaleHealthAccess_k,
				FemaleEduAttain_k = FemaleEduAttain_k,
				GeneralHealthAccess_k = GeneralHealthAccess_k)
	})
}
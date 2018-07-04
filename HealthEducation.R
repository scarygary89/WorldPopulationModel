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
	ChiEF1_k,
	ChiHF1_k,
	ChiHA1_k,
	ChiEF2_k,
	ChiHF2_k,
	ChiHA2_k,
	ChiEF3_k,
	ChiHF3_k,
	ChiHA3_k,
	Inequality,
	parms) 
{
	with(as.list(c(parms)), {
		# Auxiliary Variables
		DeprecEducationServices = EducationServices * ZetaE
		DeprecHealthServices = HealthServices * ZetaH
		GrowthEducationServices = dEconOutput * EducationInvestFrac / LambdaE
		GrowthHealthServices = dEconOutput * HealthInvestFrac / LambdaH
		FemaleEduAttain_k = 1/(1 + exp(ChiEF1_k * Inequality - ChiEF2_k * EducationServices 
			+ ChiEF3_k * TotalFemale_k))
		FemaleHealthAccess_k = 1/(1 + exp(ChiHF1_k * Inequality - ChiHF2_k * HealthServices
			+ ChiHF3_k * TotalFemale_k))
		GeneralHealthAccess_k = 1/(1 + exp(ChiHA1_k * Inequality - ChiHA2_k * HealthServices  
			+ ChiHA3_k * TotalPop_k))

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
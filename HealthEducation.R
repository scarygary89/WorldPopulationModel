########################                       ########################
######################## HEALTH AND EDUCATION  ########################
########################    SUBSYSTEM          ########################


HealthEducation = function(HealthServices,EducationServices,EconOutput,
	EducationInvestFrac, HealthInvestFrac,ZetaE,ZetaH,LambdaE,LambdaH,
	ChiEF_k,ChiHF_k,ChiHA_k,Pop_ijk,Inequality,parms) 
{
	with(parms, {

		# Auxiliary Variables
		EducationInvest = EconOutput * EducationInvestFrac
		HealthInvest = EconOutput * HealthInvestFrac
		EducationServiceCost = EducationServices * ZetaE
		HealthServiceCost = HealthServices * ZetaH
		ChangeEducationServices = (EducationInvest - EducationServiceCost) / LambdaE
		ChangeHealthServices = (HealthInvest - HealthServiceCost) / LambdaH
		TotalFemale_k = c(Rich = sum(Pop_ijk[c('RF1','RF2','RF3','RF4')]),
			Poor = sum(Pop_ijk[c('PF1','PF2','PF3','PF4')]))
		TotalPop_k = c(Rich = sum(Pop_ijk[c('RM1','RM2','RM3','RM4','RF1','RF2','RF3','RF4')]),
			Poor = sum(Pop_ijk[c('PM1','PM2','PM3','PM4','PF1','PF2','PF3','PF4')]))
		FemaleEduAttain_k = 1/(1 + exp(-ChiEF_k * Inequality * EducationServices / TotalFemale_k))
		FemaleHealthAccess_k = 1/(1 + exp(-ChiHF_k * Inequality * HealthServices / TotalFemale_k))
		GeneralHealthAccess_k = 1/(1 + exp(-ChiHA_k * Inequality * HealthServices / TotalPop_k))

		# Stock and Flow Variables
		dEducationServices = ChangeEducationServices
		dHealthServices = ChangeHealthServices

		# Output
		list( 	dEducationServices = dEducationServices,
				dHealthServices = dHealthServices,
				FemaleHealthAccess_k = FemaleHealthAccess_k,
				FemaleEduAttain_k = FemaleEduAttain_k,
				GeneralHealthAccess_k = GeneralHealthAccess_k)
	})
}
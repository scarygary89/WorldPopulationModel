########################                       ########################
######################## POUPULATION SUBSYSTEM ########################
########################                       ########################

print('-------------LOAD POPULATION SUBSYSTEM-------------------')


Population = function(Pop_ijk,TotalPop,
	FemaleEduAttain_k,FemaleHealthAccess_k,GeneralHealthAccess_k,NutritionConsPC_k,
	parms) 
{
	with(parms, {
		# Input Conversion
		OmegaH_ijk = matrix(c(		
			OmegaH_RM1,
			OmegaH_RM2,
			OmegaH_RM3,
			OmegaH_RM4,
			OmegaH_RF1,
			OmegaH_RF2,
			OmegaH_RF3,
			OmegaH_RF4,	
			OmegaH_PM1,
			OmegaH_PM2,
			OmegaH_PM3,
			OmegaH_PM4,
			OmegaH_PF1,
			OmegaH_PF2,
			OmegaH_PF3,
			OmegaH_PF4), 
			nrow = 8, ncol = 2)
		OmegaF_ijk = matrix(c(
			OmegaF_RM1,
			OmegaF_RM2,
			OmegaF_RM3,
			OmegaF_RM4,
			OmegaF_RF1,
			OmegaF_RF2,
			OmegaF_RF3,
			OmegaF_RF4,
			OmegaF_PM1,
			OmegaF_PM2,
			OmegaF_PM3,
			OmegaF_PM4,
			OmegaF_PF1,
			OmegaF_PF2,
			OmegaF_PF3,
			OmegaF_PF4), 
		nrow = 8, ncol = 2)
		GeneralHealthAccess_ijk = matrix(GeneralHealthAccess_k, 
			nrow = 8, ncol = 2, byrow = T)
		NutritionConsPC_ijk = matrix(GeneralHealthAccess_k, 
			nrow = 8, ncol = 2, byrow = T)

		# Auxiliary Variables
		MarriageIndex_k = c(Rich = min( AlphaM + BetaEM * (1 - FemaleEduAttain_k["Rich"]), 1),
							Poor = min( AlphaM + BetaEM * (1 - FemaleEduAttain_k["Poor"]), 1))
		ContraIndex_k   = c(Rich = min( AlphaC + BetaEC  * (1 - FemaleEduAttain_k["Rich"]) + 
							BetaHC * (1 - FemaleHealthAccess_k["Rich"]), 1),
							Poor = min( AlphaC + BetaEC  * (1 - FemaleEduAttain_k["Poor"]) + 
							BetaHC * (1 - FemaleHealthAccess_k["Poor"]), 1))
		TFR_k           = TNMFR * MarriageIndex_k * ContraIndex_k * TotalFecundity
		GFR_k           = Beta1 * TFR_k 	
		DeathFood_ijk   = OmegaF_ijk * NutritionConsPC_ijk / NutritionReq
		DeathHealth_ijk = OmegaH_ijk * GeneralHealthAccess_ijk  
		MortRate_ijk    = MinDeath + DeathFood_ijk + DeathHealth_ijk
		DeathMatrix_ijk  = diag(c(MortRate_ijk/1000),16)
		Mat_i           = c(1/15, 1/35, 1/15)
		AgeMatrix_ij   = matrix(c(
		 -Mat_i[1], 0,        0,        0, 0,        0,        0,        0, 
		 Mat_i[1],-Mat_i[2], 0,        0, 0,        0,        0,        0, 
		 0,        Mat_i[2],-Mat_i[3], 0, 0,        0,        0,        0, 
		 0,        0,        Mat_i[3], 0, 0,        0,        0,        0,
		 0,        0,        0,        0,-Mat_i[1], 0,        0,        0,
		 0,        0,        0,        0, Mat_i[1],-Mat_i[2], 0,        0,
		 0,        0,        0,        0, 0,        Mat_i[2],-Mat_i[3], 0,
		 0,        0,        0,        0, 0,        0,        Mat_i[3], 0),
		nrow = 8, ncol = 8, byrow = T)

		AgeMatrix_ijk   = as.matrix(bdiag(list(AgeMatrix_ij,AgeMatrix_ij)))
		TranMatrix_ijk	= AgeMatrix_ijk - DeathMatrix_ijk		
		TranMatrix_ijk[1,2] = FemaleBirthRatio * GFR_k[1]/1000
		TranMatrix_ijk[5,6] = (1 - FemaleBirthRatio) * GFR_k[1]/1000
		TranMatrix_ijk[9,10] = FemaleBirthRatio * GFR_k[2]/1000
		TranMatrix_ijk[13,14] = (1 - FemaleBirthRatio) * GFR_k[2]/1000
		Pop_ijk         = matrix(Pop_ijk,nrow = length(Pop_ijk),ncol = 1)

		# Stock and Flow Variables
		dPop_ijk        = TranMatrix_ijk %*% Pop_ijk
		dTotPop         = sum(dPop_ijk)		

		# Output
		list( 	dPop_ijk = dPop_ijk,
				dTotPop = dTotPop)
	})
}
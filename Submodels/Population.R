########################                       ########################
######################## POPULATION SUBSYSTEM  ########################
########################                       ########################

Population = function(
	Pop_ij,
	FemaleEduAttain,
	FemaleHealthAccess,
	GeneralHealthAccess_k,
	NutritionConsPC_k,
	parms) 
{
	with(as.list(c(parms)), {
		# Input Conversion
		Pop_ijk   = matrix(c((1 - PoorFrac)*Pop_ij,PoorFrac*Pop_ij),nrow = length(Pop_ij),ncol = 2)
		colnames(Pop_ijk) = c('Rich','Poor')
		rownames(Pop_ijk) = c('M1','M2','M3','M4','F1','F2','F3','F4')

		MinDeath_ijk = cbind(
			Rich = c(		
				M1 = MinDeath_RichM1,
				M2 = MinDeath_M2,
				M3 = MinDeath_M3,
				M4 = MinDeath_M4,
				F1 = MinDeath_RichF1,
				F2 = MinDeath_F2,
				F3 = MinDeath_F3,
				F4 = MinDeath_F4),	
			Poor = c(	
				M1 = MinDeath_PoorM1,
				M2 = MinDeath_M2,
				M3 = MinDeath_M3,
				M4 = MinDeath_M4,
				F1 = MinDeath_PoorF1,
				F2 = MinDeath_F2,
				F3 = MinDeath_F3,
				F4 = MinDeath_F4))
		OmegaH_ijk = cbind(
			Rich = c(		
				M1 = OmegaH_RichM1,
				M2 = OmegaH_M2,
				M3 = OmegaH_M3,
				M4 = OmegaH_M4,
				F1 = OmegaH_RichF1,
				F2 = OmegaH_F2,
				F3 = OmegaH_F3,
				F4 = OmegaH_F4),	
			Poor = c(	
				M1 = OmegaH_PoorM1,
				M2 = OmegaH_M2,
				M3 = OmegaH_M3,
				M4 = OmegaH_M4,
				F1 = OmegaH_PoorF1,
				F2 = OmegaH_F2,
				F3 = OmegaH_F3,
				F4 = OmegaH_F4))
		OmegaF_ijk = cbind(
			Rich = c(
				M1 = OmegaF_RichM1,
				M2 = OmegaF_M2,
				M3 = OmegaF_M3,
				M4 = OmegaF_M4,
				F1 = OmegaF_RichF1,
				F2 = OmegaF_F2,
				F3 = OmegaF_F3,
				F4 = OmegaF_F4),
			Poor = c(
				M1 = OmegaF_PoorM1,
				M2 = OmegaF_M2,
				M3 = OmegaF_M3,
				M4 = OmegaF_M4,
				F1 = OmegaF_PoorF1,
				F2 = OmegaF_F2,
				F3 = OmegaF_F3,
				F4 = OmegaF_F4))
		GeneralHealthAccess_ijk = matrix(GeneralHealthAccess_k, 
			nrow = 8, ncol = 2, byrow = T)
		colnames(GeneralHealthAccess_ijk) = c('Rich','Poor')
		NutritionConsPC_ijk = matrix(NutritionConsPC_k,
			nrow = 8, ncol = 2, byrow = T)
		colnames(NutritionConsPC_ijk) = c('Rich','Poor')
		# Auxiliary Variables
		GFR           = AlphaGFR  - BetaH * FemaleHealthAccess - BetaE * FemaleEduAttain

		DeathFood_ijk   = sapply(c('Rich','Poor'), 
							function(x) OmegaF_ijk[,x] * (NutritionReq -  NutritionConsPC_ijk[,x]) / 
							NutritionReq)
		colnames(DeathFood_ijk) = c('Rich','Poor')
		DeathHealth_ijk = sapply(c('Rich','Poor'), 
							function(x) OmegaH_ijk[,x] * (1 - GeneralHealthAccess_ijk[,x]))
		colnames(DeathHealth_ijk) = c('Rich','Poor')
		MortRate_ijk    = sapply(c('Rich','Poor'),
							function(x) MinDeath_ijk[,x] + DeathFood_ijk[,x] + DeathHealth_ijk[,x] )
		MatRate_ijk     = c(1/15, 1/35, 1/15, 0, 1/15, 1/35, 1/15, 0)

		# Define Flows
		MaturationMinus_ijk = sapply(c('Rich','Poor'),
							  	function(x) MatRate_ijk * Pop_ijk[,x])
		MaturationPlus_ijk = sapply(c('Rich','Poor'), 
							function(x) {
								MaturationPlus_ijk = MatRate_ijk * Pop_ijk[,x] 
								MaturationPlus_ijk = c(0,MaturationPlus_ijk[-length(MaturationPlus_ijk)])
								return(MaturationPlus_ijk)
							})
		Deaths_ijk 		= sapply(c('Rich','Poor'), 
							function(x) MortRate_ijk[,x]/1000 * Pop_ijk[,x])
		Births_ijk 		= cbind(rep(0,8),rep(0,8))
		Births_ijk[1,] 	=  GFR/1000 * (1 - FemaleBirthRatio) * Pop_ijk['F2',]
		Births_ijk[5,]	=  GFR/1000 * FemaleBirthRatio * Pop_ijk['F2',]

		# Stock and Flow Variables
		dPop_ijk = Births_ijk - Deaths_ijk + MaturationPlus_ijk - MaturationMinus_ijk
		dPop_ij = rowSums(dPop_ijk)

		# Output
		list( 	dPop_ij = dPop_ij,
				MortRate_ijk = MortRate_ijk,
				GFR = GFR)
	})
}
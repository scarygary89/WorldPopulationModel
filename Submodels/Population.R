########################                       ########################
######################## POPULATION SUBSYSTEM  ########################
########################                       ########################

Population = function(
	Pop_ij,
	FemaleEduAttain,
	FemaleHealthAccess,
	GeneralHealthAccess_k,
	NutritionConsPC_k,
	MinDeath_RichM1,
	MinDeath_PoorM1,
	MinDeath_M2,
	MinDeath_M3,
	MinDeath_M4,
	MinDeath_RichF1,
	MinDeath_PoorF1,
	MinDeath_F2,
	MinDeath_F3,
	MinDeath_F4,
	OmegaH_RichM1,
	OmegaH_PoorM1,
	OmegaH_M2,
	OmegaH_M3,
	OmegaH_M4,
	OmegaH_RichF1,
	OmegaH_PoorF1,
	OmegaH_F2,
	OmegaH_F3,
	OmegaH_F4,
	OmegaF_RichM1,
	OmegaF_PoorM1,
	OmegaF_M2,
	OmegaF_M3,
	OmegaF_M4,
	OmegaF_RichF1,
	OmegaF_PoorF1,
	OmegaF_F2,
	OmegaF_F3,
	OmegaF_F4,
	AlphaGFR,
	BetaE,
	BetaH,
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
		NutritionConsPC_ijk = matrix(NutritionConsPC_k,
			nrow = 8, ncol = 2, byrow = T)

		# Auxiliary Variables
		GFR = AlphaGFR  - BetaH * FemaleHealthAccess - BetaE * FemaleEduAttain
		GFR = pmax(GFR,0)
		DeathFood_ijk   = OmegaF_ijk * (NutritionReq -  NutritionConsPC_ijk) / 
							NutritionReq
		DeathHealth_ijk = OmegaH_ijk * (1 - GeneralHealthAccess_ijk)
		MortRate_ijk    = MinDeath_ijk + DeathFood_ijk + DeathHealth_ijk 
		MortRate_ijk 	= pmax(MortRate_ijk,0)
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
		Deaths_ijk 		= MortRate_ijk / 1000 * Pop_ijk
		Deaths_ij 		= rowSums(Deaths_ijk)
		Births_ijk 		= cbind(rep(0,8),rep(0,8))
		Births_ijk[1,] 	= GFR/1000 * (1 - FemaleBirthRatio) * Pop_ijk['F2',]
		Births_ijk[5,]	= GFR/1000 * FemaleBirthRatio * Pop_ijk['F2',]
		Births_ij 		= sum(Births_ijk)
		# Stock and Flow Variables
		dPop_ijk = Births_ijk - Deaths_ijk + MaturationPlus_ijk - MaturationMinus_ijk
		dPop_ij = rowSums(dPop_ijk)

		# Output
		list( 	dPop_ij = dPop_ij,
				MortRate_ijk = MortRate_ijk,
				GFR = GFR,
				Deaths_ij = Deaths_ij,
				Births_ij = Births_ij)
	})
}
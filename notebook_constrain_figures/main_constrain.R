
rm(list=ls())

# Load libraries + source useful functions...
library(KCC)
library(abind)
source("routines/outils.R")
source("routines/cx.R")
source("routines/generic_constrain.R")
# Set random number generator to ensure reproducibility
set.seed(13)


# Parameters of the script
############################

# Parameter nb resampling
Nres = 1000
	sample_str = c("be",paste0("nres",1:Nres))


# DATA  (input data files)
############################
message("Input data")
# List of input data:

# Input model data:
#	- Xg_fit & co
message("Load forced responses estimates")
load("Forced_responses.Rdata")
year = as.numeric(dimnames(Xg_fit)$year)
ny = length(year)
Models = dimnames(Xg_fit)$model
Nmod = length(Models)



# Input obs data: 
#	- Xog (global), an array [year, HadCRUT5_ens_member]
#	- Xof (France, annual mean), 
#	- Xof_jja (France, summer),
#	- Xof_djf (France, winter),
# All 'Xof*' are vectors: [year]
message("	Input data Xo : observed tas")
load("Observations.Rdata")	# H5_yr
year_og = as.numeric(dimnames(Xog)$year)
ny_og = length(year_og)
year_of = as.numeric(names(Xof))
ny_of = length(year_of)



# Aggregate X_fit
X_fit = abind( Xg_fit[,,scenarios,], Xf_fit[,,scenarios,], along=5,use.dnns=T)
	names(dimnames(X_fit))[5] = "spatial"
	dimnames(X_fit)$spatial = c("glo","loc")
	X_fit = aperm(X_fit,c(1,5,2,3,4))

X_fits = abind( Xg_fit[,c("all","nat"),scenarios,], Xf_fit[,c("all","nat"),scenarios,], Xfjja_fit[,,scenarios,], Xfdjf_fit[,,scenarios,], along=5,use.dnns=T)
	names(dimnames(X_fits))[5] = "spatial"
	dimnames(X_fits)$spatial = c("glo","loc-ANN","loc-JJA","loc-DJF")
	X_fits = aperm(X_fits,c(1,5,2,3,4))




## Observational constrain
############################
# Inputs: X_fit, Xo, + estimate of IV in real world
# Outputs: X (incl. X_cons and X_unsonc)
message("Constraints")
source("routines2/mvgauss_to_Xarray.R")
source("routines2/cx_da.R")
source("routines2/super_cx_da.R")

# Observations
Xog_tmp = Xog[,"median"]; names(Xog_tmp)=paste0(names(Xog[,"median"]),"_glo")
Xof_tmp = Xof; names(Xof_tmp)=paste0(names(Xof),"_loc")
Xo = c(Xog_tmp,Xof_tmp)
noxg = length(Xog_tmp)
noxf = length(Xof_tmp)

# Covariance of observations
source("define_SY.R")
## Output : SY_og, SY_of; covariance matrices of size (ny_og,ny_og) and (ny_of,ny_of), resp.
# Add obs uncertainty
Sigma_og = SY_og + var(t(Xog[,-1]))
Sigma_of = SY_of
Sigma_obs = 0 * Xo%o%Xo		# Initialize ; no glo/loc covariance
Sigma_obs[1:noxg,1:noxg] = Sigma_og
Sigma_obs[noxg+1:noxf,noxg+1:noxf] = Sigma_of
ref_CX = list(glo=1850:1900,loc=1900:1930)

# Global-only
message("GSAT only constraint")
CX_glo = ssuper_cx_da_spa(X_fit, Xog_tmp, Sigma_og, Nres, centering_CX=T, ref_CX="year_obs")

# Full
CX_full = ssuper_cx_da_spa(X_fit, Xo, Sigma_obs, Nres, centering_CX=T, ref_CX="year_obs")
CX_fullb = ssuper_cx_da_spa(X_fit, Xo+273.15, Sigma_obs, Nres, centering_CX=F)

# Loc only
message("Loc-only constraint")
CX_loc = ssuper_cx_da_spa(X_fit, Xof_tmp, Sigma_of, Nres, centering_CX=T, ref_CX="year_obs")

# Full with seasons
message("Full constraint")
Xos = Xo
	names(Xos) = str_replace(names(Xo),"_loc","_loc-ANN")
Sigma_obss = Sigma_obs
	colnames(Sigma_obss) = str_replace(colnames(Sigma_obs),"_loc","_loc-ANN")
	rownames(Sigma_obss) = str_replace(rownames(Sigma_obs),"_loc","_loc-ANN")
CX_fulls = ssuper_cx_da_spa(X_fits, Xos, Sigma_obss, Nres, centering_CX=T, ref_CX="year_obs")





rm(list=ls())


# Parameters of the script
############################

# Decomposition of X		(object)
#--------------------
experiments = c("histssp245", "1pctCO2", "histghg")
nexp = length(experiments)
# The effective df in x_fit (ANT component)
dfg = c( 6, 5, 6 )
	names(dfg) = experiments
dff = c( 8, 5, 6 )
	names(dff) = experiments


# Load libraries + source useful functions...
source("prelim_fr.R")
# Set random number generator to ensure reproducibility
set.seed(13)


# DATA  (input data files)
############################
message("Input data")
# List of input data:

# Input model data:
#	- Xg (global)
#	- Xf (France, annual)
#	- Xfjja (France, summer)
#	- Xfdjf (France, winter)
# All variables are arrays: [year, experiment, CMIP_model]
message("	Input data X")
load("Processed_CMIP6_data.Rdata")
year = as.numeric(dimnames(Xg)$year)
Models = dimnames(Xg)$model
ny = length(year)
Nmod = length(Models)



# Calculate EBM responses --> Enat
###################################
# Input:		ebm_params, FF, year, Nres
# Output:	Enat
message("EBM responses")
# Input data for EBM and forcings
load("FF_CMIP6_new.rda")
load("ebm_params.rda")
Enat = ebm_response(FF,ebm_params,year,Nres=1)



# ANT / NAT decompistion of histssp runs
############################################
# Inputs:	Xf, Enat, df
# Outputs:	X_fit, an array of size (ny,3,Nmod)
message("Forced ANT vs NAT decomposition in histssp runs")

# Xg_fit
message("	Global T: Xg_fit")
Xg_fit = array(NA,dim=c(ny,3,Nmod),#
				  dimnames=list(year = as.character(year),#
									 forc = c("all","nat","ghg"),#
									 model = Models))

	# General case
	Models_scen_full = Models[ apply(is.na(Xg[,"histssp245",]),2,sum)==0 ]
	Xg_fit[,c("all","nat"),Models_scen_full] = x_fit_be(Xg[,"histssp245",Models_scen_full],Enat,dfg["histssp245"],ant=F)
	# Particular cases
	# CAMS-CSM1-0 (simulation stops in 2099)
		year_cams = as.character(1850:2099)
		Xg_fit[year_cams,c("all","nat"),"CAMS-CSM1-0"] = x_fit_be( Xg[year_cams,"histssp245",c("CAMS-CSM1-0","CAMS-CSM1-0")], Enat[year_cams,],dfg["histssp245"],ant=F)[,,1]
		Xg_fit["2100",,"CAMS-CSM1-0"] = 2 * Xg_fit["2099",,"CAMS-CSM1-0"] - Xg_fit["2098",,"CAMS-CSM1-0"]



# Xf_fit
message("	France T: Xf_fit")
Xf_fit = array(NA,dim=c(ny,3,Nmod),#
				  dimnames=list(year = as.character(year),#
									 forc = c("all","nat","ghg"),#
									 model = Models))

	# General case
	Xf_fit[,c("all","nat"),Models_scen_full] = x_fit_be(Xf[,"histssp245",Models_scen_full],Enat,dff["histssp245"],ant=F)
	# Particular cases
	# CAMS-CSM1-0 (simulation stops in 2099)
		year_cams = as.character(1850:2099)
		Xf_fit[year_cams,c("all","nat"),"CAMS-CSM1-0"] = x_fit_be(Xf[year_cams,"histssp245",c("CAMS-CSM1-0","CAMS-CSM1-0")], Enat[year_cams,],dff["histssp245"],ant=F)[,,1]
		Xf_fit["2100",,"CAMS-CSM1-0"] = 2 * Xf_fit["2099",,"CAMS-CSM1-0"] - Xf_fit["2098",,"CAMS-CSM1-0"]



# Xfjja_fit & Xfdjf_fit
message("	France T, summer and winter: Xfjja_fit, Xfdjf_fit")
Xfjja_fit = array(NA,dim=c(ny,3,Nmod),#
				  dimnames=list(year = as.character(year),#
									 forc = c("all","nat","ghg"),#
									 model = Models))
Xfdjf_fit = array(NA,dim=c(ny,3,Nmod),#
				  dimnames=list(year = as.character(year),#
									 forc = c("all","nat","ghg"),#
									 model = Models))

	# General case
	Xfjja_fit[,c("all","nat"),Models_scen_full] = x_fit_be(Xfjja[,"histssp245",Models_scen_full],Enat,dff["histssp245"],ant=F)
	Xfdjf_fit[2:ny,c("all","nat"),Models_scen_full] = x_fit_be(Xfdjf[2:ny,"histssp245",Models_scen_full],Enat[2:ny,],dff["histssp245"],ant=F)
	# Particular cases
	# CAMS-CSM1-0 (simulation stops in 2099)
		year_cams = as.character(1850:2099)
		Xfjja_fit[year_cams,c("all","nat"),"CAMS-CSM1-0"] = x_fit_be(Xfjja[year_cams,"histssp245",c("CAMS-CSM1-0","CAMS-CSM1-0")], Enat[year_cams,],dff["histssp245"],ant=F)[,,1]
		Xfjja_fit["2100",,"CAMS-CSM1-0"] = 2 * Xfjja_fit["2099",,"CAMS-CSM1-0"] - Xfjja_fit["2098",,"CAMS-CSM1-0"]
		Xfdjf_fit[year_cams[2:(ny-1)],c("all","nat"),"CAMS-CSM1-0"] = x_fit_be(Xfdjf[year_cams[2:(ny-1)],"histssp245",c("CAMS-CSM1-0","CAMS-CSM1-0")], Enat[year_cams[2:(ny-1)],],dff["histssp245"],ant=F)[,,1]
		Xfdjf_fit["2100",,"CAMS-CSM1-0"] = 2 * Xfdjf_fit["2099",,"CAMS-CSM1-0"] - Xfdjf_fit["2098",,"CAMS-CSM1-0"]

# Trick to keep ny years (and not rewrite all scripts)
Xfdjf_fit["1850",,] = Xfdjf_fit["1851",,]



# Forced GHG response 
########################


# Fit histghg
#	(models with complete histghg run)
message("Forced GHG response in histghg runs (GSAT only)")
year_ghg = 1850:2020
ny_ghg = length(year_ghg)
is_histghg_full = (apply(is.na(Xg[as.character(year_ghg),"histghg",]),2,sum)==0)
Models_histghg_full = Models[is_histghg_full]
HatM_ghg_g = hatm(dfg["histghg"],year_ghg)
for (mod in Models_histghg_full) {
	Xg_fit[as.character(year_ghg),"ghg",mod] = HatM_ghg_g %*% Xg[as.character(year_ghg),"histghg",mod]
}


# Fit 1pctCO2  (used for reconstruction of missing histghg runs + in pattern scaling)
message("Forced CO2 response in 1%-CO2 runs (GSAT only)")
year_pct = 1850:1999
ny_pct = length(year_pct)
HatM_pct_g = hatm(dfg["1pctCO2"],year_pct)
HatM_pct_f = hatm(dff["1pctCO2"],year_pct)
Xg_fit_pct = array(NA,dim=c(ny_pct,Nmod),dimnames=list(year=year_pct,model=Models))
Xf_fit_pct = array(NA,dim=c(ny_pct,Nmod),dimnames=list(year=year_pct,model=Models))
for (mod in Models) {
	Xg_fit_pct[as.character(year_pct),mod] = smooth.spline(year_pct,Xg[as.character(year_pct),"1pctCO2",mod],df=dfg["1pctCO2"])$y
	Xf_fit_pct[as.character(year_pct),mod] = smooth.spline(year_pct,Xf[as.character(year_pct),"1pctCO2",mod],df=dff["1pctCO2"])$y
}




# Reconstructing histghg runs for non-damip models
###################################################
# Strategy : reconstruct histghg GSAT, then assume pattern scaling. 
# Reconstruct global from 1pct fit
message("Reconstructing non-available histghg runs")
	Models_histghg = Models[ is_histghg_full ]

	Sr = abind(Xg_fit_pct[as.character(year_pct),	Models_histghg],#
				  Xg_fit[as.character(year_ghg),"ghg", Models_histghg],#
				  along=1,use.dnns=T)
	Sr_mean = apply(Sr,1,mean)
	Sr_var = var(t(Sr))

	# Reconstruction
	Models_histghg_recons = Models[!is_histghg_full]
	for (mod in Models_histghg_recons) {
		# Extraction operator
		H_tmp = abind(diag(rep(1,ny_pct)),array(0,dim=c(ny_pct,ny_ghg)),along=2)
		sigma2_tmp = var(Xg[1:140,"1pctCO2",mod] - Xg_fit_pct[1:140,mod])
		Sigma_y_tmp = HatM_pct_g %*% diag( sigma2_tmp*ones(year_pct) ) %*% t(HatM_pct_g)
		v_recons = generic_constrain(Sr_mean,Sr_var,Xg_fit_pct[as.character(year_pct),mod],Sigma_y_tmp,H_tmp)
		Xg_fit[as.character(year_ghg),"ghg",mod] = v_recons$mean[-(1:ny_pct)]
	}



# Forced GHG regional response assuming pattern scaling
#-------------------------------------------------------
#source("compare_histghg.R")
dTg_1pct = apply(Xg_fit_pct[121:140,] - Xg_fit_pct[1:20,],2,mean)
dTf_1pct = apply(Xf_fit_pct[121:140,] - Xf_fit_pct[1:20,],2,mean)
Ratio_1pct = dTf_1pct / dTg_1pct
Xf_fit[,"ghg",] = (Xg_fit[,"ghg",] - ones(year)%o%apply(Xg_fit[1:50,"all",],2,mean)) * (ones(year) %o% Ratio_1pct) + ones(year)%o%apply(Xf_fit[1:50,"all",],2,mean)



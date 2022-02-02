H_extract = function(S,Xo) {

	# Time axis
	if (is.array(S)) {
		year_S_str = dimnames(S)$year
	} else {
		year_S_str = names(S)
	}
	year_obs_str = names(Xo)

	if (!sum(grep("_all",year_S_str))) {
		year_obs_str = paste0(year_obs_str,"_all")
	}
	# Dimensions
	ny = length(year_S_str)
	ny_obs  = length(year_obs_str)

	# Obs from S
	is_obs_in_year = year_S_str %in% year_obs_str
	if (sum(is_obs_in_year)!=ny_obs) {
		message("Error in H_extract.R: observed years not available in models");
		return
	}

	H_extract = array(0,dim=c(ny_obs,ny))
	H_extract[,is_obs_in_year] = diag(rep(1,ny_obs))
	return(H_extract)
}

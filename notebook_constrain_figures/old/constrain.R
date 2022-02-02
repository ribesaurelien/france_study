
constrain = function(S_mean,Sigma_mod,Xo,Sigma_obs,Nres,centering_CX=T,ref_CX=NULL) {

	# Time axis
	year_obs = names(Xo)
	nyox  = length(year_obs)
	locs_in_obs = sub("[0-9][0-9][0-9][0-9]_?","",year_obs)
	locs = unique(locs_in_obs)
	nl = length(locs)

	# Matrix H: selecting "all", centering, extracting relevant years
	S_mean_array = as.array(S_mean)
	dimnames(S_mean_array) = list(year=names(S_mean))
	H_extract = H_extract(S_mean_array,Xo)	# a (nyox,n)-matrix

	if (centering_CX) {
		# Initialize ref_CX (if needed)
		if (is.null(ref_CX) | identical(ref_CX,"year_obs"))	{
			ref_CX = NULL
			for (iloc in locs) {
				is_iloc = ( locs_in_obs == iloc ) 
				ref_CX = c(ref_CX,year_obs[is_iloc])
			}
		} 
		locs_in_ref_CX = sub("[0-9][0-9][0-9][0-9]_?","",ref_CX)
		if (!prod(ref_CX %in% year_obs)) {
			stop("Error in constrain(): Reference period is inconsistent with obs")
		}
		if ( !identical(sort(locs),sort(unique(locs_in_ref_CX))) ) {
			stop("Error in constrain(): Missing ref_CX for some location")
		}
		Center = diag(rep(1,nyox))	# a (nyox,nyox) identity matrix
		for (iloc in locs) {
			is_iloc = ( locs_in_obs == iloc ) 
			nyox_iloc = sum(is_iloc)
			year_obs_iloc = year_obs[is_iloc]
			ref_CX_iloc = ref_CX[ locs_in_ref_CX == iloc]
			Center[is_iloc,is_iloc] = diag(rep(1,nyox_iloc)) - rep(1,nyox_iloc)%o%(year_obs_iloc %in% ref_CX_iloc)/length(ref_CX_iloc)
		}

	} else {

		Center	= diag(rep(1,nyox))
	}

	H = Center %*% H_extract			# Centering * extracting

	# Other inputs : x, SX, y, SY
	x	= S_mean
	SX = Sigma_mod
	y  = Center %*% Xo
	SY = Center %*% Sigma_obs %*% t(Center)

	# List of prior params
	prior_dist = list(mean=S_mean, var=Sigma_mod)

	# Apply constraint
	post_dist = kriging(x,SX,y,SY,H)

	# Put prior_dist and post_dist together
	dist = list(uncons = prior_dist, cons = post_dist)

	return(dist)

}

# dl2rho.R: various functions to:
#	- estimate the equivalent degrees of freedom (dof) corresponding to a given smoothing parameter in the smoothing spline algo.
#	- if a dof is specified, estimate the corresponding smoothing parameter.
  
#require(MASS)

# Geometric mean
gm_mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
}


# Function dlrho(): Calculates the smoothing parameter (rho) corresponding to a given equivalent number of degrees of freedom (dl)
dlrho = function(dl,Z,G){
	vrho0 = c(1 ,100)
	while( (vrho0[2] / vrho0[1]) > 1.0001) {
		vrho = dl_dich(vrho0,dl,Z,G)
		vrho0 = vrho
	}
	rho = gm_mean(vrho0)
	return(rho )
}


# Function dl_dich(): dichotomy search of the specified dl
dl_dich = function(v0,dl,Z,G) {
  dl1 = rhodl(v0[1],Z,G);
  dl0 = rhodl(v0[2],Z,G);
  if (dl > dl1) {
    y0 = v0[1]^2/v0[2];
    y1 = v0[1]
  }
  else if (dl < dl0){
    y0 = v0[2];
    y1 = v0[2]^2/v0[1];}
  else{
    y0 = v0[1];
    y1 = gm_mean(v0);
  }
  vrho = c(y0,y1);
  return(vrho)
}



# Calculates the equivalent degrees of freedom for a given value of the smoothing parameters
rhodl = function(rho,Z,G){
	H = t(Z)%*%Z + rho * G
   dl = sum(diag(Z%*%solve(H)%*%t(Z)))
	return(dl)
}


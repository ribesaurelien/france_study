# Function smooth.spline.per: performs periodic spline smoothing
#	Equivalent to smooth.spline(y,df), but for periodic splines. NB: outputs are organized differently.

# Preliminary routines to source
source("baseSplinePer.R")
source("dl2rho.R")

smooth.spline.per = function(y,df) {
	
	if (is.na(df)) { message("Error in smooth.spline.per: unknown df"); return }

	n = length(y)
	l = baseSplinePer(n)
	Zp = l$Z
	Gp = l$G
	rho = dlrho(dl = df,Zp,Gp)
	
	## H, Gamma for f (see notation in Rigal et al., 2018):
	Hp = t(Zp)%*%Zp + rho * Gp
	Hp1 = solve(Hp)
	Gamma = Zp %*% Hp1 %*% t(Zp)

   return(Gamma %*% y)
	
}

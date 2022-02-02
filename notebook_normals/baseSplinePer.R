# Function baseSplinePer.R
#---------------------------
# Computes the periodic spline basis Zp and the gram matrix Gp
# Adapted from Alix Rigal's routines (2018)

baseSplinePer = function(p) {

	## Base spline : 
	x = (1:p)/p	# Les knots
	rp = p-1;	# Dimension de la base spline
	X = matrix(data = x,nrow = p,ncol = 1) 
	
	Y0 = diag(x=1,nrow = rp+1,ncol = rp+1)
	Y0 = Y0[1:rp,1:(rp+1)]
	Y0[1,(rp+1)] = 1
	Y0 = t(Y0)
	Y1 = Y2 = Y3 = matrix(data=0,nrow=p,ncol=rp)
	for (i in 1:rp) {
		f <- splinefun(X,Y0[,i],method = "periodic")
		Y1[,i] = f(X,deriv=1)
		Y2[,i] = f(X,deriv=2)
		Y3[,i] = f(X,deriv=3)
	}

	## Matrix Z
	Zp = Y0[1:p,]

	## Matrix G (Gram)
	# Initialization
	Gp = G0 = matrix(data = 0,nrow=dim(t(Zp)%*%Zp)[1],ncol = dim(t(Zp)%*%Zp)[2]) 
	# Calculating G
	interv = X[2:p] - X[1:(p-1)]
	interv = matrix(data = interv,nrow = length(interv),ncol = 1)
	Interv = interv%*% matrix(data = 1, nrow =1,ncol =length(Y2[1,])  )  
	for (s1 in 1:rp) { 
		# To understand this calculation, you need to come back to the definition of G, and develop the product of second order derivative on each interval
		f2 = Y2[1:(dim(Y2)[1]-1),s1]%*% matrix(data = 1, nrow =1,ncol =length(Y2[1,])  )  
		f3 = Y3[1:(dim(Y3)[1]-1),s1]%*% matrix(data = 1, nrow =1,ncol =length(Y2[1,])  )  
		g2 = Y2[1:(dim(Y2)[1]-1),]
		g3 = Y3[1:(dim(Y3)[1]-1),]
		Int = f2 * g2 *Interv + (f2 * g3 + g2 * f3)*Interv^2/2 + f3 * g3 *Interv^3/3
		Gp[s1,] =apply(Int, 2, sum)   
	}

	out = list(Z=Zp,G=Gp)
	return(out)

}

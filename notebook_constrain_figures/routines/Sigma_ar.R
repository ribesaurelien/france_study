Sigma_ar = function(alpha,n) {
	Sigma = array(NA,dim=c(n,n))
	for (i in 1:n) {
		for (j in 1:n) {
			Sigma[i,j] = alpha^(abs(i-j))
		}
	}
	return(Sigma)
}


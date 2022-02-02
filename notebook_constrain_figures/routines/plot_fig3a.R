
tmp_center = function(x) {
	x-mean(x[as.character(ref_period)])
}
scen = "histssp245"



# Plot 1: ref 1850-1900
ref_period = 1850:1900

# Fig Attribution TS: plot time-series of single forcing response
ATS = CX_full[as.character(1850:2020),"be","loc",,]
ATSc = apply(ATS,c(2,3),tmp_center)
ATSc = abind(ATSc,ATSc[,1,]-ATSc[,2,]-ATSc[,3,],ATSc[,1,]-ATSc[,2,],along=2,use.first.dimnames=T)
names(dimnames(ATSc))=c("year","forc","constrain")
dimnames(ATSc)$forc[4:5]=c("oa","ant")

#dev.new(height=5,width=8)
par(font.lab=2,font.axis=2,cex.lab=1.2,mgp=c(2.5,.7,0),mar=c(4,4,1,1))
matplot(1850:2020,ATSc[,-1,"uncons"],type="l",lwd=2,lty=2,col=c("blue","brown","chartreuse4","red"),xlab="Year",ylab="Temperature anomaly (°C)",ylim=range(ATSc))
abline(v=seq(1850,2000,50),lty=3,col="grey")
abline(h=seq(-1,3,.5),lty=3,col="grey")
matlines(1850:2020,ATSc[,-1,"cons"],lwd=2,lty=1,col=c("blue","brown","chartreuse4","red"))

legend("topleft",c("ANT","GHG","OA","NAT","Unconstrained","Constrained"),col=c("red","brown","chartreuse","blue","black","black"),lwd=2,lty=c(1,1,1,1,2,1),bg="white")

#-----------------------------




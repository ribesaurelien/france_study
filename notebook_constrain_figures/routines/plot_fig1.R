
par(font.lab=2,font.axis=2,cex.lab=1.2,mgp=c(2.5,.7,0),mar=c(4,4,1,1))

ref_period = 1900:1930
tmp_center = function(x) {
	x-mean(x[as.character(ref_period)])
}

# Observations
Xofc = Xof - mean(Xof[year_of %in% ref_period])
plot(year_of, Xofc, pch=16, ylab="Temperature (°C)", xlab="Year")
abline(v=seq(1900,2020,10),lty=3)
abline(h=seq(-1,3,.2),lty=3)


# Various constraints
mmm_raw = CX_full[as.character(year_of),"be","loc","all","uncons"]
cx_full = CX_full[as.character(year_of),"be","loc","all","cons"]
cx_glo = CX_glo[as.character(year_of),"be","loc","all","cons"]
cx_loc = CX_loc[as.character(year_of),"be","loc","all","cons"]
mmm_raw_c = tmp_center(mmm_raw)
cx_full_c = tmp_center(cx_full)
cx_glo_c = tmp_center(cx_glo)
cx_loc_c = tmp_center(cx_loc)

# Range for CX_full
cx_fulli = CX_full[as.character(year_of),-1,"loc","all","cons"]
cx_fullic = apply(cx_fulli,2,tmp_center)
cx_fullicc = cx_fullic - cx_full_c %o% ones(1:Nres)
cx_full_sd = apply(cx_fullicc,1,sd)
cx_q05 = cx_full_c + qnorm(.05) * cx_full_sd
cx_q95 = cx_full_c + qnorm(.95) * cx_full_sd

col_range = rgb(1,0,0,.15)
polygon( c(year_of,rev(year_of)), c(cx_q95,rev(cx_q05)), col=col_range, border=NA )


# Smoothing
s = smooth.spline(year_of,Xofc,df=6)
lines(s$x,s$y,lwd=2,col="orange")
lines(year_of,mmm_raw_c,lwd=2,col="grey")
lines(year_of,cx_glo_c,lwd=2,col="green")
lines(year_of,cx_loc_c,lwd=2,col="blue")
lines(year_of,cx_full_c,lwd=2,col="red")

legend("topleft",legend=c("Smoothing splines (df=6)","CMIP6 multi-model mean","KCC GSAT-only","KCC Fr-only","KCC GSAT+Fr","KCC GSAT+Fr range"),col=c("orange","grey","green","blue","red",col_range),lwd=c(2,2,2,2,2,12),bg="white",text.font=2,y.intersp=1.1,cex=.8)



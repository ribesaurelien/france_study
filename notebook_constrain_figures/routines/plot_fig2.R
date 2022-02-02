library(ellipse)

## Plot 2 : Glo / Loc warming ratio

pres_loc = 2001:2020
pres_glo = 2001:2020
ref_loc = 1900:1930
ref_glo = 1900:1930

col_obs = "black"
col_cons = "red"
col_mmm = "blue"
#col_cmip = "green3"
col_cmip = "cadetblue3"

per_label = function(per) {
	if (min(per)==max(per)) {
		return(as.character(per))
	} else {
		return(paste0(as.character(min(per)),"-",#
							as.character(max(per))))
	}
}
label_reg = paste0( per_label(pres_loc), "  wrt  ", per_label(ref_loc) )
label_glo = paste0( per_label(pres_glo), "  wrt  ", per_label(ref_glo) )


loc_fit = apply(Xf_fit[as.character(pres_loc),"all",],2,mean) - apply(Xf_fit[as.character(ref_loc),"all",],2,mean)
glo_fit = apply(Xg_fit[as.character(pres_glo),"all",],2,mean) - apply(Xg_fit[as.character(ref_glo),"all",],2,mean)

loc_obs = mean(Xof[as.character(pres_loc)]) - mean(Xof[as.character(ref_loc)])
glo_obs = mean(Xog[as.character(pres_glo),"median"]) - mean(Xog[as.character(ref_glo),"median"])

# Fited ratio in CX_full
loc_fitted = mean(cx_full[as.character(pres_loc)]) - mean(cx_full[as.character(ref_loc)])
cxg_full = CX_full[,"be","glo","all","cons"]
glo_fitted = mean(cxg_full[as.character(pres_glo)]) - mean(cxg_full[as.character(ref_glo)])


# Calculation of var_*_obs
v_loc = 0 * Xof
v_loc[as.character(pres_loc)] = 1/length(pres_loc)
v_loc[as.character(ref_loc)] = -1/length(ref_loc)
var_loc_obs = as.vector( v_loc %*% Sigma_obs_loc %*% v_loc )
v_glo = 0 * Xog[,"median"]
v_glo[as.character(pres_glo)] = 1/length(pres_glo)
v_glo[as.character(ref_glo)] = -1/length(ref_glo)
var_glo_obs = as.vector( v_glo %*% Sigma_obs_glo %*% v_glo )



#pdf(ofile)
par(font.lab=2,font.axis=2,cex.lab=1.2,mgp=c(2.5,.7,0),mar=c(5,5,1,1))
xlim = extendrange(glo_fit,f=.1)
ylim = extendrange(loc_fit,f=.1)
plot(glo_fit,loc_fit,pch=3,lwd=2,xlim=xlim,ylim=ylim,col=col_cmip,xlab="",ylab="")
title(xlab="Global warming (GSAT, °C)",font.lab=2,cex.lab=1.2,mgp=c(2.2,1,0))
title(xlab=label_glo,font.lab=2,cex.lab=.9,mgp=c(3.5,1,0))
title(ylab="Regional warming (France, °C)",font.lab=2,cex.lab=1.2,mgp=c(3.7,1,0))
title(ylab=label_reg,font.lab=2,cex.lab=.9,mgp=c(2.2,1,0))
#text( glo_fit, loc_fit, labels=names(glo_fit), cex= 0.7, font=2, pos=3)
v=par("usr")
dv = (v[2]-v[1])/(v[4]-v[3])
for (wri in c(.8,1,1.25,1.5,2)) {
	abline(a=0,b=wri,lty=3,lwd=2,col="grey")
	if (wri<1.2) {
		x0 = .1*v[1]+.9*v[2]
		text(x0,x0*wri+.05,paste0("WR=",as.character(wri)),col="grey",srt=atan(wri*dv)/pi*2*90)
	} else {
		y0 = .1*v[3]+.9*v[4]
		text(y0/wri-.04,y0,paste0("WR=",as.character(wri)),col="grey",srt=atan(wri*dv)/pi*2*90)
	}
}
points(glo_obs,loc_obs,pch=3,cex=1.2,lwd=4,col=col_obs)
points(mean(glo_fit),mean(loc_fit),pch=3,lwd=3,cex=1.2,col=col_mmm)
lines(glo_obs*c(1,1),loc_obs*c(1,1)+c(-1,1)*1.64*sqrt(var_loc_obs),col=col_obs,lwd=2)
lines(glo_obs*c(1,1)+c(-1,1)*1.64*sqrt(var_glo_obs),loc_obs*c(1,1),col=col_obs,lwd=2)
lg_reg = lm(loc_fit~glo_fit-1)
#abline(a=0,b=lg_reg$coefficients,lty=2)
points(glo_fitted,loc_fitted,pch=3,lwd=3,cex=1.2,col=col_cons)
#abline(a=0,b=loc_fitted/glo_fitted,lty=2,col="blue")
lines(ellipse(diag(c(var_glo_obs,var_loc_obs)),centre=c(glo_obs,loc_obs),level=.9),lty=2,lwd=2,col=col_obs)

# Uncertainty around unconstrained CMIP6 
loc_i_v = CX_full[,,"loc","all","uncons"]
glo_i_v = CX_full[,,"glo","all","uncons"]
loc_i = apply(loc_i_v[as.character(pres_loc),],2,mean) -  apply(loc_i_v[as.character(ref_loc),],2,mean)
glo_i = apply(glo_i_v[as.character(pres_glo),],2,mean) -  apply(glo_i_v[as.character(ref_glo),],2,mean)
#points(glo_i[-1],loc_i[-1],pch=3,lwd=1,cex=.8,col="lightblue3")
V = cbind(glo_i[-1],loc_i[-1])
Scx = var(V)
lines(ellipse(Scx,centre=c(mean(glo_fit),mean(loc_fit)),level=.9),col=col_mmm,lty=2,lwd=2)
loc_i20 = loc_i_v["2020",] -  apply(loc_i_v[as.character(ref_loc),],2,mean)

# Uncertainty around the red point
loc_i_v = CX_full[,,"loc","all","cons"]
glo_i_v = CX_full[,,"glo","all","cons"]
loc_i = apply(loc_i_v[as.character(pres_loc),],2,mean) -  apply(loc_i_v[as.character(ref_loc),],2,mean)
glo_i = apply(glo_i_v[as.character(pres_glo),],2,mean) -  apply(glo_i_v[as.character(ref_glo),],2,mean)
V = cbind(glo_i[-1],loc_i[-1])
Scx = var(V)
lines(ellipse(Scx,centre=c(glo_fitted,loc_fitted),level=.9),col=col_cons,lty=2,lwd=2)

legend("bottomright",c("Observations","Individual CMIP models","Multi-model mean","KCC GSAT+Reg"),pch=c(3,3,3,3),col=c(col_obs,col_cmip,col_mmm,col_cons),pt.lwd=2,y.intersp=1.15)


#
# NAME:
# NA
#
# PURPOSE:
# NA
#
# REQUIREMENTS:
# NA
#
# INPUT:
# NA
#
# OUTPUT:
# NA
#
# NOTES:
# NA
#
# BUGS:
# NA
#
# AUTHOR:
# NA
#
# MODIFIED:
# NA
#
# CVS_ID:
# $Id: intobsdepth.R,v 1.1 2012-03-20 20:30:21 steingod Exp $
#

intopsdepth <- function(x) {
	
    	standarddepth <- c(0,25,50,75,100,150,200,250,300,400,500)

	t <- x$data[x$data$depth<=depth,] 
	myindex <- paste(format(t$time,"%Y%m%d","GMT"),
		t$depth,sep="_")
        # Average values within day and depth
	tmp <- by(t,myindex,mean,na.rm=T)
	tmp2 <- matrix(unlist(tmp,use.names=F),ncol=6,
		dimnames=list(NULL,
		    c("time","longitude","latitude",
			"depth","temperature","salinity")),
		byrow=T)
	myindex2 <- format((ISOdate(1970,1,1,0)+tmp2[,"time"]),"%Y%m%d","GMT")
	mindepth <- by(tmp2[,"depth"],myindex2,min)
	maxdepth <- by(tmp2[,"depth"],myindex2,max)
	numdepth <- by(tmp2[,"depth"],myindex2,length)
	useful <- ifelse(mindepth==0&maxdepth==depth&numdepth>700,
		names(mindepth),
		NA)
	tmp3 <- tmp2[myindex2 %in% useful,]

}

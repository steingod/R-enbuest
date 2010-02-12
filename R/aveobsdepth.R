#
# NAME:
# aveobsdepth
#
# PURPOSE:
# To extract data for a specific depth level or average over depth levels.
# The averaging process is between the surface and specific depth level
# (e.g. 300 or 500m).
#
# REQUIREMENTS:
# NA
#
# INPUT:
# Expect an object of the type returned by readhydrography.
#
# OUTPUT:
# Returns an object of the type returned by readhydrography.
#
# NOTES:
# NA
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, METNO/FOU, 09.02.2010 
#
# MODIFIED:
# NA
#
# CVS_ID:
# $Id: aveobsdepth.R,v 1.3 2010-02-12 15:50:19 steingod Exp $
#  

aveobsdepth <- function(x,depth,method="extract") {

    if (method=="extract") {
	t <- list(info=paste(x$info,"Subsetted for depth", depth),
		data=data.frame(
		    time=x$data$time[x$data$depth==depth],
		    latitude=x$data$latitude[x$data$depth==depth],
		    longitude=x$data$longitude[x$data$depth==depth],
		    depth=x$data$depth[x$data$depth==depth],
		    temperature=x$data$temperature[x$data$depth==depth],
		    salinity=x$data$salinity[x$data$depth==depth]
		    ))
    } else if (method=="mean") {
##	t <- list(info=paste(x$info,"Weighted average until depth", depth),
##		data=data.frame(
##		    time=x$data$time[x$data$depth<=depth],
##		    latitude=x$data$latitude[x$data$depth<=depth],
##		    longitude=x$data$longitude[x$data$depth<=depth],
##		    depth=x$data$depth[x$data$depth<=depth],
##		    temperature=x$data$temperature[x$data$depth<=depth],
##		    salinity=x$data$salinity[x$data$depth<=depth]
##		    ))
	#myindex <- factor(paste(format(t$data$time,"%Y%m%d","GMT"),
	#	    t$data$depth,sep="_"))
	#mytime <- ISOdatetime(1970,1,1,0,0,0,"GMT")+tapply(t$data$time,myindex,mean,na.rm=T)
	#mydepth <- 
	#mytemp <- tapply(t$data$temperature,myindex,mean,na.rm=T)
	#mypsal <- tapply(t$data$salinity,myindex,mean,na.rm=T)

	t <- x
	myindex <- factor(paste(format(t$data$time,"%Y%m%d","GMT"),
		    t$data$depth,sep="_"))
##	tmp <- by(t$data, 
##		myindex, 
##		mean, na.rm=TRUE)
	tmp <- aggregate(t$data,list(myindex), mean, na.rm=T)
	return(tmp)

	return(list(t=mytime,d=mydepth,m=mytemp,s=mypsal))


	maxdepth <- tapply(t$data$depth,myindex,max,na.rm=T)
	mindepth <- tapply(t$data$depth,myindex,minna.rm=T)
	ndepths <- tapply(t$data$depth,myindex,length)

	return(list(a=myindex,b=maxdepth,c=mindepth,d=ndepths))

	t <- x
	x <- t$data[do.call(order,t$data[,c("time","depth")]),]
	tmp <- by(t$data[,c("temperature","depth")], 
		t$data$time, 
		function(u) weighted.mean((u$temperature[-1]+u$temperature[-nrow(u)])/2, w=diff(u$depth)/depth, na.rm=TRUE))
	return(tmp)
    } else {
	cat("Choices not supported")
	return(NULL)
    }

    return(t)
}

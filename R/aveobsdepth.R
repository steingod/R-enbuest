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
# $Id: aveobsdepth.R,v 1.4 2010-02-17 17:36:24 steingod Exp $
#  

aveobsdepth <- function(x,depth,numz=5,method="extract") {

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

	# Reduce amount of data to process according to level of
	# integration
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
	# Prepare depth averaging within day
	myindex2 <- format((ISOdate(1970,1,1,0)+tmp2[,"time"]),"%Y%m%d","GMT")
	mindepth <- by(tmp2[,"depth"],myindex2,min)
	maxdepth <- by(tmp2[,"depth"],myindex2,max)
	numdepth <- by(tmp2[,"depth"],myindex2,length)
	# Only process days with enough vertical samples
	useful <- ifelse(mindepth==0&maxdepth==depth&numdepth>numz,
		names(mindepth),
		NA)
	tmp3 <- tmp2[myindex2 %in% useful,]
	#tmp2 <- tmp3[do.call(order,tmp3[,c("time","depth")])]
	#return(tmp2)
	myindex2 <- format(ISOdate(1970,1,1,0)+tmp3[,"time"],"%Y%m%d","GMT")

	# Extract to separate function in time...
	myfun <- function(u) {
	    intdiff <- (u$temperature[-1]+u$temperature[-nrow(u)])/2
	    w <- diff(u$depth[u$depth<=depth])/depth
	    return(weighted.mean(intdiff,w,na.rm=TRUE))
	}

	# Apply weighted average over depth
	tmp2 <- by(tmp3,myindex2,myfun)

	# Prepare for export
	mytime <- strptime(names(tmp2),"%Y%m%d","GMT")
	myvalue <- unlist(tmp2,use.names=F)

	return(list(time=mytime,temperature=myvalue))


	#t <- x
	#x <- t$data[do.call(order,t$data[,c("time","depth")]),]
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

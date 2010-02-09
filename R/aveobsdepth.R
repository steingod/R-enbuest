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
# $Id: aveobsdepth.R,v 1.1 2010-02-09 16:55:37 steingod Exp $
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
    } else if (method="mean") {
	t <- list(info=paste(x$info,"Weighted average until depth", depth),
		data=data.frame(
		    time=x$data$time[x$data$depth<=depth],
		    latitude=x$data$latitude[x$data$depth<=depth],
		    longitude=x$data$longitude[x$data$depth<=depth],
		    depth=x$data$depth[x$data$depth<=depth],
		    temperature=x$data$temperature[x$data$depth<=depth],
		    salinity=x$data$salinity[x$data$depth<=depth]
		    ))
	myindex <- format(t$data$time,"%Y%m%d","GMT")

    monthlyssi <- tapply(x$shortwave,myindex,mean,na.rm=T)
	
    } else {
	return(NULL)
    }

    return(t)
}

#
# NAME:
# estheatcont
#
# PURPOSE:
# Estimate the heat content of a water column. See documentation for
# extended info.
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
# Currently a monthly column mean temperature is made without concern for
# the depths represented. Probably should generate a monthly mean for each
# level and then a weighted mean of this again??
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, METNO/FOU, 10.09.2009 
#
# MODIFIED:
# NA
#
# CVS_ID:
# $Id: estheatcont.R,v 1.1 2010-02-09 10:51:26 steingod Exp $
#  
estheatcont <- function(x, integrationdepth=500) {

    # Some constants for heat content estimation (density and heat
    # capacity).
    rho <- 1.e3
    cp <- 4218

    # Estimate mean values between depths and weight these according to
    # the number of meters the observations represent.
    #
    # Originally the mean value between two and two depths were created by
    # weighting against the number of meters these observations represent.
    # Then these weighted mean values were summarised and divided by the
    # sum of weights to find the weighted mean value for the water column.
    #
    # Mean temperatures are currently not weighted, must be redone...
    # Should split in two tasks, first create weighted mean for each time
    # step, then create monthly means...

    # Discard observations outside the integration depth
    mydata <- x[x$depth <= integrationdepth,]

    # Do the vertical weighted mean
    myindex1 <- as.integer(format(mydata$time,"%Y%j"))
    mydata$timeid <- myindex1
    tmp <- split(mydata,mydata$timeid)
    tmp <- lapply(tmp,function(u) cbind(u, w=weightest(u$depth)))
    tmp <- lapply(tmp, function(u) weighted.mean(u$temperature,u$w))
    tmp <- data.frame(do.call("rbind",tmp))
    colnames(tmp) <- "temperature"

    # Do the monthly means
    tmp$timeid <- strftime(
               strptime(rownames(tmp),"%Y%j",tz="GMT"),"%Y%m",tz="GMT"
               )
    tmp <- data.frame(
            temperature = tapply(tmp$temperature,tmp$timeid,mean,na.rm=T))
    tmp$time <- strptime(paste(rownames(tmp),"15",sep=""),"%Y%m%d",tz="GMT")
    return(tmp)

    # Estimate the difference in heat content between two consequtive time
    # steps (monthly means). Based on monthly means.
    deltat <- diff(tmp$temperature,1,2)
    delta <- cp*rho*integrationdepth*deltat
    tmp$deltaq <- delta/(30.*24*3600)

    return(tmp)
}

weightest <- function(u) {
  n <- length(u)
  w <- if (n == 1) {
      1 
  } else {
      c((u[2]-u[1])/2, diff(u,lag=2), (u[n]-u[n-1])/2)
  }
  return(w/u[length(u)])
}

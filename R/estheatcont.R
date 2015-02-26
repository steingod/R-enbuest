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

    # Discard observations outside the integration depth
    mydata <- x[x$depth <= integrationdepth,]

    # Potentially interpolate to standard depths prior to further
    # processing, discard profiles with too few observations...

    # Do the vertical weighted mean
    mydata$timeid <- as.integer(format(mydata$time,"%Y%j"))
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

    # Interpolate missing months. Interpolation is done using monhtly
    # means for the other years, not the previous and subsequent months
    # within the year.

    # Check for missing months (not represented by NAs)
    tmp1 <- strftime(seq(min(tmp$time),max(tmp$time),by="month"),"%Y%m")
    tmp1 <- tmp1[tmp1 %in% rownames(tmp) == F]
    tmp1 <- data.frame(time=strptime(paste(tmp1,"15",sep=""),"%Y%m%d"),temperature=NA)
    tmp <- rbind(tmp,tmp1)
    tmp <- tmp[order(tmp$time),]
    rownames(tmp) <- strftime(tmp$time,"%Y%m")

    # Do the monthly interpolation
    tmp$mon <- strftime(tmp$time,"%m")
    tmp1 <- split(tmp,tmp$mon)
    tmp2 <- lapply(tmp1,function(u) approx(as.POSIXct(u$time),
               u$temperature,
               xout=as.POSIXct(u$time[is.nan(u$temperature)|is.na(u$temperature)]),ties="ordered",rule=2))
    tmp2 <- do.call("rbind",lapply(tmp2,as.data.frame))
    colnames(tmp2) <- c("time","temperature")
    rownames(tmp2) <- strftime(tmp2$time,"%Y%m")
    tmp1 <- which(is.nan(tmp$temperature),arr.ind=T)
    tmp1 <- rownames(tmp[tmp1,])
    tmp$interp <- tmp$temperature
    tmp[tmp1,"interp"] <- tmp2[tmp1,"temperature"]
    return(tmp)

    # Estimate the difference in heat content between two consequtive time
    # steps (monthly means). Based on monthly means.
    deltat <- c(NA,diff(tmp$temperature,1,2),NA)
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

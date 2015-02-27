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
# Øystein Godøy, METNO/FOU, 2013-01-15
# Øystein Godøy, METNO/FOU, 2015-02-27 
#
estheatcont <- function(x, integrationdepth=500) {

    # Some constants for heat content estimation (density and heat
    # capacity).
    rho <- 1.e3
    cp <- 4218

    # Discard observations outside the integration depth
    # Need to check this carefully as sometimes there is a gap between 200
    # and e.g. 800 m. The hardcoded values has to be revisited. This
    # filtering should probably be done after interpolation to standard
    # levels...
    mydata <- x[x$depth <= (800),]

    # Create list, assuming one profile per day
    mydata$timeid <- as.integer(format(mydata$time,"%Y%j"))
    tmp <- split(mydata,mydata$timeid)

    # Check that the profiles contain enough levels to properly estimate
    # water column temperature. Standard levels are:
    # 0, 10, 25, 50, 75, 100, 150, 200, 300, 400, 500 etc.
    # Check on profile depth is hardcoded as no passing of arguments is
    # allowed.
    tmp <- lapply(tmp,function(u) if (max(u$depth) > 500) u else NULL)
    tmp <- tmp[!sapply(tmp,is.null)]
    tmp <- lapply(tmp,function(u) if (length(u$depth) > 4) u else NULL)
    tmp <- data.frame(do.call("rbind",tmp))
    tmp <- split(tmp,tmp$timeid)
    stdlev <- c(0, 10, 25, 50, 75, 100, 150, 200, 300, 400, 500)
    
    # Interpolation to standard levels... 
    # Not used currently...
    tmp1 <- lapply(tmp, function(u) approx(u$depth,u$temperature,xout=c(0,
                                                                       10,
                                                                       25,
                                                                       50,
                                                                       75,
                                                                       100,
                                                                       150,
                                                                       200,
                                                                       300,
                                                                       500)))

    # Discard profiles with too few observations... (???)

    # Do the vertical weighted mean
    # Estimate mean values between depths and weight these according to
    # the number of meters the observations represent.
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

    # Check for missing months (not represented by NAs) and add NAs for
    # these
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

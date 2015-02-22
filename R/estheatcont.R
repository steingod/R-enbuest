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
    mytime <- strptime(paste(x$data$Date,x$data$Time,x$data$Time_Zone),
	format="%Y-%m-%d %H:%M UTC")
    myindex <- as.integer(format(mytime,"%Y%m"))
    meantemp <- tapply(x$data$Temperature,list(myindex,x$data$Depth),mean,na.rm=T)
    return(list(id1=levels(factor(myindex)),id2=levels(factor(x$data$Depth)),id3=meantemp))

    # Estimate the difference in heat content between two consequtive time
    # steps. Based on monthly means.
    #deltat <- ??
    delta <- cp*rho*integrationdepth*deltat
    delatq <- delat/(30.*24*3600)

    return(data.frame(time=mytime,heatcontent=deltaq))
}

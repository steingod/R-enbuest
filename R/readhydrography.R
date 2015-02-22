#
# NAME:
# readhydrography
#
# PURPOSE:
# To read hydrographic information from OWS MIKE. Data are collected from
# ICES and in ICES format. A perl script is used to convert the ICES
# format to a more readable ASCII format. See ices2txt for details.
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
# Øystein Godøy, METNO/FOU, 11.09.2009 
#
# MODIFIED:
# NA
#
# CVS_ID:
# $Id: readhydrography.R,v 1.1 2010-02-09 10:51:26 steingod Exp $
#

readhydrography <- function(file) {
    t <- read.table(file=file,col.names=c("Date","Time","Time_Zone","Latitude","Longitude","Depth","Temperature","Salinity"),na.strings="-999.00")

    mytime <- strptime(paste(t$Date,t$Time,t$Time_Zone,sep=" "),"%Y-%m-%d %H:%M UTC",tz="UTC")

    r <- data.frame(time=mytime,latitude=t$Latitude,longitude=t$Longitude,depth=t$Depth,temperature=t$Temperature,salinity=t$Salinity)

    return(list(info="Hydrographic data from OWS MIKE",data=r))
}

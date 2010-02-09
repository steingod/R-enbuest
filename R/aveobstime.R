#
# NAME:
# aveobs
#
# PURPOSE:
# To average observations.
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
# Øystein Godøy, METNO/FOU, 09.02.2010
#
# MODIFIED:
# NA
#
# CVS_ID:
# $Id: aveobstime.R,v 1.1 2010-02-09 16:55:37 steingod Exp $
#

aveobstime <- function(time,value,period="month") {

    if (period == "month") {
	myindex <- format(time,"%Y%m","GMT")
    } else if (period == "day") {
	myindex <- format(time,"%Y%m%d","GMT")
    } else if (period == "week") {
	myindex <- format(time,"%Y%W","GMT")
    } else {
	myindex <- format(time,"%Y","GMT")
    }
    mytime <- ISOdatetime(1970,1,1,0,0,0,"GMT")+tapply(time,myindex,mean,na.rm=T)
    ii <- order(mytime)
    myvalue <- tapply(value,myindex,mean,na.rm=T)

    return(list(info="Average values",data=data.frame(time=mytime,value=myvalue)))
}

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
# $Id: aveobs.R,v 1.1 2010-02-09 11:09:12 steingod Exp $
#

avobs <- function(time,value,period="month") {

    if (period == "month") {
	myindex <- format(time,"%Y%m","GMT")
    } else if (period == "day") {
	myindex <- format(time,"%Y%m%d","GMT")
    } else {
	myindex <- format(time,"%Y","GMT")
    }
    mytime <- tapply(time,myindex,mean,na.rm=T)
    ii <- order(mytime)
    myvalue <- tapply(value,myindex,mean,na.rm=T)

    return(list(info="Average values",data=data.frame(time=mytime,value=myvalue)))
}

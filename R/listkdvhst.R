#
# NAME:
# listkdvhst
#
# PURPOSE:
# List stations available within KDVH.
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
# J. B. Bremnes, METNO/FOU, Original code from Bremnes, modified...
#
# MODIFIED:
# Øystein Godøy, METNO/FOU, 10.09.2009
#
# CVS_ID:
# $Id: listkdvhst.R,v 1.1 2010-02-09 10:51:26 steingod Exp $
#  

listkdvhst <- function(fields=c("ST_NAME","STNR","WMO_NO","UTM_E","UTM_N",
	"LON_DEC","LAT_DEC","AMSL"),
	period=NULL, type=NULL, dep=NULL, print.url=FALSE) {
  
    myurl <- "http://klapp.oslo.dnmi.no/metnopub/production/metno?re=16"
    myurl <- paste(myurl, "&nod=NA&ct=text/plain&del=semicolon", sep="")

# Specifies the period to be examined
    if (!is.null(period)) {
	myurl <- paste(myurl, "&fy=", period[1], sep="")
	if (length(period) == 2) {
	    myurl <- paste(myurl, "&ty=", period[2], sep="")
	}
    }

# Specifies the type of station to be examined
    if (!is.null(type)) { 
	myurl <- paste(myurl, paste("&t=",type,sep="",collapse=""), sep="")
    }

# Specifies the department to be examined
    if (!is.null(dep)) {
	myurl <- paste(myurl, paste("&cnr=",type,sep="",collapse=""), sep="")
    }
 
# Print URL used if required
    if (print.url) print(myurl)
    
# Read data and handle errors
    x <- NULL
    try(x <- read.table(myurl, header=TRUE, 
		sep=";", as.is=TRUE, stringsAsFactors=FALSE) )
    if (!is.null(x) && !is.null(fields)) {
	x <- x[,c(fields, setdiff(names(x),fields))] 
    }
  
    return(x)
}


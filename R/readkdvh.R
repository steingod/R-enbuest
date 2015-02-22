#
# NAME:
# readkdvh
#
# PURPOSE:
# Read meteorlogical data from KDVH.
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
# $Id: readkdvh.R,v 1.1 2010-02-09 10:51:26 steingod Exp $
#  
readkdvh <- function(stnr=76900, stname=NULL, 
	period=NULL, month=NULL, hour=NULL,
	prm=c("TA","TD","TW","PR","NN","NH","CH","CI","CL","UU","FF","DD"), 
	UTC=TRUE, daily=FALSE,
	adata=FALSE, rr.pos=TRUE, collapse.time=TRUE, print.url=FALSE) {

    myurl <- "http://klapp.oslo.dnmi.no/metnopub/production/metno"
	myurl <- paste(myurl, if (daily) "?re=14" else "?re=17", sep="")
	myurl <- paste(myurl, "&nod=NA&ct=text/plain&ddel=dot&del/space", sep="")

    if (daily)  {
	myurl <- paste(myurl, "&split=1", sep="")
    }

# Specifies whether UTC time should be used (default)
    myurl <- paste(myurl, if (UTC) "&nmt=0" else "&nmt=1", sep="")

# Specifies whether data from automatic stations should be used (default
# not).
    if (adata) {
	myurl <- paste(myurl, "&dup=A", sep="")
    }

# Specifies the time period to collect data from.
    if (is.null(period)) {
	period <- format(c(Sys.time()-7*24*3600,Sys.time()), "%d.%m.%Y")
    } else if (is.numeric(period) && period<1e5) {
	period <- format(c(Sys.time()-(period-1)*24*3600,Sys.time()), "%d.%m.%Y")
    } else {        
	period <- as.character(period)
	for (i in 1:length(period)) {
	    period[i] <-  paste(substring(period[i],7,8), 
	    substring(period[i],5,6), substring(period[i],1,4), sep=".")
	}
    }
    if (length(period)==1) {
	period <- c(period, format(Sys.time(), "%d.%m.%Y"))
    }
    if (length(period)!=2) {
	stop("'period' is not correctly specified.\n")
    }
    myurl <- paste(myurl, "&fd=", period[1], "&td=", 
	    period[2], sep="", collapse="")
   
# Specifies the stations to collect
    if (!is.null(stname)) {
	st   <- miDVHstations()
	stnr <- NULL
	for (i in stname)
	    if (any(k <- grep(i, st$ST_NAME, ignore.case=TRUE))) {
		stnr <- c(stnr, st$STNR[k])
	    }
	if (length(stnr) == 0) {
	    cat("No stations found.\n")
		return(NULL)
	}
    }
    myurl <- paste(myurl, paste("&s=",stnr,sep="",collapse=""), sep="")
  
# Specifies monthly values to collect
    if (!is.null(month)) {
	myurl <- paste(myurl, paste("&m=", month-1, sep="",collapse=""), sep="")
    }

# Specifies the hourly values to collect
    if (!is.null(hour) && !daily) {
	myurl <- paste(myurl, paste("&h=", hour, sep="",collapse=""), sep="")
    }

# Specifies the parameters to collect
    myurl <- paste(myurl, paste("&p=",prm,sep="",collapse=""), sep="")

# Dump the URL if requested
    if (print.url) {
	print(myurl)
    }

# Do some locomotion to ensure enough space
    days  <- as.numeric(difftime(strptime(period[2],format="%d.%m.%Y"),
		strptime(period[1],format="%d.%m.%Y"), units="days")) + 1
    hours <- if (is.null(hour)) 24 else length(hour)
    nrows <- if (daily) days*length(stnr) else days*hours*length(stnr)
    nrows <- if (nrows > 10000) floor(nrows*1.05) else -1
    x  <- NULL

# Read data and handle any errors
    try(x <- read.table(myurl, header=TRUE, nrows=nrows, 
	stringsAsFactors=FALSE))
    if (!is.null(x)) {
	names(x) <- toupper(names(x))
	k <- grep("TIME", names(x))
	if (length(k)==1) {
	    names(x)[k] <- "HOUR"
	}
	if (collapse.time) {
	    if ("HOUR" %in% names(x)) {
		x[,2] <- x[,"YEAR"]*1e6 + x[,"MONTH"]*1e4 + x[,"DAY"]*100 + x[,"HOUR"]
		x <- x[,-(3:5)]
		names(x)[2] <- "TIME"
	    } else {
		x[,2] <- x[,"YEAR"]*1e6 + x[,"MONTH"]*1e4 + x[,"DAY"]*100
		x <- x[,-(3:4)]
		names(x)[2] <- "TIME"
	    }
	}
    }

# Set negative precipitation to zero
    if (rr.pos) {
	if (i <- match("RR", names(x), nomatch=0)) {
	    k      <- x[,i] < 0 & !is.na(x[,i])
		x[k,i] <- 0
	}
    }
  
    if (!is.null(stname)) {
	k <- match(x$STNR, st$STNR, nomatch=0)
	x <- data.frame(ST_NAME=st$ST_NAME[k], x, stringsAsFactors=FALSE)
    }
  
    return(x)
}

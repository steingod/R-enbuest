\name{readkdvh}

\alias{readkdvh}

\title{
    Extract data from METNO's observations database. 
}

\description{
    Quality controlled observations are extracted from the observations
    database at METNO. Data are extrcted using a URI interface.
}

\usage{
    readkdvh(stnr=76900, stname=NULL, 
	period=NULL, month=NULL, hour=NULL,
	prm=c("TA","TD","TW","PR","NN","NH","CH","CI","CL","UU","FF","DD"), 
	UTC=TRUE, daily=FALSE,
	adata=FALSE, rr.pos=TRUE, collapse.time=TRUE, print.url=FALSE)
}

\arguments{
    \item{stnr}{Station identification}
    \item{prm}{Parameter identifications}
    \item{period}{The time period to be collected}
    \item{UTC}{Collects data in UTC if TRUE}
}

\value{
    Returns a \link{data.frame}
}

\seealso{
    \link{readhydrography}, \link{listkdvhst}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    # Dump information on Polarfront
    t <- listkdvh()
    t[t$ST_NAME=="MIKE"]
    ST_NAME  STNR WMO_NO  UTM_E   UTM_N LON_DEC LAT_DEC AMSL  DEPARTMENT 1055    MIKE 76900     NA -86618 7381058       2      66    6 NORSKEHAVET
    t <- readkdvh(stnr=76900)
}

\keyword{
}

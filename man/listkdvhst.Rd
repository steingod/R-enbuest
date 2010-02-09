\name{listkdvhst}

\alias{listkdvhst}

\title{
    Lists the available stations within the observations database
}

\description{
    Connects to the METNO obserrvations database and extracts information
    on the stations available. Information on other elements is to be
    added in the future.
}

\usage{
    listhdvhst(fields=c("ST_NAME","STNR","WMO_NO","UTM_E","UTM_N",
	"LON_DEC","LAT_DEC","AMSL"),
	period=NULL, type=NULL, dep=NULL, print.url=FALSE)
}

\arguments{
    \item{}{}
}

\value{
    Returns a \link{data.frame}
}

\seealso{
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    # Dump information on Polarfront
    t <- listkdvh()
    t[t$ST_NAME=="MIKE"]
}

\keyword{
}

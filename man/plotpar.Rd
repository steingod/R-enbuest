\name{plotpar}

\alias{plotpar}

\title{
    Plot hydrography
}

\description{
    Plots temperature and salinity from hydrographic records.
}

\usage{
    plotpar(x,param="temp",depth=0)
}

\arguments{
    \item{x}{Object as returned by \link{readhydrography}}
    \item{param}{Text string of type "temp" or "sal"}
    \item{depth}{Depth level to extract and plot. Defaults to 0, can be
    provided as a vector of e.g. type c(0,50,100)}
}

\value{
    NA
}

\seealso{
    \link{readhydrography}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
}

\keyword{
}

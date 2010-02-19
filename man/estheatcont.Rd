\name{estheatcont}

\alias{estheatcont}

\title{
    Estimate the temporal change in heat content of a water column.
}

\description{
    The mean temperature of a water column is estimated using a weighted
    mean. This is transformed into the heat content of the water column
    and subsequently the change in heat content over time is estimated.
    The heat content is estimated for the uppermost 500m of the water
    column.

    Standard depths of 0, 10, 25, 50, 75, 100, 150, 200, 300, 400 and 500
    meter are used. Subsequent version will include other depths.
}

\usage{
    estheatcont(x,ntegrationdepth=500)
}

\arguments{
    \item{x}{A data object returned from \link{readhydrography}}
    \item{integrationdepth}{The depth until which the mean temperature
    should be estimated}
}

\value{
    Returns a \link{list}
}

\seealso{
    \link{readhydrography}, \link{aveobsdepth}, \link{aveobstime}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    h <- estheatcont(x)
}

\keyword{
}

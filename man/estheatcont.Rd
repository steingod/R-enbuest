\name{estheatcont}

\alias{estheatcont}

\title{
    Estimate the temporal change in heat content of a water column.
}

\description{
    The mean temperature of a water column is estimated using a weighted
    mean over a integration depth. This is transformed into the heat
    content of the water column and subsequently the change in heat
    content over time is estimated.  The heat content is by default
    estimated for the uppermost 500m of the water column.

    Currently standard depths are not used, but the existing depths are
    used in the weighted average for each profile. From these averaged
    profiles a monthly mean is constructed. Missing months within the time
    period spanned are interpolated using \link{approx}.
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
    Returns a \link{data.frame} with averaged temperature profiles for
    each month as well as interpolated values and the actual heat content
    change.
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

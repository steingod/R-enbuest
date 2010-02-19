\name{aveobstime}

\alias{aveobstime}

\title{
    Estimate an average value
}

\description{
    Given observations located in time, average values for various time
    periods (e.g. year, month, week, day) are generated.
}

\usage{
    aveobstime(time, values, period="month")
}

\arguments{
    \item{time}{Time identification of the values to integrate}
    \item{values}{The values to be integrated}
    \item{period}{Character representation of the period to integrate over}
}

\value{
    \link{list} containing info and data, where data is a
    \link{data.frame}
}

\seealso{
    \link{aveobsdepth}, \link{readhydrography}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
}

\keyword{
}

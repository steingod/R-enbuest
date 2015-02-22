\name{aveobsdepth}

\alias{aveobsdepth}

\title{
    Estimate an average value
}

\description{
    Given observations located in time and depth, average values for
    various depths are generated. Currently only a weighted average of
    temperature is returned, no weighted average of ssalinity is produced.
}

\usage{
    aveobsdepth(x, depth, numz=5, method="extract")
}

\arguments{
    \item{x}{An object of the type returned by \link{readhydrography}.}
    \item{depth}{The dept level to extract or to integrate from surface
    to.}
    \item{numz}{The number of vertical levels required to perform a
    weighted average within the layer.}
    \item{method}{Text describing whether extraction or averaging is to be
    performed. Valid kyewords are presently "extract" and "mean".}
}

\value{
    \link{list} containing info and data, where data is a
    \link{data.frame}
}

\seealso{
    \link{readhydrography}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    mydata <- readhydrography("/data/ices/owsmdata.txt")
    # Extract level 300 data
    owsm300m <- aveobsdepth(mydata,300)
    # Generate weighted average of temperature from surface to 300 m depth
    owsm300mtemp <- aveobsdepth(mydata,200,method="mean")
}

\keyword{
}

\name{readhydrography}

\alias{readhydrography}

\title{
    Read hydrography data prepared from ICES format by the external
    software ices2txt.
}

\description{
    Reads ASCII files containing hydrography data collected in ICES format
    from ICES. The ICES format is transformed into a ASCII format by the
    software ices2txt.
}

\usage{
    readhydrography(file)
}

\arguments{
    \item{file}{The file to read}
}

\value{
    \link{list} containing info and data, where data is a
    \link{data.frame}
}

\seealso{
    \link{readkdvh}, \link{aveobsdepth}, \link{aveobstime}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
}

\keyword{
}

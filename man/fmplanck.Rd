\name{ fmplanck }

\alias{ planck }

\title{
    Planck function
}

\description{
    Estimate the output of the planck function for a given wavelength
    range and temperature. The Planck function returns amount of
    electromagnetic energy for a certain wavelength as it is radiated by a
    black body in thermal equilibrium (i.e. the spectral radiance of a
    black body). The output is provided in Watt per square meter per
    wavelength.
}

\usage{
    planck(wavelen, temp, p=T, r=F, ...)
}

\arguments{
    \item{wavelength}{Wave length specified in meter. Micrometer is
    1e-6meter}
    \item{temp}{Temperature of the black body specified in Kelvin. The
    mean temperature of the Earth is about 287.6K and the temperature of
    the Sun is about 5777K.}
    \item{p}{Boolean defining whether a graphical plot of the output
    should be made or not.}
    \item{r}{Boolean defining whether the computed values should be
    returned or not.}
    \item{...}{Additional parameters to plot command.}
}

\value{
    A \link{data.frame} containing the wavelength and output.
}

\seealso{
}

\author{
    Ãystein GodÃ¸y (\email{o.godoy@met.no})
}

\examples{
}

\keyword{
}

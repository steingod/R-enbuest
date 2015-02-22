#
# NAME:
# planck.fun
#
# PURPOSE:
# Computes the Planck function for a given wavelength 
# interval and temperature. 
# Wavelength is given in meters and temperature in Kelvin.
# Differences between different constants are usually 
# caused by different definitions of the first constant.
#
# AUTHOR:
# Oystein Godoy, DNMI/FoU, 02.09.1997
# Ãystein GodÃ¸y, METNO/FOU, 2012-03-18 
#

fmplanck <- function(wavelen, temp, p = T, r = F, ...) {

    rad <- vector(mode="numeric", length=length(wavelen))

    # First and second Planck constants from Liou, compares well
    # with Kidder and Vonder Haar
    c1 <- 1.1910439e-16
    c2 <- 0.01438769
    # First and second Planck constants from Wallace and Hobbs
    # 	c1 <- 3.74e-16
    #	c2 <- 0.0144
    rad <- ((c1 * (wavelen^(-5.)))/(exp((c2)/(wavelen * temp)) - 1.))
    if(p == T) {
        plot(wavelen/1e-06, rad * 1e-06 * 1e-07, type = "l", xlab = 
                "micrometer", ylab = "intensity (W/mÂ²umâ»Â¹srâ»Â¹)*10â·",...
            )
        title(paste("Planck function output for",temp,"K"))
    }
    if(r == T) {
        return(data.frame(wavelength=wavelen,intensity=rad))
    }
}

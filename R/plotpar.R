plotpar <- function(x,param="temp",depth=0) {

    xdata <- x$data$time[x$data$depth == depth]
    if (param == "temp") {
	ydata <- x$data$temperature[x$data$depth == depth]
    } else if (param == "sal") {
	ydata <- x$data$salinity[x$data$depth == depth]
    } else {
	return("Unknown parameter requested")
    }

    plot(xdata,ydata)

}

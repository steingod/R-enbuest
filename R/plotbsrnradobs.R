plotbsrnradobs <- function() {
    library(mapproj)
    library(mapdata)

    # These have only shortwave sensors
    bsrnst <- as.data.frame(matrix(c(
                    "Ny-Ålesund",00000,78.933,11.950,
                    "Lerwick",00000,60.133,-1.183,
                    "Lindenberg",00000,52.217,14.117,
                    "Cabauw",00000,51.97,4.93,
                    "Lindenberg",00000,52.21,14.1220,
                    "Toravere",00000,58.2540,26.4620,
                    "Camborne",00000,50.217,-5.317),
                byrow=T,nrow=7,ncol=4,
                dimnames=list(NULL,c("Name","Id","Lat","Lon")))) 

    # These have both shortwave and longwave sensors
    otherst <- as.data.frame(matrix(c(
                    "Sodankylä",00000,67.220,26.390,
                    "Polarfront",00000,66.0,2.0),
                byrow=T,nrow=2,ncol=4,
                dimnames=list(NULL,c("Name","Id","Lat","Lon"))))

    myorientation <- c(60,0,0)
    mytextcol <- "white"
    mypointcol <- "white"
    #mymapcolfg <- "palegreen4"
    mymapcolfg <- "black"
    mymapcolbg <- "snow4"

    map("worldHires",c(
                "Norway","Sweden","Denmark","Finland",
                "UK",
                "Netherlands","Germany","Belgium",
                "Estonia","Lithuania","Latvia",
                "Poland","USSR"),
                proj="stere",orient=myorientation,
                ylim=c(50,78),xlim=c(-10,35),
                fill=F,bg=mymapcolbg,col=mymapcolfg)
    map.grid(col=1,nx=3,ny=3,cex=0.5)
    
    # Plot BSRN stations
    points(mapproject(bsrnst$Lon,bsrnst$Lat,proj="stere",orient=myorientation),col=mypointcol,bg=mypointcol,pch=22)
    text(mapproject(bsrnst$Lon,bsrnst$Lat,proj="stere",orient=myorientation),col=mytextcol,labels=bsrnst$Name, pos=4, cex=0.75)

    # Plot other stations
    points(mapproject(otherst$Lon,otherst$Lat,proj="stere",orient=myorientation),col=mypointcol,bg=mypointcol,pch=23)
    text(mapproject(otherst$Lon,otherst$Lat,proj="stere",orient=myorientation),col=mytextcol,labels=otherst$Name, pos=4, cex=0.75)

    # Add title and legend
    title("BSRN and other stations observing radiative fluxes")

    legend(mapproject(x=-30,y=75,proj="stere",orient=myorientation),
            pch=c(22,23),
            legend=c(
                "BSRN stations",
                "Other stations"),
            cex=0.6,pt.bg=mypointcol,bg="grey",y.intersp=1.2)

}

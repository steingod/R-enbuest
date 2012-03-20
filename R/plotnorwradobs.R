plotnorwradobs <- function() {
    library(mapproj)
    library(mapdata)

    # These have only shortwave sensors
    bioforskst <- as.data.frame(matrix(c(
                    "Tjøtta",76530,65.83,12.43,
                    "Vågønes",82260,67.28,14.47,
                    "Holt",90400,69.67,18.93,
                    "Apelsvoll",11500,60.70,10.87,
                    "Løken",23500,61.12,9.07,
                    "Landvik",38140,58.33,8.52,
                    "Særheim",44300,58.78,5.68,
                    "Fureneset",56420,61.30,5.05,
                    "Kvithamar",69150,63.50,10.87),
                byrow=T,nrow=9,ncol=4,
                dimnames=list(NULL,c("Name","Id","Lat","Lon")))) 

    # These have both shortwave and longwave sensors
    metnost <- as.data.frame(matrix(c(
                    "Hopen",00000,76.3,24.04,
                    "Jan Mayen",00000,70.933333,-8.6666667,
                    "Bjørnøya",00000,74.5166667,19.01666667),
                byrow=T,nrow=3,ncol=4,
                dimnames=list(NULL,c("Name","Id","Lat","Lon"))))

    # These have both shortwave and longwave sensors
    uibst <- as.data.frame(matrix(c(
                    "Bergen",00000,60.400,5.320,
                    "Ekofisk",00000,56.32,3.13),
                byrow=T,nrow=2,ncol=4,
                dimnames=list(NULL,c("Name","Id","Lat","Lon"))))

    myorientation <- c(60,0,0)
    mytextcol <- "white"
    mypointcol <- "white"
    mymapcolfg <- "palegreen4"
    mymapcolbg <- "snow4"

    map("worldHires",c("Norway","Sweden","Denmark","Finland"),proj="stere",orient=myorientation,ylim=c(56,78),xlim=c(-10,30),fill=T,bg=mymapcolbg,col=mymapcolfg)
    map.grid(col=1,nx=3,ny=3,cex=0.5)
    
    # Plot Bioforsk stations
    points(mapproject(bioforskst$Lon,bioforskst$Lat,proj="stere",orient=myorientation),col=mypointcol,bg=mypointcol,pch=20)
    text(mapproject(bioforskst$Lon,bioforskst$Lat,proj="stere",orient=myorientation),col=mytextcol,labels=bioforskst$Name, pos=4, cex=0.75)

    # Plot METNO stations
    points(mapproject(metnost$Lon,metnost$Lat,proj="stere",orient=myorientation),col=mypointcol,bg=mypointcol,pch=22)
    text(mapproject(metnost$Lon,metnost$Lat,proj="stere",orient=myorientation),col=mytextcol,labels=metnost$Name, pos=4, cex=0.75)

    # Plot UiB stations
    points(mapproject(uibst$Lon,uibst$Lat,proj="stere",orient=myorientation),col=mypointcol,bg=mypointcol,pch=23)
    text(mapproject(uibst$Lon,uibst$Lat,proj="stere",orient=myorientation),col=mytextcol,labels=uibst$Name, pos=4, cex=0.75)

    # Add title and legend
    title("Norwegian stations observing radiative fluxes")

    legend(mapproject(x=-30,y=75,proj="stere",orient=myorientation),
            pch=c(20,22,23),
            legend=c(
                "Bioforsk stations observes\nshortwave irradiance",
                "METNO stations observes\nshortwave and longwave irradiance",
                "UiB stations observes\nshortwave and longwave irradiance"),
            cex=0.6,pt.bg=mypointcol,bg="grey",y.intersp=1.2)

}

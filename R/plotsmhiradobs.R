plotsmhiradobs <- function() {
    library(mapproj)
    library(mapdata)

    myorientation <- c(60,0,0)
    mytextcol <- "white"
    mypointcol <- "white"
    mymapcolfg <- "palegreen4"
    mymapcolbg <- "snow4"

    map("worldHires",c("Norway","Sweden","Finland","Denmark"),proj="stere",orient=myorientation,ylim=c(55,70),xlim=c(0,30),fill=T,bg=mymapcolbg,col=mymapcolfg)
    map.grid(col=1,nx=3,ny=3,cex=0.5)
    
    points(mapproject(smhiradobs[,4],smhiradobs[,3],proj="stere",orient=myorientation),col=mypointcol,bg=mypointcol,pch=c(22,23,20,20,20,20,22,20,20,20,23,20,23,20,20,20))

    text(mapproject(smhiradobs[,4],smhiradobs[,3],proj="stere",orient=myorientation),col=mytextcol,labels=smhiradobs[,1],
    pos=4, cex=0.75)

    title("SMHI operated stations observing radiative fluxes")

    legend(mapproject(x=5,y=73,proj="stere",orient=myorientation),pch=c(20,22,23),legend=c("Shortwave irradiance",
    "Shortwave and longwave irradiance",
    "Shortwave and longwave irradiance, direct and diffuse shortwave"),
    cex=0.6,bg="grey")

}

#'Create a 3-D stem plot
#'
#'@description plotStem3D is used to Create a 3-D stem plot
#'@usage plotStem3D(hi,di,col, alpha, labels,box) 
#'@param hi, vector of trees his
#'@param di, vector of trees dis
#'@param col, stem color, e.g. "chocolate"
#'@param alpha, stem transparency. Set a value from 0 to 1
#'@param labels, if TRUE, the section heighs will be labed
#'@param box, a 3D box will be added to the plot
#'@author Carlos Alberto Silva
#'@examples
#'# Importing forest inventory data
#'fpath<- system.file("extdata", "taper.csv", package="rForest") 
#'TreeList<-read.table(fpath, sep=",", head=T) # reading data
#'
#'# Subsetting Tree 1
#'T1<-subset(TreeList,TreeList[,2]==1)
#'hi<-T1$Sec
#'di<-T1$CCC/pi
#'
#'# Plotting stem 3d
#'plotStem3D(hi,di,alpha=1,col="green",labels=TRUE,box=TRUE)
#'aspect3d(0.6,0.5,2)
#'title3d(zlab = "Height (m)", col="black") # title
#'@importFrom rgl triangles3d
#'@importFrom geometry convhulln
#'@export
plotStem3D<-function(hi,di,col="chocolate4", alpha=0.75, labels=TRUE,box=FALSE) {

  cilinder<-function(X0,Y0,r,z) {
    disfun<-sqrt(1)
    angs<-seq(0,2*pi,length=60)
    x<-X0 + r*disfun*cos(angs)
    y<-Y0 + r*disfun*sin(angs)
    return(cbind(x,y,rep(z,60)))
  }
  
  hidi<-cbind(hi,di)
  maxr<-max(hidi)/2
  x=0
  y=0
  rgl::open3d()

  rgl::bg3d(col="white")
  rgl::view3d( theta = -90, phi = -90)
  for ( i in 2:(nrow(hidi))) {
      sec_i<-hidi[(i-1):i,]
        c0<-cilinder(x,y,(sec_i[1,2]/2),sec_i[1,1])
          c1<-cilinder(x,y,(sec_i[2,2]/2),sec_i[2,1])
        xyz<-cbind(rbind(c0,c1))
        rgl::plot3d(xyz,type="l",lwd=2,xlab="",add=T,col="black",ylab="",axes=FALSE, xlim=c(x-maxr-2,x+maxr+2),ylim=c(y-maxr-2,y+maxr+2))
      ch <- t(convhulln(xyz, "QJ"))
      rgl::rgl.triangles(xyz[ch,1],xyz[ch,2],xyz[ch,3], col=col,alpha=alpha)
  }
  
  if (labels==TRUE) {
    rgl::segments3d(c(-maxr-2,-maxr-2),c(0,0), c(0,max(hidi[,1])),lwd=3, col="black")
  for( i in 1:nrow(hidi)) {
    rgl::segments3d(c(-maxr-1.5,-maxr-2.5),c(x,y),hidi[i,1],lwd=3, col="black")
  if ( i == 1) {
    rgl::text3d(-maxr-4,0,hidi[i,1],paste(hidi[i,1], "m"),font=2, col="black")} else {
      rgl::text3d(-maxr-4,0,hidi[i,1],paste(hidi[i,1]),font=2, col="black")
     }
   }
  }
  if (box==TRUE) {rgl::box3d(col="black")}

}



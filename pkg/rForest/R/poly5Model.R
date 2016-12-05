#'Fitting a fifth-degree polynomial taper model
#' 
#'@description poly5Model is used to fit a fifth-degree polynomial taper model
#'@usage poly5Model(dbh,ht,di,hi, plotxy)
#'@param dbh, vector of diameter at breast height
#'@param ht, vector of measured tree heights
#'@param di, vector of measured tree diameters at i heights
#'@param hi, vector of measured tree i heights
#'@param plotxy, plot the fitted model
#'@return Returns a fifth-degree polynomial taper model as an object of class "lm"
#'@author Carlos A. Silva, Samuel P. C. Carvalho, Carine Klauberg Silva and Manoela de O. Rosa 
#'@references
#'Schoepfer (1966) model :fifth-degree polynomial taper model 
#'\itemize{
#'   \eqn{di/dbh= (hi/ht) + (hi/ht)^2 + (hi/ht)^3 + (hi/ht)^4 + (hi/ht)^5}\cr
#'}
#'@examples
#'# Importing forest inventory data
#'fpath<-system.file("extdata", "taper.csv", package="rForest") 
#'TreeList<-read.table(fpath, sep=",", head=T) # reading data
#'
#'# setting model parametersdbh and ht
#'ht<-TreeList[,6]
#'dbh<-TreeList[,7]
#'di<-TreeList[,4]/pi
#'hi<-TreeList[,3]
#'plot(hi/ht, di/dbh)
#'
#'# fitting the fifth-degree polynomial taper model
#'fit <- poly5Model(dbh,ht,di,hi, plotxy=TRUE)
#'#grid()
#'@export
poly5Model<-function(dbh,ht,di,hi, plotxy=TRUE) {
  x<-hi/ht
  lmp<-lm(I(di/dbh) ~ I(x) + I((x)^2) + I((x)^3) + I((x)^4) + I((x)^5))
  
  if (plotxy) {
  plot(hi/ht, di/dbh)
  curve(lmp$coefficients[1]+lmp$coefficients[2]*x+lmp$coefficients[3]*x^2
        +lmp$coefficients[4]*x^3+lmp$coefficients[5]*x^4+lmp$coefficients[6]*x^5, 
        add=T,col='red', lty=2)
        grid()}
  return(list(model=lmp,fit=lmp$fitted))
}

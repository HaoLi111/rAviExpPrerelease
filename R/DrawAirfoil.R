#DrawAirfoil
#load airfoil
loadAirfoil<-function(airfoil){
  af<-read.table(paste(getwd(),'/Profiles/',airfoil,'.dat',sep =''),header = F)
  return(af)
}

drawAirfoil<-function(airfoil,points=F){
  if(points == F){
    plot(airfoil[,1],airfoil[,2],type = "l",asp = 1,xlab="Relative X axis",
                      ylab = "Relative Y axis",main = deparse(substitute(airfoil)))
  }else{
    points(airfoil[,1],airfoil[,2],type = "l")
    }

}

zoomAirfoil<-function(airfoil,n) airfoil*n


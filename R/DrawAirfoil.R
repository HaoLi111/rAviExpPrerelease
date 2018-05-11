#DrawAirfoil
#load airfoil
loadAirfoil<-function(airfoil){
  af<-read.table(paste(getwd(),'/Profiles/',airfoil,'.dat',sep =''),header = F)
  return(af)
}

drawAirfoil<-function(airfoil){
  plot(airfoil[,1],airfoil[,2],type = "l",asp = 1,xlab="Relative X axis",
       ylab = "Relative Y axis",main = deparse(substitute(airfoil)))
}

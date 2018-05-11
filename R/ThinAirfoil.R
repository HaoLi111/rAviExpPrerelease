#Foil Cy and Cx calculation by thin airfoil theory.
#https://en.wikipedia.org/wiki/Airfoil#Thin_airfoil_theory
Cl_thin<-function(alpha) return(alpha/90*9.869604)

#https://www.grc.nasa.gov/www/k-12/airplane/induced.html
AR1<-function(span, area) span*span/area
AR<-function(span,cord) span/cord

#Osward efficiency coefficient- if I spelled it right.
e_type<-list(elliptical = 1,
             rectangular = .7)#do not write this as a find function!!!!!
#to use it go e_type$elliptical, if ,as vector, e_type['elliptical'] to get 1(if you remember the data then forget about this list)

e_GAAD_M1<-function(AR) 1.78*(1-.045*AR^0.68)-.64 #*GAAD P64 Method 1 (9.5.14)
Cd_induced<-function(Cl,AR, e) (Cl^2) / (3.141593 * AR * e)
Cdi<-function(alpha,Clfunction = Cl_thin,
              AR = 8,e = 1){
  (Clfunction(alpha)^2) / (3.141593 * AR * e)
}
# AR = span^2/area;area = span * cordAvg_geometric; AR = span/cordAvg_geometric
#Foil Cx Calculation
# (lift induced drag)
# required -  lift induced drag coefficient k

k<-function(AR = 8, e= 1) (3.141593 * AR * e)^(-1)

#Cl correction formulae
#Cd_thin<-function(alpha,Cdf = .05) Cdi(alpha)+Cdf

#createAlpha
createAlpha<-function(alpha = seq(from = -5, to = 10, by = .1),
                      Clfunction = Cl_thin,#Thin airfoil theory
                      Cdfunction = Cdi,#all Cl and Cd witten with respect to Alpha (or set default for other parameters)
                      plot = T,
                      return = T){
  Cl<-Clfunction(alpha)
  Cd<-Cdfunction(alpha)
  alpha<-data.frame(alpha)
  print(Clfunction)
  print(Cdfunction)
  alpha$Cl = Cl
  alpha$Cd = Cd
  alpha$k = (Cl/Cd)
  if(plot == T) plotAlpha(alpha)
  if(return ==T) return(alpha)
}
#Cl_thin(seq(from = -5, to = 10, by = .1))
plotAlpha<-function(alpha){
  layout(matrix(1:4,2))

  plot(alpha$alpha,alpha$Cl,type = 'l',main = "Cl @ alpha")
  plot(alpha$alpha,alpha$Cd,type =  'l',main = "Cd @ alpha")
  plot(alpha$alpha,alpha$k,type = 'l',main = "k (Cl/Cd) (d/h)max @ alpha")
  plot(alpha$Cd,alpha$Cl,type = 'l',main = "Cl ~ Cd")
  layout(matrix(1))
}
#createAlpha(plot = F)
#createAlpha(plot = T)

#specify by airfoil <- createAlpha(...)

#fetchAlpha
#fetchAlpha()
fetchAlpha<-function(airfoil = get('airFoil',envir = .GlobalEnv),aspect = c(T,T,T,T)){
  subset(airfoil,Cd == min(Cd)|k == max(k)|Cl == max(Cl))
}
#fetchAlpha(createAlpha())
fetchAlpha_Clmax<-function(airfoil = get('airFoil',envir = .GlobalEnv),aspect = c(T,T,T,T)){
  subset(airfoil,Cl == max(Cl),aspect)
}
#fetchClmax(createAlpha())
fetchAlpha_Cdmin<-function(airfoil = get('airFoil',envir = .GlobalEnv),aspect = c(T,T,T,T)){
  subset(airfoil,Cd == min(Cd),aspect)
}
#
fetchAlpha_kmax<-function(airfoil = get('airFoil',envir = .GlobalEnv),aspect = c(T,T,T,T)){
  subset(airfoil,k == max(k),aspect)
}

fetchAlpha_Cl<-function(airfoil,Cl){
  DFRead2<-function(df,target,independent = 1,dependent = seq(from = 2, to = ncol(df), by = 1)){
    dfx<-df[,independent]
    dfy<-df[,dependent]
    for (i in 1:(length(dfx)-1)){
      if (target == dfx[i]){

        return(dfy[i,])
      }else if(dfx[i]<target& target<dfx[i+1]){
        m=(dfy[i+1,]-dfy[i,])/(dfx[i+1]-dfx[i])
        return(m*(target-dfx[i])+dfy[i,])

      }
    }
    if(target ==dfx[length(dfx)]) dfy[length(dfx),]
  }
  DFRead2(df = airfoil,target = Cl, independent = 2,dependent = 1:4)
}

#Air

#https://en.wikipedia.org/wiki/Density_of_air
g=9.80665

AIR<-c(p0=101.325e3,#sea level standard atmospheric pressure, 101.325 kPa
       T0=288.15,#sea level standard temperature, 288.15 K
       L =.0065,# temperature lapse rate, 0.0065 K/m
       R = 8.31447,#ideal (universal) gas constant, 8.31447 J/(mol??K)
       Mrair = 0.0289644 #kg/mol#molar mass of dry air, 0.0289644 kg/mol
       )
rhoFromH<-function(H = 0) 101.325e3*(1-H*.0065/288.15)^(9.80665*0.0289644/8.31447/.0065)*0.0289644/8.31447/(288.15-H*0.0065)
completeRho<-function(constraintstate){
  constraintstate$rho<-rhoFromH(constraintstate$H)
  constraintstate
}

q<-function(v,H = 0) .5*rhoFromH(H)*v^2#Potential Energy Density

#For each motion specify v and h extend for q,
completeQ<-function(constraintstate){
	constraintstate$q<-q(constraintstate$v,constraintstate$H)
	constraintstate
}
#completeRho
completeRho<-function(constraintstate){
  constraintstate$rho = rhoFromH(H = constraint$H)
}

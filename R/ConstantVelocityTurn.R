#1 Constraint Analysis


#T/W for a level constant velocity turn
CA_TURN2<-function(W_S=get("W_S"),
  Cd_min = get("CA$Cd_min"),
                  q=get("q_default"),
                  k=get("k_default"),
                  n=get("n_default")
){
  T_W = q*(Cd_min/W_S+k*(n/q)^2*(W_S))
  return(T_W)
}

CA_TURN<-function(requirement){
  frame<-within({
    turn = CA_TURN2()
  })
}

turn.template = list(Cdmin = NA,
                     v = NA,
                     H = NA,
                     k = NA,
                     n = NA,
                     q =NA)
CA_TURN<-function(W_S,constraint ) CA_TURN2()
#Cd_min = minimewm drag coefficient
#k = lift-induced drag constant
#q = dynamic pressure at the selected airspeed and altitude (lb /ft 2 or N/m 2 f )
#n = load factor = 1 / cos f --f bank angle

#Gudmundsson, Snorri. General Aviation Aircraft Design : Applied Methods and Procedures, Elsevier Science & Technology, 2013. ProQuest Ebook Central, http://ebookcentral.proquest.com/lib/canterbury/detail.action?docID=1377690.
#Created from canterbury on 2018-04-05 16:01:04.






#-----------------------------------------------------------------
#2 T/W for a desired specific energy level
#CA_ENERGY_LEVEL

CA_ENERGY_LEVEL2<-function(W_S,
                          q,
                          Cd_min=.01,
                          k,
                          n,
                          Ps,
                          v){
  T_W = q* (Cd_min/(W_S) + k * (n/q)^2 * (W_S)) + Ps/v
  T_W
}
#q = dynamic pressure at the selected airspeed and altitude P
#S = specific energy level at the condition
#v = airspeed
#Gudmundsson, Snorri. General Aviation Aircraft Design : Applied Methods and Procedures, Elsevier Science & Technology, 2013. ProQuest Ebook Central, http://ebookcentral.proquest.com/lib/canterbury/detail.action?docID=1377690.
#Created from canterbury on 2018-04-05 16:07:27.
energy_level.template = list(Cd_min = .01,
                             k = NA,
                             n = NA,
                             Ps = NA,
                             v = NA,
                             H = NA,
                             q = NA)


#-----------------------------------------------------------------
#3 T/W for a Desired Rate of Climb
CA_CLIMB<-function(W_S,
                   v_v,
                   v,
                   k,
                   q,
                   Cd_min = .01){
  T_W = v_v/v + q/W_S*Cd_min + k/q*W_S
  T_W
}
#q= dynamic pressure at the selected airspeed and altitude
#v = airspeed
#v v = vertical speed
#Gudmundsson, Snorri. General Aviation Aircraft Design : Applied Methods and Procedures, Elsevier Science & Technology, 2013. ProQuest Ebook Central, http://ebookcentral.proquest.com/lib/canterbury/detail.action?docID=1377690.
#Created from canterbury on 2018-04-05 16:21:26.
climb.template<-list(v_v = NA,
                     v = NA,
                     k = NA,
                     Cd_min = NA,
                     H = NA,
                     q = NA)





#-----------------------------------------------------------------
#4 T/W for a Desired Cruise Airspeed
#
CA_CRUISE_V2<-function(W_S,
                      Cd_min = .01,
                      q,
                      k){
  T_W = q*Cd_min*(1/W_S) + k * (1/q) * (W_S)
  T_W
}
#q = dynamic pressure at the selected airspeed and altitude
#S = wing area
cruise_v.template = list(H = NA,
                         v = NA,
                         q = NA)

#-----------------------------------------------------------------
#5 T/W For a Service Ceiling( ROC  = 100 fpm or .508 ms^-1)
# CA_SERVICE_CEILING
CA_SERVICE_CEILING2<-function(W_S,
                             v_v,
                             Cd_min,
                             H,
                             rho,
                             k
                             ){
  T_W = v_v/sqrt(2/rho * W_S * sqrt(k/3/Cd_min) ) + 4 * sqrt(k*Cd_min/3)
  T_W
}

service_ceiling.template<-list(v_v = NA,
                               rho = NA)

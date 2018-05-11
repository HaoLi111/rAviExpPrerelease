cord_avg_trap<-function(root,tip) sqrt(tip^2+root^2)/sqrt(2)
cord_avg_trap_geo<-function(root,tip) (root+tip)/2
sweep_prop<-function(rtcord,tpcord,span,sweep,p) atan((span*tan(sweep*pi/180)-(rtcord-tpcord)*p)/span)*180/pi
area_trap<-function(a,b,h) (a+b)*h/2
yF_trap<-function(root,tip,span) span/2*(root-sqrt(tip^2+root^2)/sqrt(2))/(root-tip)
xF_trap<-function(root,tip,span,sweep) sind(sweep)*span/2*(root-sqrt(tip^2+root^2)/sqrt(2))/(root-tip)+sqrt(tip^2+root^2)/sqrt(2)/4

S_M_<-function(X_np,X_cg,cord) (X_np-X_cg)/cord

N_P_1<-function(AR_main,AR_horz,V_horz,cord) ((4*AR_main-8)*AR_horz*cord*V_horz+(AR_main*AR_Horz+2*AR_main)*cord)/(4*AR_main*AR_horz+8*AR_main)
N_P__<-function(AR_main,AR_horz,V_horz,cord)  .25+ (1+2/AR_main)/(1+2/AR_horz) * (1 - 4/(AR_main + 2)) * V_horz
N_P_<-function(AR_main,AR_horz,V_horz,cord,model=1) ifelse(model==1,N_P_1(AR_main,AR_horz,V_horz,cord),NULL)


#Volume Coefficient
Vh=function(S_horz,L_horz,S_main,cord)  S_horz*L_horz/S_main/cord
Vv=function(S_vert,L_vert,S_main,span) S_vert*L_vert/S_main/span
#Douglas factor
B=function(L_vert,dihedral,span,Cl)  (L_vert*dihedral)/span/Cl

#Center of Mass
CGM<-function(m){
  s<-sum(m[4,])
  x<-sum(m[4,]*m[1,])/s
  y<-sum(m[4,]*m[2,])/s
  z<-sum(m[4,]*m[3,])/s
  list(x=x,y=y,z=z,FG_M = s)
}
#roll_authority<-function(Vcv,B) Vcv*B
#


Stability_conventional1<-function(concept,load = c(0,0,0,0),state = list(Cl = .8)){
  #M---------------------------------------------------
  Main_xF<-xF_trap(concept$WM$CordR,concept$WM$CordT,
                   span = concept$WM$Span,
                   sweep = concept$WM$Sweep)
  Main_yF<-yF_trap(concept$WM$CordR,concept$WM$CordT,#Aerodynamic focus on the wing component
                   span = concept$WM$Span)
  Main_x<-concept$WM$x+Main_xF#Aerodynamic focus on the plane = estimate CG of wing component = point of lift
  Main_C<-cord_avg_trap(root = concept$WM$CordR,concept$WM$CordT)#dynamic avereage cord
  Main_S<-area_trap(a = concept$WM$CordR,b = concept$WM$CordT,h = concept$WM$Span)#Area


  #H---------------------------------------------------
  Horz_xF<-xF_trap(concept$WH$CordR,concept$WH$CordT,
                   span = concept$WH$Span,
                   sweep = concept$WH$Sweep)
  Horz_yF<-yF_trap(concept$WH$CordR,concept$WH$CordT,
                   span = concept$WH$Span)
  Horz_x <- Horz_xF + concept$WH$x

  Horz_C<-cord_avg_trap(concept$WH$CordR,concept$WH$CordT)
  Horz_S<-area_trap(a = concept$WH$CordR,b = concept$WH$CordT, h =Horz_C)
  Vert_xF<-xF_trap(concept$WV$CordR,concept$WV$CordT,
                   span = concept$WV$Span,
                   sweep = concept$WV$Sweep)
  Vert_zF<-yF_trap(concept$WH$CordR,concept$WV$CordT,
                   span = concept$WV$Span)
  Vert_x <- Vert_xF + concept$WV$x

  Vert_C<-cord_avg_trap(concept$WV$CordR, concept$WV$CordT)
  Vert_S<-area_trap(a = concept$WV$CordR,b = concept$WV$CordT, h = concept$WV$Span)
  Vert_z<-Vert_zF + concept$WV$z


  #CM---------------------------------------------------
  CM<-CGM(cbind(c(Main_x,0,concept$WM$z,concept$WM$m),
                c(Horz_x,0,concept$WH$z,concept$WH$m),
                c(Vert_x,0,concept$WV$z,concept$WV$m),
                c(concept$fuselage$x,concept$fuselage$y,concept$fuselage$z,concept$fuselage$m),
                load))#Assume CG of components  =  Aerodynamic Focus of components
  Horz_L<-Horz_x- CM$x
  Vert_L<-Vert_x - CM$x
  #AF--------------------------------------------------
  AF<-list(main  =list(Center = c(x = Main_xF,y = Main_yF),CenterAbs = c(x = Main_x,y = 0,z =concept$WM$z),C = Main_C,S = Main_S),
           horz = list(Center = c(x = Horz_xF,y = Horz_yF),CenterAbs=  c(x = Horz_x,y = 0,z = concept$WH$z),C = Horz_C,S = Horz_S,L = Horz_L),
           vert = list(Center = c(x = Vert_xF,z = Vert_zF),CenterAbs= c(x = Vert_x,y = 0,z = Vert_z)),C = Vert_C,S = Vert_S,L = Vert_L)
  #NP---------------------------------------------------
  NP<-N_P__(AR_main = 2*concept$WM$Span/(concept$WM$CordR + concept$WM$CordT),
            AR_horz = 2*concept$WH$Span/(concept$WH$CordR + concept$WH$CordT),
            V_horz = Vh(S_horz = Horz_S,
                        L_horz = Horz_L,
                        S_main = Main_S,
                        cord = Main_C),cord = Main_C)
  NP = NP + concept$WM$x
  #Sizing
  Sizing<-list(Vh = Vh(S_horz = Horz_S,
                       L_horz = Horz_L,
                       S_main = Main_S,cord = Main_C),
    Vv = Vv(S_vert = Vert_S,
            L_vert = Vert_L,
            S_main = Main_S,
             span = concept$WM$Span),
    B = B(dihedral = concept$WM$Dihedral,L_vert = Vert_L,span = concept$WM$Span,state$Cl)
    #dVvB_dCl
    )

  Sizing$VvB = Sizing$Vv*Sizing$B

  #Stage 2 check
  #---------------------------------------
  #5 staCheck(StabilityMargin) check
  staCheck<-function(staMargin){
    if(staMargin==0){
      re="neutral"
    }else if(staMargin>0){
      if(staMargin>=0.4){
        re="strongly stable"
      }else if(staMargin>0.05){
        re="stable"
      }else{
        re="weakly stable"
      }
    }else{
      if(staMargin<=-0.4){
        re="strongly unstable"
      }else if(staMargin<-0.05){
        re="unstable"
      }else{
        re="weakly unstable"
      }
    }
    paste(staMargin,re," - Stability Margin(-.4,-.05,0,.05,4)")
  }

  #6 VhCheck-check how the volume coefficient of horizontal tail
  #is compared to a 'normal' plane
  #A "Normal" plane has a Vh in between 0.30 and 0.60
  VhCheck=function(Vh){
    if (Vh>0.60){
      re='Large'
    }else if(Vh>=0.30){
      re="Normal"
    }else{
      re='small'
    }
    paste(Vh,re,"horizontal volume coefficient(.30,.60)")
  }
  #7 VvCheck
  VvCheck=function(Vv){
    if(Vv>0.05){
      re='Large'
    }else if (Vv>=0.02){
      re='Normal'
    }else{
      re="Small"
    }
    paste(Vv,re,"vertical volume coefficient(.02,.05)")
  }

  #8 Roll Authority Check
  VvBCheck=function(VvB){
    if (VvB>0.20){
      re="Large"
    }else if (VvB>=0.10){
      re="Normal"
    }else{
      re="Small"
    }
    paste(VvB,re,"Roll Authority(.10,.20)")
  }
  BCheck<-function(B){
    if(B>5){
      re = 'Stable'
    }else if(B==5){
     re = 'Neutral'
    }else{
      re = 'Unstable'
    }
    paste(B,'-Spirally',re,'(5)')
  }


  Balance = list(NP = NP,SM =S_M_(X_np = NP,X_cg = CM$x,cord = Main_C))
  message(paste('X.NP =',NP,'X.CG =',CM$x))
  message(staCheck(Balance$SM))
  message(VvCheck(Sizing$Vv))
  message(VhCheck(Sizing$Vh))
  message(BCheck(Sizing$B))
  message(VvBCheck(Sizing$VvB))
  AutoCheck = list(staCheck(Balance$SM),
                   VvCheck(Sizing$Vv),
                   BCheck(Sizing$B),
                   VvBCheck(Sizing$VvB))
  list(CM = CM,AF = AF,Balance = Balance,Sizing = Sizing, State = state)
}

checkStability = function(concept,load,state = list(Cl = .8)){
  #sta
}

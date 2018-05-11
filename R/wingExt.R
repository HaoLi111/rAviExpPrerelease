#create list
#main<-list(CordR = 300, CordT = 10,sweep = 25, profile = "Clark-Y",span = 600)
#main

wingExt_planar<-function(wing){

  wing$area<-(wing$CordT+wing$CordR)*wing$span/2            #AREA
  wing$area_half<-(wing$area/2)                            #-/2
  return(wing)
}

#main2<-wingExt_planar(main)
#main2
#View(main2)

wingExt_Properties_planar<-function(wing){
  within(wing,{
    focus_dyn_x<-xF_trap(root = CordR,tip = CordT, sweep = sweep)
    focus_dyn_y<-yF_trap(root = cord,tip = tip)


  })
}

wingExt<-function(wing){
  message('Wing Extention for conventional layout')
  message(wing)
  wing$CordAvgGeometric<-(wing$CordR+wing$CordT)/2
  wing$CordAvgAerodynamic<-cord_avg_trap(root = wing$CordR,        #DYNAMIC CORD OF AVERAGE FOR TRAPEZOID WINGS
                               tip = wing$CordT)
  wing$SpanHalf<-wing$Span/2
  wing$SweepQualter<-sweep_prop(rtcord = wing$CordR,    #SWEEP AT QUARTER CORD
                                 tpcord = wing$CordT,
                                 p=.25,
                                 span = wing$SpanHalf,
                                 sweep = wing$Sweep)
  wing$SweepHalf<-sweep_prop(rtcord = wing$CordR,       #SWEEP AT HALF CORD
                              tpcord = wing$CordT,
                              p=.5,
                              span = wing$SpanHalf,
                              sweep = wing$Sweep)
  wing$SweepEnd<-sweep_prop(rtcord = wing$CordR,       #SWEEP AT HALF CORD
                              tpcord = wing$CordT,
                              p=1,
                              span = wing$SpanHalf,
                              sweep = wing$Sweep)
  wing$Lamda<-wing$CordT/wing$CordR                      #tip/root
  wing$Area<-(wing$CordT+wing$CordR)*wing$Span/2            #AREA
  wing$Area_half<-(wing$Area/2)                            #-/2
  return(wing)
}

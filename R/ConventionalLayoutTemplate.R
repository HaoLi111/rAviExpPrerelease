#Templates for the components of a design
#Empty Template
#Wing - consist of basic geometric properties of a wing which can be filled to a more compoicated version
#(WingExt) by calculation(of simple trapezoid wing with some dihedral) or the user can also directly name these
#extension properties of the wing in  the Ext version
#it is safer to check
#1.the extension list does not corrupt with the simpler wing model
#2.when extending (may overwite to the existing customized data)or extracting(may lose details if not for the simplified model), the calculation model is applicable for the specific plane.
Wing.template<-list(m = NA,
                    x = NA, y = NA, z = NA,
                    CordR=NA,CordT=NA,Span=NA,Sweep=NA,
                    Foil=NA,
                    Dihedral=NA)

# Wing
#   |
#   |wingExt()
#   |
#   |
#  \|/
# WingExt
WingExt.template=list(CordR=NA,
                      CordT=NA,
                      CordAvgGeometric=NA,#
                      CordAvgAerodynamic=NA,#
                      Span=NA,
                      SpanHalf=NA,#
                      SweepFront=NA,
                      SweepQualter=NA,#
                      SweepHalf=NA,#
                      SweepEnd=NA,#WORK OF FUNCTION DONE SO FAR!!!!!!#$#$@$%@#@%
                      SweepThickest=NA,#
                      Foil=NA,
                      FoilProportionThickest = NA,#
                      FoilAlphaCl0=NA,#
                      FoilAlphaCd0=NA,#
                      FoilAlphaCla=NA,#
                      WingAlphaCl0=NA,#
                      WingAlphaCd0=NA,#
                      WingAlphaCla=NA,#
                      Dihedral = NA,
                      AR = NA
)

#Specify WM/WV/WH<-Wing/WingExt
#fuselage.template
fuselage.template=list(Xload = NA,
                       Yload = NA,
                       mload = NA,
                       mPP = NA,#m power Plant
                       P = NA,
                       E = NA,
                       EfficiencyEstimation = NA,
                       EfficiencyEquation = NA,
                       FT = NA,
                       Diammeter=NA
)

#new_fuselage<-function(){
 # fuselage=getAnywhere('fuselage.template')
  #edit(fuselage)
#}
#Specify fuselage<-fuselage
#Concept
#Concept<-list(fuselage,
#             WM,
#            WH,
#           WV)

Concept.template<-list(fuselage = fuselage.template,
                       WM = Wing.template,
                       WH= Wing.template,
                       WV = Wing.template)




save_concept<-function(name = "my_concept",concept=my_concept){
  write.csv(as.data.frame(as.vector(concept$fuselage)),paste(name,"fuselage.csv",sep = "_"))
  write.csv(as.data.frame(as.vector(concept$WM)),paste(name,"WM.csv",sep="_"))
  write.csv(as.data.frame(as.vector(concept$WH)),paste(name,"WH.csv",sep="_"))
  write.csv(as.data.frame(as.vector(concept$WV)),paste(name,"WV.csv",sep="_"))
  print(paste("File saved to",getwd()))
}



load_concept<-function(name = "my_concept"){
  fuselage<-as.list(as.data.frame(read.csv(paste(name,"fuselage.csv",sep = "_"),header = T)))
  WM<-as.list(as.data.frame(read.csv(paste(name,"WM.csv",sep="_"),header = T)))
  WH<-as.list(as.data.frame(read.csv(paste(name,"WH.csv",sep="_"),header = T)))
  WV<-as.list(as.data.frame(read.csv(paste(name,"WV.csv",sep="_"),header = T)))
  list(fuselage = fuselage,WM = WM,WH= WH,WV=WV)
}

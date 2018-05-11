#Constraint Data Manipulations functions

#createConstraint
createConstraint<-function(W_S = get("W_S_default"),
                           constraint = get("constraint"),
                           state = c("CA_TURN",
                                     "CA_ENERGY_LEVEL",
                                     "CA_CLIMB",
                                     "CA_T_O_DISTANCE",
                                     "CA_CRUISE_V",
                                     "CA_SERVICE_CEILING",
                                     "CA_CL_MAX_STALL",
                                     "CA_TAKEOFF",
                                     "CA_LANDING")){
  W_S<-data.frame(W_S)
  #print(constraint)
  for(func in state){
    W_S<-within(W_S,{
      assign(func,sapply(W_S,get(func)))
    })
  }
  return(W_S)
}







#plotConstrait
#Given a dataset with the 1st column as W_S, other as the other valiables (T/F_G)
#Plot the Constraint Diagram with legend
plotConstraint<-function(data,target = "T/F_G",
                         legendlocation = "top"){
  with(data,{
    matplot(data[,-1],type = 'l',xlab = "W/S",ylab = target,lty = 1)
    legend(legendlocation,colnames(data[,-1]),col = 2:length(ncol(data)),cex = 1,fill = 2:length(ncol(data)))
  })
}


#optimConstraint
#find the absolute minimum (numerical) value of the target variable of the constraint analysis
#(T/F_G) in the feasible region

optimConstraint<-function(data){
  restrictMax<-apply(data[,-1],1,max)
  Min=min(restrictMax)
  W_D_optim<-data[which(restrictMax == Min),1]
  return(c(Min = Min, W_D_optim = W_D_optim))
}





locateConstraint<-function(data){
  plotConstraint(data)
  optimC<-optimConstraint(data)
  abline(v = optimC["W_D_optim"])
  abline(h = optimC["Min"])
  return(optimC)
}

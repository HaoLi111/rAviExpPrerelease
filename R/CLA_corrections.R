ClA_correction_trapsimp<-function(AR,sweep_quarter,
                                 ClA,Ma){
  2*pi*AR/(2+sqrt(4+(AR/(ClA/2*pi))^2*((1-Ma^2)+tan(sweep_quarter)^2)))
}
#require(Ryacas)
#PrettyForm.Func(ClA_correction_trapsimp)
#TeXForm(expression(2*pi*AR/(2+sqrt(4+(AR/(ClA/2*Pi))^2*((1-Ma^2)+tan(sweepQuarter)^2)))))

ClA_correction_trapsimp_ext<-function(wing = getAnywhere('wing')){
	wing$ClA_correction<-ClA_correction_trapsimp(wingExt$AR,
	                                             wing$SweepQuarter)
}

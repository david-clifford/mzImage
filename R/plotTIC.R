plotTIC <-
function(obj,log="y",...) {
  plot(obj$AllMass,obj$TIC,type="l",xlab="Mass (mz)",ylab="Total Intensity",log=log,...)
}


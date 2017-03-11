plotSNR <-
function(obj,...) {
  plot(obj$AllMass,obj$SNR,type="l",xlab="Mass (mz)",ylab="SNR",las=1,...)
}


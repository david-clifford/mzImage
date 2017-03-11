BGcorrect <-
function(obj) {
  knots <- c(999,2000,3000,4000,5000)
  knots <- knots[knots>min(obj$AllMass) & knots<max(obj$AllMass)]
  basis <- bs(obj$AllMass,knots=knots)
  
  standardise <- function(vect) (vect - mean(vect)) / sqrt(var(vect))
  X <- cbind(1,basis[,-c(1,5,6)],standardise(obj$AllMass))
  
  ff <- function(vect) {
    ind <- which(!is.na(vect))

    ## rlm is weird - it looks like rlm(y~X-1) should work here and avoid the 'x' is singular error
    mod.huber <- rlm(log(vect)~X-1) ## formula method
    ##mod.huber <- rlm(X,log(vect),weights=rep(1,length(vect))) ## default method (avoids singular error but predict doesn't work)
    
    yHat <- predict.lm(mod.huber)
    ret <- rep(NA,length(vect))
    ret[ind] <- log(vect[ind])-yHat
    ret
  }

  data <- t(apply(obj$data,1,function(x) tryCatch(ff(x),error=function(e) rep(NA,length(x)))))
  ret <- list(AllMass=obj$AllMass,data=data,X=obj$X,Y=obj$Y)
  ret
}


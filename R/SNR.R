SNRobj <- function(obj, type=c("Linear","Quadratic")) {
    type <- match.arg(type,c("Linear","Quadratic"))
    return(apply(obj$data,MARGIN=2,FUN=SNR,x=obj$X,y=obj$Y,type=type))
}

SNR <- function(stat,x,y,type=c("Linear","Quadratic")) {

  xx <- sort(unique(x))
  yy <- sort(unique(y))
  type <- match.arg(type,c("Linear","Quadratic"))

  z <- matrix(NA, nrow=max(xx)-min(xx)+1, ncol=max(yy)-min(yy)+1)
  z[cbind(x-min(xx)+1, y-min(yy)+1)] <- stat

  v1 <- var(c(z), na.rm=TRUE)

  if(type=="Linear") signal <- nearestNeighbourMean(z)
  if(type=="Quadratic") signal <- nearestNeighbourQuadratic(z)

  noise <- z - signal

  ## old version of SNR
  v2 <- 4/5*mean(noise^2,na.rm=TRUE)
  ret <- v1/v2 - 1

  ## new version of SNR to line up with Generalised Eigen Value and MNF work
  v2 <- var(c(noise),na.rm=TRUE)
  ret <- v1 / v2

  ## NOTE:
  ## Proper version of SNR to line up with Marks papers would be
  ## ret <- ret - 1 in which case SNR of MNF bands are eigenvalues - 1

  ## Actual proper definition - which lines up with earlier one when
  ## actual variances are known, and zero-correlation assumption
  ## holds. We prefer the method above to line up with SNR values for
  ## MNF bands = eigenvalue-1

  ## v1 <- var(c(signal),na.rm=TRUE)
  ## v2 <- var(c(noise),na.rm=TRUE)
  ## ret <- v1 / v2

  return(ret)
}


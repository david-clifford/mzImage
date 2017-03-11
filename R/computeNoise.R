computeNoise <-
function(stat,x,y,type=c("Linear","Quadratic")) {
  xx <- sort(unique(x))
  yy <- sort(unique(y))
  type <- match.arg(type,c("Linear","Quadratic"))
  z <- matrix(NA, nrow=max(xx)-min(xx)+1, ncol=max(yy)-min(yy)+1)
  z[cbind(x-min(xx)+1, y-min(yy)+1)] <- stat
  v1 <- var(c(z), na.rm=TRUE)
  if(type=="Linear") mtx <- nearestNeighbourMean(z)
  if(type=="Quadratic") mtx <- nearestNeighbourQuadratic(z)
  na.omit(as.vector(z-mtx))
}


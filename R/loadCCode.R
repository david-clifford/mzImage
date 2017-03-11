nearestNeighbourMean <- function(x) {

  ## Add rows and columns of NAs - in C this makes dealing with edge effects easier
  x <- cbind(NA,x,NA)
  x <- rbind(NA,x,NA)

  nrows <- nrow(x)
  ncols <- ncol(x)

  pp <- .C("nearestNeighbourMean",
           x=as.double(as.vector(x)),
           nrows=as.integer(nrows),
           ncols=as.integer(ncols),
           eY=double(nrows*ncols),
           dup=FALSE,NAOK=TRUE)
  res <- matrix(pp$eY,nrows,ncols)
  res[which(is.na(x))] <- NA
  res[which(is.nan(res))] <- NA
  res <- res[-c(1,nrows),]
  res <- res[,-c(1,ncols)]
  res
}

nearestNeighbourQuadratic <- function(x) {

  ## Add rows and columns of NAs - in C this makes dealing with edge effects easier
  x <- cbind(NA,x,NA)
  x <- rbind(NA,x,NA)

  nrows <- nrow(x)
  ncols <- ncol(x)

  pp <- .C("nearestNeighbourQuadratic",
           x=as.double(as.vector(x)),
           nrows=as.integer(nrows),
           ncols=as.integer(ncols),
           eY=double(nrows*ncols),
           dup=FALSE,NAOK=TRUE)
  res <- matrix(pp$eY,nrows,ncols)
  res[which(is.na(x))] <- NA
  res[which(is.nan(res))] <- NA
  res <- res[-c(1,nrows),]
  res <- res[,-c(1,ncols)]
  res
}

dilation <- function(y,span)
  {
    nmes <- names(y)
    res <- .C("dilation",
              y = as.double(y),
              nym = as.integer(length(y)),
              sp = as.integer(span),
              ey = double(length(y)),
              dup=FALSE,NAOK=TRUE)$ey
    names(res) <- nmes
    res
  }

peaks <- function(y,span)
  {
    return(y==pmax(dilation(y,span),rev(dilation(rev(y),span))))
  }

LapackGenEigen <- function(A,B,IL=1,IU=3) {
  W <- double(nrow(A))
  Z <- double((IU-IL+1)*nrow(A))
  info <- integer(1)
  pp <- .C("LapackGenEigen",
           A=as.double(as.vector(A)),
           B=as.double(as.vector(B)),
           W=as.double(as.vector(W)),
           Z=as.double(as.vector(Z)),
           IL=as.integer(IL),
           IU=as.integer(IU),
           nrows=as.integer(nrow(A)),
           INFO=as.integer(info),
           dup=FALSE,
           NAOK=FALSE)
  info <- pp$INFO
  values <- pp$W[1:(IU-IL+1)]
  vectors <- matrix(pp$Z,nrow(A),IU-IL+1)
  return(list(values=values,vectors=vectors,info=info))
}

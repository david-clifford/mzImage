computeMNF <-
function(obj,ind=NULL,n=12,smooth=NULL,iter=TRUE) {

  ## iter=TRUE - allow it to go again if a particular kind of error is found
  
  ## How to subset what I am doing the MNF on - when SNR > threshold,
  ## or top XXX SNR values?

  ## How to decide the value for n? 

  ## Be careful - specifc MNF means you are going with the default (or
  ## specified ind) - these should be based on the same object

  ##if(is.null(ind)) ind <- which(obj$SNR>2)
  ##if(is.null(ind)) ind <- which(log(obj$SNR)>1)
  ##if(is.null(ind)) ind <- order(obj$SNR,decreasing=TRUE)[1:1000]
  if(is.null(ind)) ind <- which(obj$SNR>1.25)

  ## Take care the the number of mz-values is not longer than the
  ## number of pixels - go up to 90% and see what happens
  if(length(ind)>length(obj$X)) {
    ind <- order(obj$SNR,decreasing=TRUE)
    ind <- ind[1:floor(0.25*length(obj$X))]
  }  
  
  ##ind <- c(ind,ind+1)
  ##ind <- c(ind,ind-1)
  ##ind <- c(ind,ind+2)
  ##ind <- c(ind,ind-2)
  ##ind <-  sort(unique(ind))
  length(ind)
  
  A <- cov(obj$data[,ind])   ## data covariance matrix
  dim(A)
  sum(is.na(A))
  B <- cov(obj$noise[,ind]) ## noise covariance matrix
  dim(B)
  sum(is.na(B))
  
  if(!is.null(smooth)) {
    D <- matrix(0,ncol(obj$data)-1,ncol(obj$data)-1)
    diag(D) <- 1
    D <- cbind(D,rep(0,nrow(D)))
    D <- rbind(rep(0,ncol(D)),D)
    D <- D + t(D)
    diag(D) <- -2
    
    D <- D[ind,ind]
    diag(D) <- diag(D) - apply(D,1,sum)
    A <- A - smooth * t(D %*% D)
  }
  
  MNF <- LapackGenEigen(A,B,nrow(A)-n+1,nrow(A))

  if(MNF$info > nrow(A) & iter) {

    cat("Trying once again with",MNF$info - nrow(A)-1,"instead of",nrow(A),"peaks\n")
    
    ## if INFO = N + i, for 1 <= i <= N, then the leading minor of order i of B is not positive definite. The factorization of B could not be completed and no eigenvalues or eigenvectors were computed.
    MNF$info - nrow(A) - 1## Choose this many peaks instead at most
    ind <- order(obj$SNR,decreasing=TRUE)[1:(MNF$info - nrow(A)-1)]

    MNF <- computeMNF(obj,ind=ind,n=n,smooth=smooth,iter=FALSE) ## but don't let it get into an infinite loop
  }

  MNF$ind <- ind
  
  MNF
}


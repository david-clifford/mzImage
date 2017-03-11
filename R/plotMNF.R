plotMNF <-
function(obj,ind=NULL,n=12,new=TRUE,smooth=NULL,mfrow=NULL,MNF=NULL,Main=TRUE,unit=2,...) {

  ## How to subset what I am doing the MNF on - when SNR > threshold,
  ## or top XXX SNR values?

  ## How to decide the value for n? 

  ## Be careful - specifc MNF means you are going with the default (or
  ## specified ind) - these should be based on the same object

  ##if(is.null(ind)) ind <- which(obj$SNR>2)
  ##if(is.null(ind)) ind <- which(log(obj$SNR)>1)
  ##if(is.null(ind)) ind <- order(obj$SNR,decreasing=TRUE)[1:1000]
  if(is.null(ind)) ind <- obj$MNF$ind
  if(is.null(ind)) ind <- which(obj$SNR>1.25)
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
  
  if(is.null(MNF)) {
    MNF <- obj$MNF
  }

  if(new) {
    ln <- n ## number to plot - could be less than number in MNF
    if(is.null(mfrow)) {
      mfrow <- c(ceiling(ln/(ceiling(sqrt(ln)))),ceiling(sqrt(ln)))
      mfrow <- c(floor(sqrt(ln)),ceiling(ln/(floor(sqrt(ln)))))
    }
    ww <- unit 
    hh <- unit * diff(range(obj$Y))/diff(range(obj$X))
    
    graphics.off()
    x11(width=ww*mfrow[2],height=hh*mfrow[1],bg="white")
    
    par(mfrow=mfrow)
    par(mar=c(1,1,1,1)+0.1)
  }
  for(ii in seq(ncol(MNF$vectors),len=n,by=-1)) {
    stat <- obj$data[,ind] %*% MNF$vectors[,ii]
    if(!Main) main <- "" else main <- paste("Eigen Value =",signif(MNF$values[ii],3))
    display(stat,obj$X,obj$Y,axes=FALSE,main=main,...)
  }

  MNF$ind <- ind
  invisible(MNF)
}


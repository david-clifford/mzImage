## This code will be one complete example of reading in the raw data,
## looking at it, computing TICs and SNRs and MNFs etc. I will then do
## a package.skeleton at the end to create a package of all the parts.

if(FALSE) {
  tmp <- ReadSet("path to top level directory") ## edit this line
  obj <- toMatrix(tmp)
  rm(tmp)
  
  ## Pre-processing - replace NAs by 0 and add 1, robust regression and log transform
  obj$data[is.na(obj$data)] <- 0
  obj$data <- obj$data + 1
  obj <- BGcorrect(obj)
  
  ## Compute TIC
  obj$TIC <- TIC(obj)
  
  ## plotTIC
  plotTIC(obj,log="")
  
  ## Display intensities at point of highest TIC value
  ss <- which.max(obj$TIC)
  library(colorspace,lib.loc="/home/cli065/Rpackages")
  with(obj,display(data[,ss],X,Y))
  
  ## Compute SNR
  obj$SNR <- SNRobj(obj)
  
  ## plotSNR
  plotSNR(obj)
  
  ## Display intensities with highest SNR
  ss <- which.max(obj$SNR)
  with(obj,display(data[,ss],X,Y))
  
  ## Compute Noise
  obj$noise <- with(obj,apply(data, MARGIN = 2, FUN = computeNoise, x = X, y = Y))
  
  ## Display raw data, spatial smooth and noise at one mz value
  ss <- which.max(obj$SNR)
  par(mfrow=c(1,2))
  with(obj,display(data[,ss],X,Y))
  with(obj,display(noise[,ss],X,Y))
  
  ## Compute MNF transform - first 12 bands computed
  obj$MNF <- computeMNF(obj)
  
  ## plot first 4 bands of MNF
  plotMNF(obj,n=4)

}
 

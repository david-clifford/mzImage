\name{mzImage-package}
\alias{mzImage-package}
\alias{mzImage}
\alias{BGcorrect}
\alias{TIC}
\alias{SNR}
\alias{SNRobj}
\alias{display}
\alias{plotTIC}
\alias{plotSNR}
\alias{computeNoise}
\alias{computeMNF}
\alias{plotMNF}
\alias{LapackGenEigen}
\alias{nearestNeighbourMean}
\alias{nearestNeighbourQuadratic}
\alias{dilation}
\alias{peaks}
\alias{readRaw}
\alias{readOne}
\alias{readAll}
\alias{ReadSet}
\alias{Patch}
\alias{toMatrix}

\docType{package}
\title{What the package does (short line)}

\description{ More about what it does (maybe more than one line)
  A concise (1-5 lines) description of the package}

\details{
\tabular{ll}{
Package: \tab mzImage\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-10-14\cr
License: \tab What license is it under?\cr
LazyLoad: \tab yes\cr
}

An overview of how to use the package, including the most important functions
}

\author{
David Clifford
Maintainer: Who to complain to <David.Clifford@csiro.au>
}

\references{
~~ Literature or other references for background information ~~
}

\keyword{ package }

\seealso{
~~ Optional links to other man pages, e.g. ~~
}

\examples{

if(FALSE) {

  ## Do the following once

  ## Read in raw ims data stored in the subfolders of
  ##   040909_IPAplusminus0.1TFA_CHIP1000/040909_IPA_section_1_2 and
  ##   convert to mzxml and then to RData format in an output directory
  ##   called Data2
  ndir <- "040909_IPAplusminus0.1TFA_CHIP1000"
  tmp <- ReadSet(file.path(ndir,"040909_IPA_section_1_2"), "Data2")
  obj <- toMatrix(tmp)
  rm(tmp)

  ## Save data is R-friendly binary format
  save(obj,file="DataM2.RData")

  ## Start here in future:
  ## clear workspace and load data in matrix format
  rm(list=ls())
  load("DataM2.RData")

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
  library(colorspace)

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

}

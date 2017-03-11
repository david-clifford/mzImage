## Email from Glenn Oct 19th 2010

## The ReadData stuff needs to be run on windows, (or linux with wine)
## and CompassXport installed.

## Because I ran out of memory on some windows machines, ReadSet saves
## the spectra in batches of 100 in a directory "odir"

## Patch stitches these together again - a list of spectra.

## And Matrix converts to the matrix format.

readRaw <- function(fdir, show.output=FALSE) {
  cmd <- paste("CompassXport.exe -multi", fdir, sep=" ")
  if(.Platform$OS.type=="unix") cmd <- paste("wine", cmd, sep=" ")
  if(show.output) system(cmd)
  else system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE)
  tmpf <- list.files(fdir, recursive=TRUE, full.names=TRUE)
  mzfile <- grep("analysis.mzXML", tmpf, ignore.case=TRUE, value=TRUE)
  require(caMassClass)
  tmp <- read.mzXML(mzfile)
  unlink(mzfile)
  return(tmp)
}

readOne <- function(fdir, show.output=FALSE) {
  tmp <- readRaw(fdir, show.output)
  pf <- tmp$parentFile
  XY <- strsplit(sub("^.*0_R00X(...)Y(...).*$", "\\1 \\2", pf)," ")[[1]]
  X <- as.numeric(XY[1])
  Y <- as.numeric(XY[2])
  return(list(X=X, Y=Y, mass=tmp$scan[[1]]$mass, intensity=tmp$scan[[1]]$peak))
}

readAll <- function(tdir, show.output=FALSE, trace=TRUE) {
  res <- list()
  fdirs <- dir(tdir, full.names=TRUE)
  for(i in 1:length(fdirs) ) {
    if(trace) cat("CONVERTING", i, "OF", length(fdirs), "\n")
    res <- c(res, list(readOne(fdirs[i], show.output)))
  }
  return(res)
}

readChunks <- function(tdir, odir, part.size=100, show.output=FALSE, trace=TRUE) {
  res <- list()
  fdirs <- dir(tdir, full.names=TRUE)
  j <- 1
  for(i in 1:length(fdirs) ) {
    if(trace) cat("CONVERTING", i, "OF", length(fdirs), "\n")
    res <- c(res, list(readOne(fdirs[i], show.output)))
    if(i%%part.size==0) {
      save(res, file=file.path(odir, paste("D",j,".RData", sep="")))
      j <- j+1
      res <- list()
      gc()
    }
  }
  if(length(res)>0) save(res, file=file.path(odir, paste("D",j,".RData", sep="")))

  files <- list.files(odir)

  FUN <- function(x) substr(strsplit(x,"\\.")[[1]][1], 2,99)
  jj <- as.numeric(sapply(files, FUN))

  files <- files[order(jj)]
  data.list <- list()
  
  for(ff in files) {
    load(file.path(odir,ff))
    data.list <- c(data.list, res)
  }
  return(data.list)
}

ReadSet <- function(tdir, part.size=0, show.output=FALSE, trace=TRUE) {
  if(part.size>0) {
    odir <- tempdir()
    if(file.exists(odir)) unlink(odir, recursive=TRUE)
    dir.create(odir)
    data.list <- readChunks(tdir, odir, part.size, show.output, trace)
    unlink(odir, recursive=TRUE)
  } else {
    data.list <- readAll(tdir, show.output, trace)
  }
  return(data.list)
}

toMatrix <- function(data.list) {
    X <- sapply(data.list, function(x) x$X)
    Y <- sapply(data.list, function(x) x$Y)

    AllMass <- sort(unique(unlist(sapply(data.list, function(x) x$mass))))

    data.matrix <- matrix(NA, nrow=length(data.list), ncol=length(AllMass))

    for(i in 1:nrow(data.matrix)) {
        tmp <- data.list[[i]]$mass
        if(length(tmp)) {
            wh <- match(tmp, AllMass)
            data.matrix[i,wh] <- data.list[[i]]$intensity
        }
    }

    return(list(X=X,Y=Y,AllMass=AllMass, data=data.matrix))
}


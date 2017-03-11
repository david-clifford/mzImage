display <-
function(stat,X,Y,main=NULL,axes=TRUE) {
  require(colorspace)

  x <- sort(unique(X))
  y <- sort(unique(Y))
  x <- seq(min(x),max(x),by=min(diff(x)))
  y <- seq(min(y),max(y),by=min(diff(y)))
  z <- matrix(NA, nrow=round((max(x)-min(x))/(x[2]-x[1])+1), ncol=round((max(y)-min(y))/(y[2]-y[1])+1))
  z[round(cbind((X-min(x))/(x[2]-x[1])+1, (Y-min(y))/(y[2]-y[1])+1),0)] <- stat

  xlab <- ylab <- ""
  if(axes) {
    xlab <- "X"
    ylab <- "Y"
  }

  cols <- heat_hcl(25)
  cols <- diverge_hcl(25, c = 100, l = c(50, 90), power = 1)
  plot(range(X),range(Y),type="n",axes=axes,main=main,xlab=xlab,ylab=ylab,asp=1)
  image(x,y,z, col=cols,add=TRUE)
  box()
  invisible()
}


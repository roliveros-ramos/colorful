
polar2rect = function(radius, angle, ylim, rmin=0) {
  r = rmin + ((radius - ylim[1,])/diff(ylim))*(1-rmin)
  x = r*cos(pi*angle/180)
  y = r*sin(pi*angle/180)
  return(list(x=x, y=y))
}


.checkYlim = function(ylim, x) {

  n = if(is.matrix(x)) ncol(x) else length(x)

  matrixOK = is.matrix(x) & (NROW(x) > 2)
  if(is.null(ylim) & matrixOK) {
    ylim = apply(x, 2, range, na.rm=TRUE)
  }
  if(!is.matrix(ylim)) {
    if(is.null(ylim)) stop("You must specify 'ylim'.")
    if(length(ylim)!=2) stop("'ylim' must be of length 2.")
    ylim = matrix(ylim, ncol=n, nrow=2)
  }

  if(ncol(ylim)!=n)
    stop("Number of colums of ylim must match the number of columns of x.")

  return(ylim)

}



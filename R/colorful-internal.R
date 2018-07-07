
polar2rect = function(radius, angle, ylim, rmin=0) {
  r = rmin + ((radius - ylim[1,])/diff(ylim))*(1-rmin)
  x = r*cos(pi*angle/180)
  y = r*sin(pi*angle/180)
  return(list(x=x, y=y))
}

getAngles = function(theta, n, clockwise=FALSE) {
  if(length(theta)==n) {
    if(length(unique(theta %% 360))!=n) stop("Angles suplied in theta are not unique.")
    theta0 = theta - theta[1] # rotating first to zero degrees
    if(any(theta0>=360)) stop("Angles supplied exceed one turn. ")
    inc = all(diff(theta) > 0)
    dec = all(diff(theta) < 0)
    if(!dec & !inc) stop("Angles must be suplied in an increasing or decreasing order.")
    return(theta)
  }
  if(length(theta)==1) {
    tmax = if(isTRUE(clockwise)) -360 else 360
    theta = theta + head(seq(0, tmax, length=n+1), n)
    return(theta)
  }
  stop("The argument 'theta' must be a single rotation or the angles for each value (length of x).")
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



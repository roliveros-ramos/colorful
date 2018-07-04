
polar2rect = function(radius, angle, ylim, rmin=0) {
  r = rmin + ((radius - ylim[1])/diff(ylim))*(1-rmin)
  x = r*cos(pi*angle/180)
  y = r*sin(pi*angle/180)
  return(list(x=x, y=y))
}

spiderAxis = function(n, at=0.5, rmin=1/nvar, theta=0, col="grey") {

  symbols(x=c(0,0), y=c(0,0), circles = c(rmin, 1), add=TRUE, inches=FALSE,
          fg=col)
  x0 = rep(0, n)
  x1 = rep(1, n)
  theta0 = theta + head(seq(0, 360, length=n+1), n)
  xy0 = polar2rect(radius=x0, angle=theta0, ylim=ylim, rmin=rmin)
  xy1 = polar2rect(radius=x1, angle=theta0, ylim=ylim, rmin=rmin)
  segments(x0=xy0$x, x1=xy1$x, y0=xy0$y, y1=xy1$y, col=col)

  if(!is.null(at)) {
    at = rmin + at*(1-rmin)
    for(i in seq_along(at)) {
      symbols(x=0, y=0, circles = at[i], add=TRUE, inches=FALSE,
              fg=col)
    }
  }

  return(invisible())

}

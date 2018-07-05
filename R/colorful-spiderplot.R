
#' @export
spiderplot = function(x, ...) {
  UseMethod("spiderplot")
}

#' @export
spiderplot.default = function(x, ylim, type="b", col=1, fill=FALSE, border=NULL,
                              theta=0, pch=19, lwd=1, lty=1, cex=NULL, density=NULL,
                              angle=45, alpha=1, axes=TRUE, rmin=NULL,
                              col.axis="gray", ...) {

  plot.new()
  plot.window(xlim=c(-1.2, 1.2), ylim=c(-1.2, 1.2), asp=1)

  nvar = length(x)
  if(is.null(rmin)) rmin = 1/nvar

  ylim = .checkYlim(ylim, x)

  if(isTRUE(axes))
    spiderAxis(n=nvar, at=0.5, rmin=rmin, theta=theta, col=col.axis)

  spider(x=x, ylim=ylim, type=type, col=col, fill=fill, border=border,
         theta=theta, pch=pch, lwd=lwd, lty=lty, cex=cex, density=density,
         angle=angle, alpha=alpha, rmin=rmin, ...)

  return(invisible())

}



# Auxiliar functions for spiderplot ---------------------------------------

#' Title
#'
#' @param n
#' @param ylim
#' @param at
#' @param rmin
#' @param theta
#' @param col
#'
#' @return
#' @export
#'
#' @examples
spiderAxis = function(n, at=0.5, rmin=1/nvar, theta=0, col="grey") {

  symbols(x=c(0,0), y=c(0,0), circles = c(rmin, 1), add=TRUE, inches=FALSE,
          fg=col)
  x0 = rep(0, n)
  x1 = rep(1, n)

  ylim = .checkYlim(c(0,1), x0)

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

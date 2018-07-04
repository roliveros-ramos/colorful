
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

  if(isTRUE(axes))
    spiderAxis(n=nvar, at=0.5, rmin=rmin, theta=theta, col=col.axis)

  spider(x=x, ylim=ylim, type=type, col=col, fill=fill, border=border,
         theta=theta, pch=pch, lwd=lwd, lty=lty, cex=cex, density=density,
         angle=angle, alpha=alpha, rmin=rmin, ...)

  return(invisible())

}



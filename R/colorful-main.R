


#' Add a spider web to a spider plot.
#'
#' @param x
#' @param ylim
#' @param type
#' @param col
#' @param fill
#' @param border
#' @param theta
#' @param pch
#' @param lwd
#' @param lty
#' @param cex
#' @param density
#' @param angle
#' @param alpha
#' @param rmin
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
spider = function(x, ylim, type="b", col=1, fill=FALSE, border=NULL,
                  theta=0, pch=19, lwd=1, lty=1, cex=NULL, density=NULL,
                  angle=45, alpha=1, rmin=NULL, clockwise=FALSE, ...) {

  type = match.arg(type, c("b", "p", "l", "n"))

  nvar = length(x)

  if(is.null(rmin)) rmin = 1/nvar

  theta = getAngles(theta, n=nvar, clockwise=clockwise)

  ylim = .checkYlim(ylim, x)

  xy = polar2rect(radius=x, angle=theta, ylim=ylim, rmin=rmin)
  x = xy$x
  y = xy$y

  cex = if(is.null(cex)) par("cex") else cex*par("cex")

  if(is.null(border)) border = col

  if(is.logical(fill)&!is.na(fill)) fill = if(isTRUE(fill)) col else NA
  if(is.na(fill)) alpha = 1
  if(alpha!=1) {
    fill = grDevices::col2rgb(fill)
    alpha = floor(255*alpha)
    fill = grDevices::rgb(red=fill[1], green=fill[2], blue=fill[3], alpha=alpha, maxColorValue=255)
  }

  polygon(x,y, col=fill, border=NA, density=density, angle=angle)
  if(type %in% c("b", "l"))
    lines(c(x, x[1]),c(y, y[1]), col=border, lwd=lwd, lty=lty)
  if(type %in% c("b", "p"))
    points(x,y, pch=pch, col=border, cex=cex)
  return(invisible())

}


#' Add a donut to a spider plot.
#'
#' @param x1
#' @param x2
#' @param ylim
#' @param type
#' @param col
#' @param fill
#' @param border
#' @param theta
#' @param pch
#' @param lwd
#' @param lty
#' @param cex
#' @param density
#' @param angle
#' @param alpha
#' @param rmin
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
donut = function(x1, x2, ylim, type="b", col=1, fill=FALSE, border=NULL,
                 theta=0, pch=19, lwd=1, lty=1, cex=NULL, density=NULL,
                 angle=45, alpha=1, rmin=NULL, clockwise=FALSE, ...) {

  if(length(x1)!=length(x2)) stop("Lengths of 'x1' and 'x2' don't match.")

  type = match.arg(type, c("b", "p", "l", "n"))

  nvar = length(x1)

  if(is.null(rmin)) rmin = 1/nvar

  theta = getAngles(theta, n=nvar, clockwise=clockwise)

  ylim = .checkYlim(ylim, x)

  xy1 = polar2rect(radius=x1, angle=theta, ylim=ylim, rmin=rmin)
  x1 = xy1$x
  y1 = xy1$y

  xy2 = polar2rect(radius=x2, angle=theta, ylim=ylim, rmin=rmin)
  x2 = xy2$x
  y2 = xy2$y

  x = c(x1, x1[1], x2, x2[1])
  y = c(y1, y1[1], y2, y2[1])

  cex = if(is.null(cex)) par("cex") else cex*par("cex")
  if(is.null(border)) border = col

  if(is.logical(fill)&!is.na(fill)) fill = if(isTRUE(fill)) col else NA
  if(is.na(fill)) alpha = 1
  if(alpha!=1) {
    fill = grDevices::col2rgb(fill)
    alpha = floor(255*alpha)
    fill = grDevices::rgb(red=fill[1], green=fill[2], blue=fill[3], alpha=alpha, maxColorValue=255)
  }

  polygon(x, y, col=fill, border=NA, density=density, angle=angle)

  if(type %in% c("b", "l")) {
    polygon(x1, y1, border=border, lwd=lwd, lty=lty)
    polygon(x2, y2, border=border, lwd=lwd, lty=lty)
  }
  if(type %in% c("b", "p")) {
    points(x,y, pch=pch, col=col, cex=cex)
  }

  return(invisible())

}

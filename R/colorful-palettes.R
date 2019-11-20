

#' Color palette for 'bias' plots.
#'
#' @param n Number of colors
#' @param zlim Range of the variable, zero must be inside.
#' @param col Vector of two colors, for the negative and positive values.
#' @param symmetric Boolean, if TRUE makes the palette symmetric (same intensity given the absolute value of the variable).
#' @param p Power to scale the increase in intensity. Default is 0.8, 1 gives linear decrease.
#' @param center Center of the palette, zero by default.
#'
#' @return
#' @export
#'
#' @examples
divergencePalette = function(n=64, zlim=c(-1,1), col=c("dodgerblue3", "firebrick3"),
                       symmetric=TRUE, p=0.8, center=0) {

  zlim = zlim - center

  if(prod(zlim)>=0) stop("Zero must be within 'zlim'.")

  zseq = seq(from=zlim[1], to=zlim[2], length=n)

  if(isTRUE(symmetric)) {

    K = max(abs(zlim)) # make it symetric!
    alpha = zseq/K

  } else {

    alpha = zseq
    alpha[alpha<0] = alpha[alpha<0]/abs(zlim[1])
    alpha[alpha>=0] = alpha[alpha>=0]/abs(zlim[2])

  }

  alpha = sign(alpha)*abs(alpha)^p

  cols = c(sapply(abs(alpha[alpha<0]), FUN=.makeTransparent, col=col[1]),
           sapply(abs(alpha[alpha>=0]), FUN=.makeTransparent, col=col[2]))

  return(unlist(cols))

}



#' Directional color palette.
#'
#' @param n Number of colors
#' @param col Base color for the palette.
#' @param p Power to scale the increase in intensity. Default is 0.8, 1 gives linear decrease.
#'
#' @return
#' @export
#'
#' @examples
directionalPalette = function(n=64, col="green3", p=0.8, ...) {

  alpha = seq(from=0, to=1, length=n)^p

  cols = sapply(alpha, FUN=.makeTransparent, col=col)

  return(unlist(cols))

}


# Internal ----------------------------------------------------------------

  .makeTransparent = function(alpha, col) {
    col = col2rgb(col)
    alpha = floor(255*alpha)
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }

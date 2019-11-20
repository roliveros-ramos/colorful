

# to keep back compatibility for a while
#' @export
biasPalette = function(n=64, zlim=c(-1,1), col=c("dodgerblue3", "firebrick3"),
                       symmetric=TRUE, p=0.8, center=0) {

  .Deprecated("divergencePalette")
  divergencePalette(n=n, zlim=zlim, col=col, symmetric=symmetric, p=p, center=center)

}

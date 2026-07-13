#' Convert pixel coordinates to real coordinates
#'
#' @export
pixelToUnits <- function(x,
                         coords){

  if(is.null(x$scale))
    stop(
      "Image has not been calibrated."
    )


  coords * x$scale
}

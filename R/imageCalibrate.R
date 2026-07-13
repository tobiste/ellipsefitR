#' Calibrate image scale using a scale bar
#'
#' Select two points corresponding to the ends of a scale bar.
#'
#' @param x A geoImage object.
#' @param units Real-world units of the scale bar.
#'
#' @return A calibrated geoImage object.
#'
#' @export
calibrateScale <- function(x,
                           units = "mm"){

  if(!inherits(x, "geoImage")){
    stop(
      "x must be a geoImage object."
    )
  }


  message(
    "Select the two endpoints of the scale bar."
  )


  plot(x)


  pts <- locator(
    n = 2,
    type = "p"
  )


  if(is.null(pts$x) ||
     length(pts$x) != 2){

    stop(
      "Exactly two points required."
    )
  }


  p1 <- c(
    pts$x[1],
    pts$y[1]
  )

  p2 <- c(
    pts$x[2],
    pts$y[2]
  )


  pixel_distance <-
    sqrt(
      sum(
        (p2 - p1)^2
      )
    )


  known_distance <- as.numeric(
    readline(
      paste0(
        "Enter scale bar length (",
        units,
        "): "
      )
    )
  )


  if(is.na(known_distance) ||
     known_distance <= 0){

    stop(
      "Invalid scale length."
    )
  }


  x$scale <-
    known_distance /
    pixel_distance


  x$units <- units


  x$calibration <- list(

    points = rbind(
      p1,
      p2
    ),

    pixel_distance =
      pixel_distance,

    distance =
      known_distance,

    units =
      units,

    units_per_pixel =
      x$scale,

    pixels_per_unit =
      1/x$scale
  )


  class(x) <- "geoImage"


  invisible(x)
}

#' Rotate image using a reference horizontal line
#'
#' Select two points that should define a horizontal line.
#'
#' @param x A geoImage object.
#'
#' @return Rotated geoImage object.
#'
#' @export
rotateImage <- function(x){

  if(!inherits(x,"geoImage")){
    stop(
      "x must be a geoImage object."
    )
  }


  plot(x)


  message(
    "Select two points defining the horizontal reference line."
  )


  pts <- locator(
    n=2,
    type="p"
  )


  if(length(pts$x) != 2){

    stop(
      "Two points required."
    )
  }


  dx <-
    pts$x[2] -
    pts$x[1]

  dy <-
    pts$y[2] -
    pts$y[1]


  angle <-
    atan2(
      dy,
      dx
    )


  angle_deg <-
    angle * 180/pi


  message(
    "Rotation angle: ",
    round(angle_deg,3),
    " degrees"
  )


  ## rotate image

  if(!requireNamespace("magick",
                       quietly=TRUE)){

    stop(
      "Package 'magick' required."
    )
  }


  temp <- tempfile(
    fileext=".png"
  )


  png::writePNG(
    x$image,
    temp
  )


  im <-
    magick::image_read(temp)


  rotated <-
    magick::image_rotate(
      im,
      angle = -angle_deg
    )


  rotated_array <-
    magick::image_data(
      rotated,
      channels="rgb"
    )


  ## convert magick format
  ## [channel,x,y] -> [y,x,channel]

  rotated_array <-
    aperm(
      rotated_array,
      c(3,2,1)
    )


  y <- x

  y$image <-
    rotated_array


  y$width <-
    dim(rotated_array)[2]

  y$height <-
    dim(rotated_array)[1]


  y$rotation <-
    x$rotation + angle_deg


  y$rotation.matrix <-
    matrix(
      c(
        cos(angle),
        -sin(angle),
        sin(angle),
        cos(angle)
      ),
      2,
      2
    )


  class(y) <- "geoImage"


  y
}

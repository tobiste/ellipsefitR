#' Create a geoImage object
#'
#' @param image image array
#' @param file filename
#'
#' @return object of class geoImage
#'
#' @keywords internal
.new_geoImage <- function(image,
                          file = NULL){

  structure(
    list(

      image = image,

      file = file,

      width = dim(image)[2],

      height = dim(image)[1],

      scale = 1,

      units = NULL,

      rotation = 0,

      perspective = NULL

    ),
    class = "geoImage"
  )
}

#' Correct perspective distortion of a geoImage
#'
#' Click four corners clockwise.
#'
#' @param x geoImage object
#'
#' @return corrected geoImage
#'
#' @export
perspectiveCorrect <- function(x){

  if(!inherits(x,"geoImage")){

    stop(
      "x must be a geoImage object."
    )
  }


  plot(x)


  message(
    "Click four corners clockwise."
  )


  pts <- locator(
    n=4,
    type="p"
  )


  if(length(pts$x) != 4){

    stop(
      "Four points required."
    )
  }


  src <- cbind(
    pts$x,
    pts$y
  )


  width <-
    max(src[,1])-min(src[,1])

  height <-
    max(src[,2])-min(src[,2])


  dst <- matrix(
    c(
      0,0,
      width,0,
      width,height,
      0,height
    ),
    4,
    2,
    byrow=TRUE
  )


  H <- .homography(
    src,
    dst
  )


  # write temporary image

  tmp <- tempfile(
    fileext=".png"
  )


  png::writePNG(
    x$image,
    tmp
  )


  im <-
    magick::image_read(tmp)


  # perspective transformation
  #
  # magick expects:
  # x1,y1 x2,y2 ...

  coords <- paste(
    c(
      as.vector(t(src)),
      as.vector(t(dst))
    ),
    collapse=","
  )


  corrected <-
    magick::image_distort(
      im,
      method="perspective",
      coordinates=coords,
      bestfit=TRUE
    )


  arr <-
    magick::image_data(
      corrected,
      channels="rgb"
    )


  arr <-
    aperm(
      arr,
      c(3,2,1)
    )


  y <- x

  y$image <-
    arr


  y$width <-
    dim(arr)[2]

  y$height <-
    dim(arr)[1]


  y$perspective <- list(

    source = src,

    target = dst,

    matrix = H

  )


  class(y) <- "geoImage"


  y
}

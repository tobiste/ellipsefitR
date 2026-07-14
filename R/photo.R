#' Display an image with pixel coordinates
#'
#' @param file image filename
#' @param asp aspect ratio
#'
#' @return image array invisibly
#'
#' @export
#' @importFrom tools file_ext
#' @importFrom jpeg readJPEG
#' @importFrom png readPNG
plotImage <- function(file,
                      asp = 1) {
  ext <- tools::file_ext(file)

  if (tolower(ext) %in% c("jpg", "jpeg")) {
    img <- jpeg::readJPEG(file)
  } else if (tolower(ext) == "png") {
    img <- png::readPNG(file)
  } else {
    stop("Only jpeg and png supported.")
  }


  nr <- dim(img)[1]
  nc <- dim(img)[2]


  plot(
    NA,
    xlim = c(0, nc),
    ylim = c(nr, 0),
    asp = asp,
    xlab = "pixel x",
    ylab = "pixel y"
  )


  rasterImage(
    img,
    xleft = 0,
    ybottom = nr,
    xright = nc,
    ytop = 0
  )


  invisible(img)
}

#' Fit ellipse interactively on an image
#'
#' @param file image filename
#' @param nmin minimum number of points
#'
#' @export
locatorEllipseImage <- function(file, nmin = 5) {
  img <- plotImage(file)

  message(
    "Left-click points. Press ESC or right-click when finished."
  )

  pts <- locator()

  if (is.null(pts)) {
    stop("No points selected.")
  }

  dat <- cbind(pts$x, pts$y)

  if (nrow(dat) < nmin) {
    stop("Need at least ", nmin, " points.")
  }

  fit <- fitEllipse(dat)

  ## redraw image
  plotImage(file)

  ## draw selected points
  points(dat,
    pch = 16,
    col = "yellow"
  )

  ## draw ellipse
  lines(predict(fit),
    col = "red",
    lwd = 2
  )

  ## draw centre
  points(fit$center[1],
    fit$center[2],
    pch = 3,
    cex = 2,
    lwd = 2
  )

  fit$data <- dat

  invisible(fit)
}

#' Calibrate image scale using a scale bar
#'
#' @param file image filename
#' @param units real-world units
#'
#' @export
calibrateImage <- function(file,
                           units = "mm") {
  img <- plotImage(file)


  message(
    "Click the two ends of the scale bar"
  )


  pts <- locator(
    n = 2,
    type = "p"
  )


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


  known_distance <-
    as.numeric(
      readline(
        paste0(
          "Enter scale bar length (",
          units,
          "): "
        )
      )
    )


  scale <-
    known_distance /
      pixel_distance


  result <- list(
    pixels =
      pixel_distance,
    distance =
      known_distance,
    units =
      units,
    scale =
      scale,
    units_per_pixel =
      scale
  )


  class(result) <-
    "image_scale"


  invisible(result)
}

convertEllipseUnits <- function(fit,
                                calibration) {
  scale <-
    calibration$units_per_pixel


  fit$major.units <-
    fit$major * scale


  fit$minor.units <-
    fit$minor * scale


  fit$area.units <-
    fit$area * scale^2


  fit$rmse.units <-
    fit$rmse * scale


  fit$units <-
    calibration$units


  fit
}

#' Rotate image using two reference points
#'
#' @export
#' @importFrom magick image_read image_rotate image_write
#' @importFrom tools file_path_sans_ext file_ext
rotateImage <- function(file,
                        output = NULL) {
  img <- plotImage(file)


  message(
    "Click two points defining the horizontal line"
  )


  pts <- locator(
    n = 2,
    type = "p"
  )


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
    angle * 180 / pi


  im <-
    magick::image_read(file)


  ## rotate opposite direction

  rotated <-
    magick::image_rotate(
      im,
      -angle_deg
    )


  if (is.null(output)) {
    output <-
      paste0(
        tools::file_path_sans_ext(file),
        "_rotated.",
        tools::file_ext(file)
      )
  }


  magick::image_write(
    rotated,
    output
  )


  list(
    file =
      output,
    rotation =
      angle_deg
  )
}


.homography <- function(src, dst) {
  n <- nrow(src)

  A <- matrix(0, n * 2, 8)
  b <- numeric(n * 2)


  for (i in seq_len(n)) {
    x <- src[i, 1]
    y <- src[i, 2]

    X <- dst[i, 1]
    Y <- dst[i, 2]


    A[2 * i - 1, ] <-
      c(
        x, y, 1,
        0, 0, 0,
        -X * x,
        -X * y
      )

    A[2 * i, ] <-
      c(
        0, 0, 0,
        x, y, 1,
        -Y * x,
        -Y * y
      )

    b[2 * i - 1] <- X
    b[2 * i] <- Y
  }


  h <- solve(A, b)


  matrix(
    c(
      h,
      1
    ),
    3,
    3,
    byrow = TRUE
  )
}

#' Correct perspective distortion of an image
#'
#' Click four corners clockwise.
#'
#' @export
#' @importFrom magick image_read image_distort image_write
perspectiveCorrect <- function(file,
                               width = NULL,
                               height = NULL,
                               output = NULL) {
  img <- plotImage(file)


  message(
    "Click four corners clockwise"
  )


  pts <- locator(
    n = 4,
    type = "p"
  )


  src <- cbind(
    pts$x,
    pts$y
  )


  if (is.null(width)) {
    width <- max(src[, 1]) - min(src[, 1])
  }

  if (is.null(height)) {
    height <- max(src[, 2]) - min(src[, 2])
  }


  dst <- matrix(
    c(
      0, 0,
      width, 0,
      width, height,
      0, height
    ),
    4,
    2,
    byrow = TRUE
  )


  H <-
    .homography(
      src,
      dst
    )


  ## use magick perspective transform

  im <-
    magick::image_read(file)


  coords <-
    paste(
      as.vector(t(src)),
      collapse = ","
    )


  corrected <-
    magick::image_distort(
      im,
      "perspective",
      coords,
      bestfit = TRUE
    )


  if (is.null(output)) {
    output <-
      paste0(
        tools::file_path_sans_ext(file),
        "_corrected.",
        tools::file_ext(file)
      )
  }


  magick::image_write(
    corrected,
    output
  )


  list(
    file = output,
    homography = H,
    source = src,
    target = dst
  )
}

#' @export
plot.geoImage <- function(x,
                          ...) {
  plot(
    NA,
    xlim = c(0, x$width),
    ylim = c(x$height, 0),
    asp = 1,
    xlab = "pixel x",
    ylab = "pixel y",
    ...
  )


  graphics::rasterImage(
    x$image,
    0,
    x$height,
    x$width,
    0
  )


  invisible(x)
}

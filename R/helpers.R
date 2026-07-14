.openDevice <- function() {
  if (.Platform$OS.type == "windows") {
    windows()
  } else if (Sys.info()["sysname"] == "Darwin") {
    quartz()
  } else {
    x11()
  }
}

drawImage <- function(img) {
  plot(
    NA,
    xlim = c(0, img$width),
    ylim = c(img$height, 0),
    asp = 1,
    xaxs = "i",
    yaxs = "i"
  )

  rasterImage(
    img$image,
    0,
    img$height,
    img$width,
    0
  )
}


.new_geoImage <- function(image,
                          file = NULL) {
  transformations <- list()

  structure(
    list(
      image = image,
      image.original = image,
      file = file,
      width = dim(image)[2],
      height = dim(image)[1],
      scale = NULL,
      units = NULL,
      calibration = NULL,
      rotation = 0,
      rotation.matrix = NULL,
      perspective = NULL
    ),
    class = "geoImage"
  )
}


.homography <- function(src, dst) {
  n <- nrow(src)

  A <- matrix(
    0,
    nrow = 2 * n,
    ncol = 8
  )

  b <- numeric(2 * n)


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
    c(h, 1),
    3,
    3,
    byrow = TRUE
  )
}

.new_ellipse_collection <- function(image) {
  structure(
    list(
      image = image,
      ellipses = list()
    ),
    class = "ellipse_collection"
  )
}

#' @exportS3Method base::plot
plot.ellipse_collection <- function(x, ...) {
  plot(x$image)


  for (i in seq_along(x$ellipses)) {
    e <- x$ellipses[[i]]


    lines(
      predict(e),
      lwd = 2
    )


    points(
      e$image.points,
      pch = 16
    )


    text(
      e$center[1],
      e$center[2],
      labels = i,
      cex = 1.5
    )
  }


  invisible(x)
}

#' @exportS3Method base::summary
summary.ellipse_collection <- function(object, ...) {
  data.frame(
    id =
      seq_along(object$ellipses),
    major =
      sapply(
        object$ellipses,
        `[[`,
        "major"
      ),
    minor =
      sapply(
        object$ellipses,
        `[[`,
        "minor"
      ),
    Rf =
      sapply(
        object$ellipses,
        `[[`,
        "Rf"
      ),
    angle =
      sapply(
        object$ellipses,
        `[[`,
        "angle"
      ),
    RMSE =
      sapply(
        object$ellipses,
        `[[`,
        "rmse"
      )
  )
}


#' @export
as.data.frame.ellipse_collection <- function(x, ...) {
  stopifnot(inherits(x, "ellipse_collection"))

  if (length(x$ellipses) == 0) {
    return(data.frame())
  }

  do.call(
    rbind,
    lapply(seq_along(x$ellipses), function(i) {
      e <- x$ellipses[[i]]

      data.frame(
        id = i,
        x = e$center[1],
        y = e$center[2],
        major = e$major,
        minor = e$minor,
        angle = e$angle,
        eccentricity = e$eccentricity,
        area = e$area,
        rmse = e$rmse,
        residual_mean = mean(e$residuals),
        residual_sd = stats::sd(e$residuals),
        residual_min = min(e$residuals),
        residual_max = max(e$residuals),
        n_points = nrow(e$points),
        stringsAsFactors = FALSE
      )
    })
  )
}

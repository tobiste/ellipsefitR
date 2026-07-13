fit_ellipse <- function(x, y) {

  stopifnot(length(x) == length(y))
  if(length(x) < 5)
    stop("At least 5 points are required.")

  ## Design matrix
  D <- cbind(
    x^2,
    x * y,
    y^2,
    x,
    y,
    1
  )

  ## Solve using SVD
  sv <- svd(D)
  coef <- sv$v[, 6]

  names(coef) <- c("A","B","C","D","E","F")

  A <- coef[1]
  B <- coef[2]
  C <- coef[3]
  Dd <- coef[4]
  Ee <- coef[5]
  Ff <- coef[6]

  ## Center
  den <- B^2 - 4*A*C

  x0 <- (2*C*Dd - B*Ee) / den
  y0 <- (2*A*Ee - B*Dd) / den

  ## Orientation
  theta <- 0.5 * atan2(B, A - C)

  ## Translate constant term
  F0 <- Ff +
    A*x0^2 +
    B*x0*y0 +
    C*y0^2 +
    Dd*x0 +
    Ee*y0

  ## Rotated quadratic coefficients
  ct <- cos(theta)
  st <- sin(theta)

  Ap <- A*ct^2 + B*ct*st + C*st^2
  Cp <- A*st^2 - B*ct*st + C*ct^2

  ## Semi-axis lengths
  a <- sqrt(-F0 / Ap)
  b <- sqrt(-F0 / Cp)

  ## Ensure a >= b
  if(b > a){
    tmp <- a
    a <- b
    b <- tmp
    theta <- theta + pi/2
  }

  list(
    center = c(x = x0, y = y0),
    major = a,
    minor = b,
    angle = theta
  )
}

.ellipse_fitzgibbon <- function(x, y) {

  stopifnot(length(x) == length(y))

  if(length(x) < 5)
    stop("At least five points are required.")

  x <- as.numeric(x)
  y <- as.numeric(y)

  ## normalize coordinates for numerical stability

  mx <- mean(x)
  my <- mean(y)

  sx <- sd(x)
  sy <- sd(y)

  xs <- (x - mx)/sx
  ys <- (y - my)/sy

  ## design matrices

  D1 <- cbind(
    xs^2,
    xs * ys,
    ys^2
  )

  D2 <- cbind(
    xs,
    ys,
    1
  )

  ## scatter matrices

  S1 <- crossprod(D1)
  S2 <- crossprod(D1, D2)
  S3 <- crossprod(D2)

  ## constraint matrix

  C1 <- matrix(
    c(
      0, 0, 2,
      0,-1, 0,
      2, 0, 0
    ),
    3,3,
    byrow = TRUE
  )

  ## reduced eigenproblem

  T <- -solve(S3, t(S2))

  M <- solve(C1, S1 + S2 %*% T)

  eg <- eigen(M)

  cond <- 4 * eg$vectors[1,] * eg$vectors[3,] -
    eg$vectors[2,]^2

  k <- which(cond > 0)

  if(length(k) == 0)
    stop("No ellipse found.")

  a1 <- eg$vectors[, k[1]]

  coef <- c(a1, T %*% a1)

  names(coef) <- c(
    "A","B","C","D","E","F"
  )

  ## un-normalize

  A <- coef[1]/sx^2
  B <- coef[2]/(sx*sy)
  C <- coef[3]/sy^2

  D <- coef[4]/sx -
    2*A*mx -
    B*my

  E <- coef[5]/sy -
    B*mx -
    2*C*my

  F <- coef[6] -
    coef[4]*mx/sx -
    coef[5]*my/sy +
    A*mx^2 +
    B*mx*my +
    C*my^2

  c(A,B,C,D,E,F)
}

.ellipse_parameters <- function(coef){

  A <- coef[1]
  B <- coef[2]
  C <- coef[3]
  D <- coef[4]
  E <- coef[5]
  F <- coef[6]

  den <- B^2 - 4*A*C

  if(abs(den) < .Machine$double.eps)
    stop("Degenerate conic.")

  xc <- (2*C*D - B*E)/den
  yc <- (2*A*E - B*D)/den

  theta <- 0.5 * atan2(B, A - C)

  up <- 2*(A*xc^2 +
             C*yc^2 +
             B*xc*yc -
             F)

  down1 <- A + C +
    sqrt((A-C)^2 + B^2)

  down2 <- A + C -
    sqrt((A-C)^2 + B^2)

  a <- sqrt(up/down1)
  b <- sqrt(up/down2)

  if(b > a){

    tmp <- a
    a <- b
    b <- tmp

    theta <- theta + pi/2
  }

  theta <- theta %% pi

  list(

    center = c(x = xc,
               y = yc),

    major = a,

    minor = b,

    angle = theta,

    eccentricity =
      sqrt(1 - b^2/a^2),

    area =
      pi*a*b,

    coefficients =
      coef
  )
}

fitEllipse <- function(x, y = NULL){

  if(is.matrix(x)){

    y <- x[,2]
    x <- x[,1]

  } else if(is.data.frame(x)){

    y <- x[[2]]
    x <- x[[1]]

  }

  coef <- .ellipse_fitzgibbon(x, y)

  pars <- .ellipse_parameters(coef)

  pars$call <- match.call()

  class(pars) <- "ellipse_fit"

  pars
}

.ellipse_points <- function(center, major, minor, angle,
                            n = 200){

  t <- seq(0, 2*pi, length.out = n)

  ct <- cos(angle)
  st <- sin(angle)

  x <- center[1] +
    major*cos(t)*ct -
    minor*sin(t)*st

  y <- center[2] +
    major*cos(t)*st +
    minor*sin(t)*ct

  cbind(
    x = x,
    y = y
  )
}

.ellipse_closest <- function(point,
                             center,
                             a,
                             b,
                             theta,
                             tol = 1e-12,
                             maxit = 100){

  ## transform to ellipse coordinates

  R <- matrix(
    c(cos(theta), -sin(theta),
      sin(theta),  cos(theta)),
    2,2
  )

  p <- solve(R) %*%
    (point - center)

  x <- p[1]
  y <- p[2]

  ## initial guess

  t <- 0

  for(i in seq_len(maxit)){


    f <-
      (a^2*x^2)/(t+a^2)^2 +
      (b^2*y^2)/(t+b^2)^2 -
      1


    df <-
      -2*a^2*x^2/(t+a^2)^3 -
      2*b^2*y^2/(t+b^2)^3


    tnew <- t - f/df


    if(abs(tnew-t) < tol)
      break

    t <- tnew
  }


  qx <- a^2*x/(t+a^2)
  qy <- b^2*y/(t+b^2)


  ## transform back

  q <- R %*%
    c(qx,qy) +
    center


  distance <- sqrt(sum((point-q)^2))


  list(
    point=q,
    distance=distance,
    iterations=i
  )
}

ellipseResiduals <- function(object,
                             x,
                             y=NULL){

  if(is.null(y)){

    y <- x[,2]
    x <- x[,1]
  }


  res <- lapply(
    seq_along(x),
    function(i){

      .ellipse_closest(
        c(x[i],y[i]),
        object$center,
        object$major,
        object$minor,
        object$angle
      )
    }
  )


  distances <- vapply(
    res,
    function(z) z$distance,
    numeric(1)
  )


  distances
}

fitEllipse <- function(x, y=NULL){

  if(is.matrix(x)){

    y <- x[,2]
    x <- x[,1]

  } else if(is.data.frame(x)){

    y <- x[[2]]
    x <- x[[1]]
  }


  coef <- .ellipse_fitzgibbon(x,y)

  pars <- .ellipse_parameters(coef)


  residuals <-
    ellipseResiduals(
      pars,
      x,
      y
    )


  pars$residuals <- residuals

  pars$rmse <-
    sqrt(mean(residuals^2))

  pars$mad <-
    median(abs(residuals -
                 median(residuals)))

  pars$max.error <-
    max(residuals)


  pars$n <-
    length(x)

  pars$call <-
    match.call()


  class(pars) <- "ellipse_fit"


  pars
}

#' @export
print.ellipse_fit <- function(x, ...){

  cat("Ellipse fit\n")
  cat("-----------\n")

  cat("Center:\n")
  print(x$center)

  cat("\nSemi-major axis:",
      signif(x$major,5),
      "\n")

  cat("Semi-minor axis:",
      signif(x$minor,5),
      "\n")

  cat("Orientation:",
      signif(x$angle*180/pi,5),
      "degrees\n")

  cat("\nEccentricity:",
      signif(x$eccentricity,5),
      "\n")

  cat("Area:",
      signif(x$area,5),
      "\n")

  cat("\nResidual statistics:\n")

  cat("  RMSE:",
      signif(x$rmse,5),
      "\n")

  cat("  MAD:",
      signif(x$mad,5),
      "\n")

  cat("  Maximum error:",
      signif(x$max.error,5),
      "\n")

  invisible(x)
}

#' @export
coef.ellipse_fit <- function(object, ...){

  c(
    center.x = object$center[1],
    center.y = object$center[2],
    major = object$major,
    minor = object$minor,
    angle = object$angle
  )
}

#' @export
predict.ellipse_fit <- function(object,
                                n = 200,
                                ...){

  .ellipse_points(
    object$center,
    object$major,
    object$minor,
    object$angle,
    n
  )
}

#' @export
plot.ellipse_fit <- function(x,
                             points = TRUE,
                             add = FALSE,
                             ...){

  ell <-
    predict(x)


  if(!add){

    plot(
      ell,
      type="n",
      asp=1,
      xlab="x",
      ylab="y"
    )
  }


  lines(
    ell,
    ...
  )


  if(points &&
     !is.null(x$data)){

    points(
      x$data[,1],
      x$data[,2],
      pch=16
    )
  }


  invisible(x)
}

#' @export
summary.ellipse_fit <- function(object,...){

  list(

    geometry =
      coef(object),

    eccentricity =
      object$eccentricity,

    area =
      object$area,

    error =
      c(
        RMSE = object$rmse,
        MAD = object$mad,
        Maximum = object$max.error
      ),

    n =
      object$n

  )
}


#' Interactive ellipse fitting
#'
#' Click points and finish with right-click.
#'
#' @export
locatorEllipse <- function(nmin=5, ...){

  message(
    "Click points. Finish with ESC"
  )


  pts <- locator(
    type="p",
    ...
  )


  if(length(pts$x) < nmin)
    stop(
      "At least ",
      nmin,
      " points required."
    )


  dat <- cbind(
    x=pts$x,
    y=pts$y
  )


  fit <-
    fitEllipse(dat)


  plot(
    dat,
    asp=1,
    pch=16,
    xlab="x",
    ylab="y"
  )


  lines(
    predict(fit),
    lwd=2
  )


  points(
    fit$center[1],
    fit$center[2],
    pch=4,
    cex=2,
    lwd=2
  )


  invisible(fit)
}



#
# plot(
#   1,
#   type="n",
#   xlim=c(0,10),
#   ylim=c(0,10),
#   asp=1
# )
#
# fit <- locatorEllipse()

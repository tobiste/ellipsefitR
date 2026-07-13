.applyHomogeneous <- function(coords, H){

  p <- cbind(
    coords,
    1
  )

  out <- t(
    H %*% t(p)
  )


  out[,1:2] / out[,3]
}

#' Transform coordinates of a geoImage
#'
#' @param x geoImage object
#' @param coords two-column matrix
#'
#' @export
transformCoordinates <- function(x,
                                 coords){

  if(!inherits(x,"geoImage")){
    stop(
      "x must be a geoImage object."
    )
  }


  if(!is.matrix(coords) ||
     ncol(coords)!=2){

    stop(
      "coords must be a two-column matrix."
    )
  }


  H <- diag(3)


  ## perspective

  if(!is.null(x$perspective)){

    H <-
      x$perspective$matrix %*%
      H

  }


  ## rotation

  if(!is.null(x$rotation.matrix)){

    R <- matrix(
      c(
        x$rotation.matrix,
        0,0,1
      ),
      3,
      3
    )

    H <-
      R %*% H

  }


  .applyHomogeneous(
    coords,
    H
  )
}


#' Transform corrected coordinates back to original image
#'
#' @export
inverseTransformCoordinates <- function(x,
                                        coords){


  if(!inherits(x,"geoImage")){
    stop(
      "x must be a geoImage object."
    )
  }


  H <- diag(3)


  if(!is.null(x$perspective)){

    H <-
      x$perspective$matrix %*%
      H

  }


  if(!is.null(x$rotation.matrix)){

    R <- matrix(
      c(
        x$rotation.matrix,
        0,0,1
      ),
      3,
      3
    )

    H <-
      R %*% H
  }


  .applyHomogeneous(
    coords,
    solve(H)
  )
}

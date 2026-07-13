#' @exportS3Method base::print
print.geoImage <- function(x,...){

  cat("geoImage\n")
  cat("--------\n")

  cat(
    "File:",
    x$file,
    "\n"
  )

  cat(
    "Dimensions:",
    x$width,
    "x",
    x$height,
    "pixels\n"
  )


  if(is.null(x$units)){

    cat(
      "Scale: not calibrated\n"
    )

  } else {

    cat(
      "Scale:",
      signif(x$scale,5),
      x$units,
      "/pixel\n"
    )
  }


  if(x$rotation != 0){

    cat(
      "Rotation:",
      round(x$rotation,3),
      "degrees\n"
    )

  }

  if(!is.null(x$perspective)){

    cat(
      "Perspective corrected\n"
    )

  }

  invisible(x)
}

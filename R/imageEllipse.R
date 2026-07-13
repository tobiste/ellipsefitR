.ensureImageDevice <- function(){

  if(length(dev.list()) == 0){

    .openDevice()

  }

}


#' Digitize multiple ellipses on an image
#'
#' @param x geoImage object
#' @param nmin minimum points per ellipse
#'
#' @return ellipse_collection object
#'
#' @export
imageEllipse <- function(x, nmin = 5){

  if(!inherits(x, "geoImage")){
    stop("x must be a geoImage object.")
  }

  .ensureImageDevice()

  result <- .new_ellipse_collection(x)

  repeat {

    plot(x)

    # redraw existing ellipses
    if(length(result$ellipses) > 0){

      for(i in seq_along(result$ellipses)){

        e <- result$ellipses[[i]]

        if(!inherits(e, "ellipse_fit")){
          warning(
            "Skipping invalid ellipse object at index ",
            i
          )
          next
        }

        lines(
          predict(e),
          lwd = 2,
          col = "#E69F00"
        )

        points(
          e$points,
          pch = 16,
          col = "#56B4E9"
        )

        text(
          e$center[1],
          e$center[2],
          labels = i,
          cex = 1.5,
          col = "#009E73"
        )
      }
    }


    message(
      "Select points for ellipse ",
      length(result$ellipses)+1,
      ". Minimum points: ",
      nmin
    )


    pts <- locator()


    # user stopped input
    if(is.null(pts) ||
       length(pts$x)==0){

      message(
        "Finished digitizing."
      )

      break
    }


    coords <- cbind(
      x = pts$x,
      y = pts$y
    )


    # check number of points

    if(nrow(coords) < nmin){

      message(
        "Ellipse ignored: only ",
        nrow(coords),
        " points selected (minimum ",
        nmin,
        ")."
      )

      next
    }


    # try fitting ellipse safely

    fit <- tryCatch(

      fitEllipse(coords),

      error = function(e){

        message(
          "Ellipse fitting failed: ",
          e$message
        )

        NULL
      }
    )


    # fitting failed

    if(is.null(fit)){

      next
    }


    # store point coordinates

    fit$points <- coords

    fit$image <- x


    result$ellipses[[length(result$ellipses)+1]] <- fit


    message(
      "Stored class: ",
      class(result$ellipses[[length(result$ellipses)]])
    )


    answer <- readline(
      "Add another ellipse? (y/n): "
    )


    if(tolower(answer)!="y"){

      break
    }

  }


  # final redraw

  plot(x)

  for(i in seq_along(result$ellipses)){

    e <- result$ellipses[[i]]

    lines(
      predict(e),
      lwd = 2,
      col = "#E69F00"
    )

    points(
      e$points,
      pch = 16,
      col = "#56B4E9"
    )

    text(
      e$center[1],
      e$center[2],
      labels = i,
      cex = 1.5,
      font = 2,
      col = "#009E73"
    )
  }


  class(result) <- "ellipse_collection"

  return(result)
}

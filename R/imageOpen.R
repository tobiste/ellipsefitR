#' Open an image for geological measurements
#'
#' @param file image filename
#'
#' @return geoImage object
#'
#' @export
imageOpen <- function(file){

  ext <- tolower(
    tools::file_ext(file)
  )


  if(ext %in% c("jpg","jpeg")){

    img <- jpeg::readJPEG(file)

  } else if(ext == "png"){

    img <- png::readPNG(file)

  } else {

    stop(
      "Only JPEG and PNG images supported."
    )
  }


  .new_geoImage(
    image = img,
    file = file
  )
}

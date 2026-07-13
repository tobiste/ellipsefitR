.openDevice <- function(){

  if(.Platform$OS.type == "windows"){

    grDevices::windows()

  } else if(
    Sys.info()["sysname"] == "Darwin"
  ){

    grDevices::quartz()

  } else {

    grDevices::x11()

  }

}

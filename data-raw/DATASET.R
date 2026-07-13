## code to prepare `DATASET` dataset goes here

ht13 <- magick::image_read('data-raw/HT13xz.jpg')
usethis::use_data(ht13, overwrite = TRUE)

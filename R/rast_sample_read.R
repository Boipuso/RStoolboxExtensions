#' Read Sample Data for RStoolboxExtensions Package
#'
#' This function can be used to load the sample data included in the RStoolboxExtensions package
#' into your environment.
#'
#' @param path path to filename
#'
#' @return a \code{raster} that can be used as test data for the RStoolboxExtensions package functions.
#'
#' #' @details included files are:
#' Sebangau15.tif - Landsat8 median composite for the year 2015 of the Sebangau Nationalpark, Borneo
#' Sebangau23.tif - Landsat8 median composite for the year 2023 of the Sebangau Nationalpark, Borneo
#' class_img.tif <- classified landcover change detection image 2015-2023 of the Sebangau Nationalpark, Borneo
#'
#' @export
#'
#' @importFrom terra rast
#'
#' @examples
#' Sebangau15 <- system.file("extdata", "Sebangau15.tif", package = "RStoolboxExtensions")
#' Sebangau15 <- rast_sample_read(Sebangau15)
#'
#' Sebangau23 <- system.file("extdata", "Sebangau23.tif", package = "RStoolboxExtensions")
#' Sebangau23 <- rast_sample_read(Sebangau23)
#'
#' class_img <- system.file("extdata", "class_img.tif", package = "RStoolboxExtensions")
#' class_img <- rast_sample_read(class_img)

rast_sample_read <- function(path) {
  rast(path)
}


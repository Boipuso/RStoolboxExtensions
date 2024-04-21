#' Read Sample SF Data for RStoolboxExtensions Package
#'
#' This function can be used to load the sample sf data included in the RStoolboxExtensions package
#' into your environment. It serves as a demonstration how to retrieve the sf data imbedded in the RStoolboxExtensions package.
#'
#' @param path path to filename
#'
#' @return a \code{sf} that can be used as test data for the RStoolboxExtensions package functions.
#'
#' #' @details included files are:
#' trainPoints.geojson - training points for landcover change detection of the Sebangau Nationalpark, Borneo between 2015-2023
#' trainPolygons.geojson - training polygons for landcover change detection of the Sebangau Nationalpark, Borneo between 2015-2023
#'
#' @export
#'
#' @importFrom sf st_read
#'
#' @examples
#' # retrieving the file where the sf object is stored and load it
#' trainPoints <- system.file("extdata", "trainPoints.geojson", package = "RStoolboxExtensions")
#' trainPoints <- sf_sample_read(trainPoints)
#'
#' trainPolygons <- system.file("extdata", "trainPolygons.geojson", package = "RStoolboxExtensions")
#' trainPolygons <- sf_sample_read(trainPolygons)

sf_sample_read <- function(path) {
  st_read(path)
}

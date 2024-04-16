#' Pre-process Training Features for Supervised Classification
#'
#' Pre-processes training features to prepare them for a supervised classification using the "superClass" command from the RStoolbox package.
#' The function transforms the training features to a sf object if it wasn't already and reprojects it to the coordinate reference system (CRS) of the raster input.
#'
#' @param trainFeat Training features, either an sf object or an object that can be converted to an sf object.
#' @param raster A raster object representing the spatial extent and coordinate reference system (CRS) to which the training features will be aligned.
#'
#' @return A pre-processed sf object of training features aligned with coordinate reference system (CRS) of the input raster.
#'
#' @details This function pre-processes the training features to ensure they are in the same coordinate reference system (CRS) as the input raster. If the training features are not already in the sf format, they are converted to an sf object using `sf::st_as_sf()`. Then, if the CRS of the training features differs from the CRS of the raster, the function transforms the CRS of the training features to match the CRS of the raster using `sf::st_transform()`.
#'
#' @examples
#' # Load required packages
#' library(RStoolbox)
#' library(sf)
#'
#' # Create a raster object
#' raster <- raster::raster(matrix(runif(100), nrow = 10))
#'
#' # Create training features
#' trainFeat <- data.frame(x = runif(10), y = runif(10))
#' trainFeat <- sf::st_as_sf(trainFeat, coords = c("x", "y"))
#'
#' # Pre-process training features
#' pp_features(trainFeat, raster)
#'
#' @import sf
#'
#' @export

pp_features <- function(trainFeat, raster) {

  # convert training features to sf if it is not already an sf object
  if (!inherits(trainFeat, "sf")) {
    trainFeat <- sf::st_as_sf(trainFeat)
  }

  # pre-process by trainFeat if needed and renaming response column
  if (!identical(st_crs(trainFeat), st_crs(raster))) {
    trainFeat <- st_transform(trainFeat, st_crs(raster))
  }

  return(trainFeat)
}

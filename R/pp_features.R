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

  # convert training features to sf cause superClass function has problems with sp objects
  if (!inherits(trainFeat, "sf")) {
    message("trainFeat is not a sf object: Converting trainFeat to 'sf' object...")
    trainFeat <- st_as_sf(trainFeat)

    #check if the transformation worked
    if (!inherits(trainFeat, "sf")) {
      stop("trainFeat cannot be converted to a sf object. Provide a data type that can be converted to sf")
    }
    else{
      message("Conversion to sf objected completed")
    }
  }
  else{
    message("Object is already a sf object")
  }


  # and bring them to the same crs for the classification to work
  if (!identical(st_crs(trainFeat), st_crs(raster))) {
    message("CRS mismatch: Transforming trainFeat to match the CRS of the raster...")
    trainFeat <- st_transform(trainFeat, st_crs(raster))
    #check if the transformation worked
    if (!identical(st_crs(trainFeat), st_crs(raster))) {
      stop("CRS of trainFeat cannot be converted to CRS of the raster. Provide fitting types of CRS for the conversion with st_transform()")
    }
    else{
      message("Transformation to raster CRS completed")
    }
  }
  else{
    message("CRS of trainFeat and raster are already alligned")
  }

  return(trainFeat)
}



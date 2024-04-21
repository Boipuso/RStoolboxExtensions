#' Pre-process Training Features for Supervised Classification
#'
#' Pre-processes training features to prepare them for a supervised classification using the \code{superClass} command from the RStoolbox package.
#' The function transforms the training features to a sf object if it wasn't already and reprojects it to the CRS of the raster input.
#'
#' @param trainFeat Training features, either an sf object or an object that can be converted to an sf object.
#' @param raster A raster object representing the spatial extent and CRS to which the training features CRS will be aligned.
#'
#' @return A pre-processed sf object of training features aligned with the CRS of the input raster.
#'
#' @details This function uses \code{sf::st_as_sf()} to convert to a sf object and \code{sf::st_transform()} to align the CRS.
#'
#' @examples
#' \dontrun{
#' # read sample sf file of the RStoolboxExtensions package
#' trainPoints <- system.file("extdata", "trainPoints.geojson", package = "RStoolboxExtensions")
#' trainPoints <- sf_sample_read(trainPoints)
#'
#' # read sample raster file of the RStoolboxExtensions package
#' Sebangau15 <- system.file("extdata", "Sebangau15.tif", package = "RStoolboxExtensions")
#' Sebangau15 <- rast_sample_read(Sebangau15)
#'
#' # run the function
#' pp_trainPoints <- pp_features(trainPoints, Sebangau15)
#'
#' # checking the class - should be sf
#' class(pp_trainPoints)
#'
#' # checking the crs - should be aligned with raster crs
#' sf::st_crs(pp_trainPoints)
#' sf::st_crs(Sebangau15)
#' }
#'
#' @import sf
#'
#' @export

pp_features <- function(trainFeat, raster) {

  #### validating the input and stop if something is wrong ####
  if (missing(trainFeat)) {
    stop("'trainFeat' input missing")
  }
  if (missing(raster)) {
    stop("'raster' input missing")
  }
  ##############################################################

  # convert training features to sf cause superClass function has problems with sp objects
  if (!inherits(trainFeat, "sf")) {
    message("trainFeat is not a sf object: Converting trainFeat to 'sf' object...")
    trainFeat <- st_as_sf(trainFeat)

    #check if the transformation worked
    if (!inherits(trainFeat, "sf")) {
      stop("trainFeat cannot be converted to a sf object. Provide a data type that can be converted to sf")
    }
    else{
      message("Conversion to sf object completed")
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

#' Calculate Spectral Indices
#'
#' Calculates specified spectral indices from multispectral raster data.
#' Supported indices include NDVI, NDWI, NDBI, and NDMI.
#'
#' @param raster A raster object containing bands for Green, Red, NIR and SWIR1. The naming has to be accurate.
#' (e.g. the red band has to be named "Red", etc.)
#' @param indices A character vector specifying which indices to calculate. Defaults to c("ndvi", "ndwi", "ndbi", "ndmi").
#'
#' @return A raster object with calculated spectral indices added as additional layers.
#'
#' @details This function calculates the specified indices using the following formulas:
#' \deqn{NDVI = \frac{NIR - Red}{NIR + Red}}
#' \deqn{NDWI = \frac{Green - NIR}{Green + NIR}}
#' \deqn{NDBI = \frac{SWIR1 - NIR}{SWIR1 + NIR}}
#' \deqn{NDMI = \frac{NIR - SWIR1}{NIR + SWIR1}}
#'
#' @examples
#' # running the example may take up to a few minutes as raster processing takes its time.
#' # I recommend to run the example in a script to get update messages in the console.
#'
#' # read sample file of the RStoolboxExtensions package
#' Sebangau15 <- system.file("extdata", "Sebangau15.tif", package = "RStoolboxExtensions")
#' Sebangau15 <- rast_sample_read(Sebangau15)
#'
#' # rename the bands to have the correct band names for indices calculation
#' Sebangau15 <- rename_bands(Sebangau15, sensor = "Landsat8", subsetting = TRUE)
#'
#' # calculate the indices of your choice
#' Sebangau15_indices <- calc_indices(Sebangau15, indices = c("ndvi", "ndbi", "ndmi"))
#'
#' names(Sebangau15_indices)
#' head(Sebangau15_indices)
#'
#' @import terra
#' @export

calc_indices <- function(raster, indices = c("ndvi", "ndwi", "ndbi", "ndmi")) {

  # Raster input validation
  if (!inherits(raster, "SpatRaster")) {
    stop("Input 'raster' must be a SpatRaster object.")
  }

  # Indices input validation
  if (!is.character(indices)) {
    stop("Parameter 'indices' must be a character vector.")
  }

  # Handle case sensibility of indices input
  indices = tolower(indices)

  # Check if all index names are valid
  valid_indices <- c("ndvi", "ndwi", "ndbi", "ndmi")
  invalid_indices <- setdiff(indices, valid_indices)

  # stop and print error message if they are not valid
  if (length(invalid_indices) > 0) {
    stop(paste("Invalid index names:", paste(invalid_indices, collapse = ", ")))
  }

  # Handle case sensitivity of raster names but store old names
  original_names <- names(raster)
  names(raster) <- tolower(names(raster))

  # create empty list and string to store index values and their respective names
  index_values <- list()
  index_names <- c()

  # Calculate the specified indices
  if ("ndvi" %in% indices) {
    if (!all(c("red", "nir") %in% names(raster))) {
      stop("Input raster must have 'red' and 'nir' bands for NDVI calculation.")
    }
    message("Calculating NDVI ...")
    index_values$NDVI <- (raster$nir - raster$red) / (raster$nir + raster$red)
    index_names <- c(index_names, "NDVI")
    message("Done")
  }
  if ("ndwi" %in% indices) {
    if (!all(c("green", "nir") %in% names(raster))) {
      stop("Input raster must have 'green' and 'nir' bands for NDWI calculation.")
    }
    message("Calculating NDWI ...")
    index_values$NDWI <- (raster$green - raster$nir) / (raster$green + raster$nir)
    index_names <- c(index_names, "NDWI")
    message("Done")
  }
  if ("ndbi" %in% indices) {
    if (!all(c("swir1", "nir") %in% names(raster))) {
      stop("Input raster must have 'swir1' and 'nir' bands for NDBI calculation.")
    }
    message("Calculating NDBI ...")
    index_values$NDBI <- (raster$swir1 - raster$nir) / (raster$swir1 + raster$nir)
    index_names <- c(index_names, "NDBI")
    message("Done")
  }
  if ("ndmi" %in% indices) {
    if (!all(c("nir", "swir1") %in% names(raster))) {
      stop("Input raster must have 'nir' and 'swir1' bands for NDMI calculation.")
    }
    message("Calculating NDMI ...")
    index_values$NDMI <- (raster$nir - raster$swir1) / (raster$nir + raster$swir1)
    index_names <- c(index_names, "NDMI")
    message("Done")
  }

  # return to the old colnames
  names(raster) <- original_names

  # Add the specified indices to the raster
  for (index_name in names(index_values)) {
    add(raster) <- index_values[[index_name]]
  }

  # name them in the raster
  names(raster)[(nlyr(raster) - length(index_values) + 1):nlyr(raster)] <- names(index_values)

  return(raster)
}



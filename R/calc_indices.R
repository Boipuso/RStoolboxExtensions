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
#' raster <- terra::rast(nir = rast(matrix(runif(100), nrow = 10)),
#'                 red = rast(matrix(runif(100), nrow = 10)),
#'                 green = rast(matrix(runif(100), nrow = 10)),
#'                 swir1 = rast(matrix(runif(100), nrow = 10)))
#' calc_indices(raster)
#'
#' @import terra
#' @export

calc_indices <- function(raster, indices = c("ndvi", "ndwi", "ndbi", "ndmi")) {

  index_values <- list()
  index_names <- c()

  if ("ndvi" %in% indices) {
    index_values$ndvi <- (raster$nir - raster$red) / (raster$nir + raster$red)
    index_names <- c(index_names, "ndvi")
  }
  if ("ndwi" %in% indices) {
    index_values$ndwi <- (raster$green - raster$nir) / (raster$green + raster$nir)
    index_names <- c(index_names, "ndwi")
  }
  if ("ndbi" %in% indices) {
    index_values$ndbi <- (raster$swir1 - raster$nir) / (raster$swir1 + raster$nir)
    index_names <- c(index_names, "ndbi")
  }
  if ("ndmi" %in% indices) {
    index_values$ndmi <- (raster$nir - raster$swir1) / (raster$nir + raster$swir1)
    index_names <- c(index_names, "ndmi")
  }

  # Add the specified indices to the raster
  for (index_name in names(index_values)) {
    add(raster) <- index_values[[index_name]]
  }

  # name them
  names(raster)[(nlyr(raster) - length(index_values) + 1):nlyr(raster)] <- names(index_values)

  return(raster)
}

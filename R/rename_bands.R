#' Rename Bands in a Raster
#'
#' Renames bands in a raster object according to the specified sensor.
#'
#' @param raster A raster object containing bands to be renamed.
#' @param sensor A character string specifying the sensor type (e.g., "Landsat8", "Sentinel2").
#' @param subsetting Logical indicating whether to subset the raster before renaming bands. Defaults to TRUE.
#'
#' @return A raster object with renamed bands.
#'
#' @details This function renames bands in a raster object based on the sensor type. Supported sensors and corresponding bands:
#' \itemize{
#'   \item Landsat 8: Blue, Green, Red, NIR, SWIR1, SWIR2
#'   \item Landsat 7: Blue, Green, Red, NIR, SWIR1, SWIR2
#'   \item Landsat 5: Blue, Green, Red, NIR, SWIR1, SWIR2
#'   \item Sentinel 2: Blue, Green, Red, NIR, SWIR1, SWIR2
#' }
#'
#' If \code{subsetting} is set to FALSE, band names are directly replaced without subsetting the raster.
#'
#' If \code{subsetting} is TRUE (default), the function subsets the raster to include only the bands specified for the sensor and then renames the bands.
#'
#' @examples
#' raster <- terra::rast(blue = rast(matrix(runif(100), nrow = 10)),
#'                          green = rast(matrix(runif(100), nrow = 10)),
#'                          red = rast(matrix(runif(100), nrow = 10)),
#'                          NIR = rast(matrix(runif(100), nrow = 10)),
#'                          SWIR1 = rast(matrix(runif(100), nrow = 10)),
#'                          SWIR2 = rast(matrix(runif(100), nrow = 10)))
#' rename_bands(raster, sensor = "Landsat8")
#'
#' @import terra
#' @export

rename_bands <- function(raster, sensor, subsetting = TRUE) {

  band_names <- c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2")

  if (sensor == "Landsat8") {
    bands <- c(2:7)
  }
  else if (sensor == "Landsat7") {
    bands <- c(1:5, 8)
  }
  else if (sensor == "Landsat5") {
    bands <- c(1:5, 7)
  }
  else if (sensor == "Sentinel2") {
    bands <- c(2:4, 8, 11:12)
  }
  else {
    stop("Sensor not supported yet.")
  }

  if(subsetting == FALSE){
    names(raster[[bands]]) <- band_names
  }
  else{
    raster <- terra::subset(raster, bands)
    names(raster) <- band_names
  }
  return(raster)
}




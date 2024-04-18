#' Rename Bands in a Raster
#'
#' Renames bands in a raster object according to the specified sensor. The function identifies original band positions for the bands
#' Blue, Green, Red, NIR, SWIR1, SWIR2 according to the sensor and names them "Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2".
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
#' If \code{subsetting} is TRUE (default), the function subsets the raster to include only the renamed bands.
#'
#' @examples
#' # read sample file of the RStoolboxExtensions package
#' Sebangau15 <- system.file("extdata", "Sebangau15.tif", package = "RStoolboxExtensions")
#' Sebangau15 <- rast_sample_read(Sebangau15)
#'
#' names(Sebangau15)
#'
#' Sebangau15 <- rename_bands(Sebangau15, sensor = "Landsat8", subsetting = TRUE)
#'
#' names(Sebangau15)
#'
#' @import terra
#' @export

rename_bands <- function(raster, sensor, subsetting = TRUE) {

  # stop if no raster or sensor input was given
  if (missing(raster)) {
    stop("raster input missing")
  }
  if (missing(sensor)) {
    stop("Please specify the sensor (e.g., 'Landsat8', 'Landsat7', 'Landsat5', or 'Sentinel2').")
  }

  # Convert sensor input to lower case to handle case input sensitivity
  sensor <- tolower(sensor)

  # create string of band names
  band_names <- c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2")

  # identify band positions for renaming
  if (sensor == "landsat8") {
    bands <- c(2:7)
  }
  else if (sensor == "landsat7") {
    bands <- c(1:5, 8)
  }
  else if (sensor == "landsat5") {
    bands <- c(1:5, 7)
  }
  else if (sensor == "sentinel2") {
    bands <- c(2:4, 8, 11:12)
  }
  else {
    stop("Sensor not supported yet. Supported sensors are Landsat5, Landsat7, Landsat8 and Sentinel2.")
  }

  # rename the bands and subset the renamed bands if specified by the user
  if(subsetting == FALSE){
    names(raster[[bands]]) <- band_names
  }
  else{
    raster <- terra::subset(raster, bands)
    names(raster) <- band_names
  }
  return(raster)
}



#' Rename Bands in a Raster
#'
#' Renames bands in a raster object according to the specified sensor. Prerequisite function to run
#' the \code{calc_indices} function to assure correct naming for indexing.
#' The function identifies original band names for the bands Blue, Green, Red, NIR, SWIR1, SWIR2
#' and names them "Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2".
#'
#' @param raster A raster object containing bands to be renamed.
#' @param sensor A character string specifying the sensor type (e.g., "Landsat8", "Sentinel2").
#' @param subsetting Logical indicating whether to subset the raster before renaming bands. Defaults to TRUE.
#'
#' @return A raster object with renamed bands.
#'
#' @details This function renames bands in a raster object based on the sensor type. Supported sensors and corresponding bands are:
#' \itemize{
#'   \item Landsat 8: "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7"
#'   \item Landsat 7: "SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B7"
#'   \item Landsat 5: "B1", "B2", "B3", "B4", "B5", "B7"
#'   \item Sentinel 2: "B2", "B3", "B4", "B8", "B11", "B12"
#' }
#'
#' If \code{subsetting} is set to FALSE, band names are directly replaced without subsetting the raster.
#'
#' If \code{subsetting} is TRUE (default), the function subsets the raster to include only the renamed bands.
#'
#' @examples
#' \dontrun{
#' # read sample file of the RStoolboxExtensions package
#' Sebangau15 <- system.file("extdata", "Sebangau15.tif", package = "RStoolboxExtensions")
#' Sebangau15 <- rast_sample_read(Sebangau15)
#'
#' # checking the names
#' names(Sebangau15)
#'
#' # applying the function
#' Sebangau15 <- rename_bands(Sebangau15, sensor = "Landsat8", subsetting = TRUE)
#'
#' # checking the names again to see if it worked
#' names(Sebangau15)
#' }
#'
#' @import terra
#'
#' @export

rename_bands <- function(raster, sensor, subsetting = TRUE) {

  #### validating the input and stop if something is wrong ####
  if (missing(raster)) {
    stop("raster input missing")
  }
  if (missing(sensor)) {
    stop("Please specify the sensor (e.g., 'Landsat8', 'Landsat7', 'Landsat5', or 'Sentinel2').")
  }
  if (!is.character(sensor)) {
    stop("Parameter 'sensor' must be a character.")
  }
  ##############################################################

  # convert sensor input to lower case to handle case sensitivity
  sensor <- tolower(sensor)

  # create string of band names for renaming
  band_names <- c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2")

  # identify band names for indexing the correct bands when renaming
  if (sensor == "landsat8") {
    bands <- c("SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7")
  }
  else if (sensor == "landsat7") {
    bands <- c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B7")
  }
  else if (sensor == "landsat5") {
    bands <- c("B1", "B2", "B3", "B4", "B5", "B7")
  }
  else if (sensor == "sentinel2") {
    bands <- c("B2", "B3", "B4", "B8", "B11", "B12")
  }
  else {
    stop("Sensor not supported yet. Supported sensors are Landsat5, Landsat7, Landsat8 and Sentinel2.")
  }

  # rename the bands and subset the renamed bands if specified by the user
  if(subsetting == FALSE){
    # iterate over each pair of bands and their new names
    for (i in seq_along(bands)) {
      # rename the band
      names(raster)[names(raster) == bands[i]] <- band_names[i]
    }
  }
  else{
    raster <- terra::subset(raster, bands)
    names(raster) <- band_names
  }
  return(raster)
}




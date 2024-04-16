#' Renaming Band Layers of a Raster
#'
#' Function to rename (and subset) the band layers
#'     "blue", "green", "red", "nir", "swir1" and "swir2"
#'
#' @param raster A raster object. The band names of this raster will be renamed based on their
#' original column position. If the band positions were altered the naming will be incorrect.
#' @param sensor The sensor your raster scene was generated with. So far Landsat 5-8 and Sentinel2
#' sensors are supported.
#' @param subsetting a logical evaluating to TRUE or FALSE indicating whether the raster scene should
#' be subsetted to the renamed bands ("blue", "green", "red", "nir", "swir1", "swir2")
#' @return Returns the input raster with the renamed band layers. If subsetting = TRUE all other
#' bands than the renamed ones are removed.
#'
#' @importFrom terra subset
#'
#' @examples
#' \dontrun{
#' your_raster <- terra::rast('path_to_your_raster')
#' your_raster_renamed <- rename_bands_fun(raster = your_raster, sensor = "Landsat8", subsetting = TRUE)
#' }
#'
#'
#' @export

renameBands <- function(raster, sensor, subsetting = TRUE) {

  band_names <- c("blue", "green", "red", "nir", "swir1", "swir2")

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




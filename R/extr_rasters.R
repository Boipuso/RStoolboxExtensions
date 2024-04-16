#' Crops and Mask Rasters to (Multi-)polygons
#'
#' Crops and masks a raster to a list of (multi-)polygons and saves the masked rasters as GeoTIFF or other raster formats.
#'
#' @param raster A raster object to be cropped and masked.
#' @param polygon_list A list of (multi-)polygon sf objects, each representing a landcover class.
#' @param out_dir The directory where the output masked rasters will be saved. Defaults to "output_masked".
#' @param overwrite Logical indicating whether to overwrite existing files if they already exist. Defaults to TRUE.
#' @param datatype The data type of the output raster files. Defaults to "tif".
#'
#' @return A list of cropped and masked raster objects, each corresponding to an entry of the \code{polygon_list}.
#'
#' @examples
#' # Load required packages
#' library(sf)
#' library(terra)
#'
#' # Create a raster object
#' raster <- rast(nrow=100, ncol=100)
#' values(raster) <- matrix(runif(10000), nrow=100)
#'
#' # Create multipolygon sf objects representing landcover classes
#' polygons_list <- list(class1 = st_buffer(st_sfc(st_point(c(10, 10))), 10),
#'                       class2 = st_buffer(st_sfc(st_point(c(50, 50))), 10))
#'
#' # Extract and mask rasters
#' extr_rasters(raster, polygons_list)
#'
#' @import terra
#' @import sf
#' @export

extr_rasters <- function(raster,
                         polygon_list,
                         out_dir = "output_masked",
                         overwrite = TRUE,
                         datatype = "tif") {

  # create empty list to store the results
  masked_raster_list <- list()

  # Create a directory to save the polygons if it doesn't exist already
  if (!dir.exists(out_dir)) {
    dir.create(out_dir) #outdir als variable in function
  }

  for (name in names(polygon_list)) {
    # Convert data frame to sf object
    polygon_sf <- st_as_sf(polygon_list[[name]])

    # Crop and mask the raster
    cropped_raster <- crop(raster, polygon_sf)
    masked_raster <- mask(cropped_raster, polygon_sf)

    # Add raster to the list with integer names
    masked_raster_list[[name]] <- masked_raster

    # Save raster as geopackage
    filename <- file.path(out_dir, paste0("/", name, ".", datatype))

    # Writing the raster
    writeRaster(masked_raster, filename, overwrite = overwrite)
  }
  return(masked_raster_list)
}

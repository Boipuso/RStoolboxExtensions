#' Crops and Masks a Raster to a list of (Multi-)polygons
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
#' # loading sample multitpolygons from the RStoolboxExtensions package
#' afforestation <- system.file("extdata", "afforestation.gpkg", package = "RStoolboxExtensions")
#' afforestation <- sf_sample_read(afforestation)
#' deforestation <- system.file("extdata", "deforestation.gpkg", package = "RStoolboxExtensions")
#' deforestation <- sf_sample_read(deforestation)
#' nonforest <- system.file("extdata", "nonforest.gpkg", package = "RStoolboxExtensions")
#' nonforest <- sf_sample_read(nonforest)
#' forest <- system.file("extdata", "forest.gpkg", package = "RStoolboxExtensions")
#' forest <- sf_sample_read(forest)
#'
#' # load classified sample image of the RStoolboxExtensions package
#' class_img <- system.file("extdata", "class_img.tif", package = "RStoolboxExtensions")
#' class_img <- rast_sample_read(class_img)
#'
#' # put them together in a list
#' class_polygons <- list(c(afforestation, deforestation, nonforest, forest))
#'
#' # apply the function
#' class_rasters <- extr_rasters(class_img, class_polygons, out_dir = "test_output_rasters")
#'
#' View(class_rasters)
#'
#' @import terra
#' @import sf
#' @export

extr_rasters <- function(raster,
                         polygon_list,
                         out_dir = "output_masked",
                         overwrite = TRUE,
                         datatype = "tif") {

  # Raster input validation
  if (!inherits(raster, "SpatRaster")) {
    stop("Input 'raster' must be a SpatRaster object.")
  }

  # Check if polygon_list is a list of polygons or multipolygons
  if (!all(sapply(polygon_list, function(x) inherits(x, c("Polygon", "MultiPolygon"))))) {
    stop("Input 'polygon_list' must be a list of polygons or multipolygons.")
  }

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

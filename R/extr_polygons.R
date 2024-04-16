#' Extract Landcover Classes as Multipolygons from Classified Image
#'
#' Extracts landcover classes from a classified image as multipolygons and saves them as shapefiles or GeoPackages.
#'
#' @param class_img A raster object representing the classified image.
#' @param data_format The format of the output data files. Can be ".shp" for shapefiles or ".gpkg" for GeoPackages. Defaults to ".gpkg".
#' @param class_col The name of the column in \code{class_img} containing the class labels. Defaults to "class".
#' @param append Logical indicating whether to append to existing data files if they already exist. Defaults to FALSE.
#' @param out_dir The directory where the output files will be saved. Defaults to "output_polygons".
#'
#' @return A list of multipolygon sf objects, each representing a landcover class extracted from the classified image.
#'
#' @details This function extracts landcover classes from a classified image represented by a raster object. It loops over unique class labels in the specified column (\code{class_col}) of the raster and converts each class into a binary raster. Then, it converts the binary raster into polygons using the \code{as.polygons} function from the \code{sp} package. The polygons are further converted into sf objects using \code{st_as_sf} from the \code{sf} package and saved as shapefiles or GeoPackages in the specified output directory (\code{out_dir}). The function returns a list of multipolygon sf objects, with each object representing a landcover class.
#'
#' @examples
#' # Load required packages
#' library(sf)
#' library(terra)
#'
#' # Create a classified image raster
#' class_img <- rast(matrix(sample(1:3, 100, replace = TRUE), nrow = 10))
#'
#' # Extract polygons
#' extr_polygons(class_img)
#'
#' @import terra
#' @import sf
#' @export

extr_polygons <- function(class_img,
                                 data_format = ".gpkg",
                                 class_col = "class",
                                 append = FALSE,
                                 out_dir = "output_polygons") {

  # Get unique values in the raster
  unique_values <- unique(class_img[[class_col]])

  # Remove NA values if present
  unique_values <- unique_values[!is.na(unique_values)]

  # create an empty list to store polygons
  polygons_list <- list()

  # Create a directory to save the polygons if it doesn't exist already
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # Loop over unique values (classes)
  for (value in unique_values) {
    # Create a binary raster for each class
    binary_raster <- ifel(class_img[[class_col]] == value, 1, NA_integer_)

    # Convert binary raster to polygons
    polygons <- as.polygons(binary_raster)

    # Convert polygons to sf object
    sf_polygons <- st_as_sf(polygons)

    # Save the polygons as shapefile
    shapefile_name <- paste0(out_dir,"/", as.character(value),"_polygon", data_format)
    st_write(sf_polygons, shapefile_name, append = append)

    # Add polygons to the list
    polygons_list[[as.character(value)]] <- sf_polygons
  }

  return(polygons_list)

}

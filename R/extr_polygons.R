#' Extract Landcover Classes as (Multi-)polygons from Classified Image
#'
#' Extracts landcover classes from a classified image as (multi-)polygons and optionally saves them in a format specified by the user.
#'
#' @param class_img A raster object representing the classified image.
#' @param saveLoc Logical indicating whether to store the output polygons in a local folder. Defaults to FALSE:
#' @param datatype The format of the output data files. Can be ".shp" for shapefiles or ".gpkg" for GeoPackages. Defaults to ".gpkg".
#' @param class_col The name of the column in \code{class_img} containing the class labels. Defaults to "class".
#' @param append Logical indicating whether to append to existing data files if they already exist. Defaults to FALSE.
#' @param out_dir The directory where the output files will be saved. Defaults to "output_polygons".
#'
#' @return A list of (multi-)polygon sf objects, each representing a landcover class extracted from the classified image.
#'
#' @details This function extracts landcover classes from a classified image represented by a raster object.
#' It loops over unique class labels in the specified column (\code{class_col}) of the raster and converts each class into a binary raster.
#' Then, it converts the binary raster into polygons using the \code{as.polygons} function from the \code{sp} package.
#' The polygons are further converted into sf objects using \code{st_as_sf} from the \code{sf} package and optionally saved as shapefiles or GeoPackages in the specified output directory (\code{out_dir}).
#' The function returns a list of multipolygon sf objects, with each object representing a landcover class.
#'
#' @examples
#' \dontrun{
#' # load classified sample image of the RStoolboxExtensions package
#' class_img <- system.file("extdata", "class_img.tif", package = "RStoolboxExtensions")
#' class_img <- rast_sample_read(class_img)
#'
#' # apply the function
#' class_polygons <- extr_polygons(class_img, out_dir = "test_output_polygons")
#'
#' # the function returns a list of the (multi-)polygons
#' View(class_polygons)
#'
#' }
#'
#' @import terra
#' @import sf
#' @export

extr_polygons <- function(class_img,
                          saveLoc = TRUE,
                          datatype = "gpkg",
                          class_col = "class",
                          append = FALSE,
                          out_dir = "class_polygons") {

  #### validating the input and stop if something is wrong ####
  if (missing(class_img)) {
    stop("'class_img' input missing")
  }
  if (!is.character(datatype)) {
    stop("Parameter 'datatype' must be a character.")
  }
  if (!is.character(class_col)) {
    stop("Parameter 'class_col' must be a character.")
  }
  if (!is.character(out_dir)) {
    stop("Parameter 'out_dir' must be a character.")
  }
  # check if 'class_col' exists in the raster colnames
  if (!(class_col %in% names(class_img))) {
    stop("Specified 'class_col' does not exist in the raster attributes. Please provide a valid column name.")
  }
  ##############################################################

  # get unique values in the raster to loop over them
  unique_values <- unique(class_img[[class_col]])

  # remove NA values if present
  unique_values <- unique_values[!is.na(unique_values)]

  # create an empty list to store polygons while looping
  polygons_list <- list()

  # loop over unique values (classes) to create binary raster for each class and convert them to polygons
  for (value in unique_values) {
    # create a binary raster for each class
    binary_raster <- ifel(class_img[[class_col]] == value, 1, NA_integer_)

    # convert binary raster to polygons
    polygons <- as.polygons(binary_raster)

    # convert polygons to sf object
    sf_polygons <- st_as_sf(polygons)

    # add polygons to the list for the environment output
    polygons_list[[as.character(value)]] <- sf_polygons

    # if specified by the user, save the polygons in a local output folder
    if(saveLoc == TRUE) {
      # create a directory to save the polygons if it doesn't exist already
      if (!dir.exists(out_dir)) {
        dir.create(out_dir)
      }
      file_name <- paste0(out_dir,"/", as.character(value),".", datatype)
      st_write(sf_polygons, file_name, append = append)
    }
  }

  return(polygons_list)

}

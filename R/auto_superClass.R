#' Automate Supervised Classification Process
#'
#' Automates the process of conducting a supervised classification, including pre-processing of raster data and training features, model training, and classification.
#' Offers possibility to conduct a change detection by including a second raster input (assuming training features for change detection).
#'
#' @param img A raster object representing the input image for classification.
#' @param img2 Optional. A second raster object representing a second input image for change detection.
#' @param train_features A data frame containing the training features with locations.
#' @param responseCol The name of the column in \code{train_features} containing the response variable.
#' @param sensor The sensor type of the input raster(s). Used for band renaming if necessary.
#' @param subsetting Logical indicating whether to perform subsetting of bands. Defaults to TRUE.
#' @param rename_bands Logical indicating whether to rename bands based on sensor type. Defaults to TRUE.
#' @param calc_indices Logical indicating whether to calculate spectral indices. Defaults to FALSE.
#' @param indices Character vector specifying the spectral indices to be calculated if \code{calc_indices} is TRUE. Defaults to c("ndvi", "ndwi", "ndbi", "ndmi").
#' @param model The classification model to be used. Defaults to "rf" (random forest).
#' @param nSamples Integer (polygon based). Number of samples per land cover class. If NULL all pixels covered by training polygons are used (memory intensive!). Ignored if train_features consists of POINTs. Defaults to 100.
#' @param nSamplesV Integer. Number of validation samples per land cover class. If NULL all pixels covered by validation polygons are used (memory intensive!). Ignored if train_features consists of POINTs. Defaults to 50.
#' @param trainPartition The proportion of points or polygons to be used for training, rest is used for validation. Defaults to 0.66.
#' @return A list containing the classified image, model fit information, pre-processed raster(s), and pre-processed training features.
#'
#' @details This function automates the entire process of conducting a supervised classification. It performs pre-processing of the input raster(s) (consisting of the \code{rename_bands} and \code{calc_indices} functions) and training features (consisting of the \code{pp_features} function), model training using the specified classification algorithm, and classification of the input raster(s) using the trained model. Depending on the geometry type of the training features (points or polygons), the appropriate training function (\code{points_superClass} or \code{superClass}) is called. The function returns a list containing the classified image, model fit information, pre-processed raster(s), and pre-processed training features for further analysis. The function deploys the superClass function from the RStoolbox package for the training of the model and the classification itself.
#'
#' @examples
#' # running the example may take up to several minutes as raster processing takes its time.
#' # I recommend to run the example in a script to get update messages in the console.
#'
#' # read sample raster files of the RStoolboxExtensions package
#' Sebangau15 <- system.file("extdata", "Sebangau15.tif", package = "RStoolboxExtensions")
#' Sebangau15 <- rast_sample_read(Sebangau15)
#' Sebangau23 <- system.file("extdata", "Sebangau23.tif", package = "RStoolboxExtensions")
#' Sebangau23 <- rast_sample_read(Sebangau23)
#'
#' # read sample sf file of the RStoolboxExtensions package
#' trainPoints <- system.file("extdata", "trainPoints.geojson", package = "RStoolboxExtensions")
#' trainPoints <- sf_sample_read(trainPoints)
#'
#' # apply the function
#' asC_output <- auto_superClass(img = Sebangau15,
#'                               img2 = Sebangau23,
#'                               train_features = trainPoints,
#'                               responseCol = "landcover",
#'                               sensor = "Landsat8",
#'                               calc_indices = TRUE,
#'                               indices = c("ndvi", "ndbi"))
#'
#' # check that the generated list contains all 4 output objects
#' names(asC_output)
#'
#' # extract objects
#' class_img <- asC_output$superClass_img
#' pp_features <- asC_output$pp_features
#' pp_raster <- asC_output$pp_raster
#' accuracy <- asC_output$modelFit
#'
#' # view the results
#' terra::plot(class_img)
#' View(accuracy)
#'
#' @import terra
#' @import sf
#' @import RStoolbox
#'
#' @export

# random forest function that extracts relevant bands for classification, calculates indices
# can work with two rasters for change detection and computes the classification raster

auto_superClass <- function(img,
                  img2 = NULL,
                  train_features,
                  responseCol,
                  sensor,
                  subsetting = TRUE,
                  rename_bands = TRUE,
                  calc_indices = FALSE,
                  indices = c("ndvi", "ndwi", "ndbi", "ndmi"),
                  model = "rf",
                  nSamples = 100,
                  nSamplesV = 50,
                  trainPartition = 0.66) {

  ############################
  ######Validate inputs#######
  ############################

  # img input validation
  if (!inherits(img, "SpatRaster")) {
    stop("Input 'img' must be a SpatRaster object.")
  }

  # img2 input validation
  if (!is.null(img2) & !inherits(img2, "SpatRaster")) {
    stop("Input 'img2' must be a SpatRaster object.")
  }


  #############################################
  ###applying raster preprocessing functions###
  #############################################

  # check which functions the user wants to apply and apply them to the raster or both rasters if 2 rasters are provided
  if (is.null(img2)) {

    message("Start pre-processing the img ...")

    if (rename_bands == TRUE) {
      img_bands <- rename_bands(raster = img, sensor = sensor, subsetting = subsetting)
    }
    else if (rename_bands == FALSE) {
      img_bands <- img
    }

    if (calc_indices == TRUE) {
      input_raster <- calc_indices(raster = img_bands, indices = indices)
    }
    else if (calc_indices == FALSE) {
      input_raster <- img_bands
    }
    message("Finished pre-processing the img.")
  }

  # if provided process the second img as well and add suffixes to its band_names to avoid duplicate naming
  # stack the images for change detection band information
  else if (!is.null(img2)){

    message("Start pre-processing img and img2 ...")

    if (rename_bands == TRUE) {
      img_bands <- rename_bands(raster = img, sensor = sensor, subsetting = subsetting)
      img2_bands <- rename_bands(raster = img2, sensor = sensor, subsetting = subsetting)
    }
    else if (rename_bands == FALSE) {
      img_bands <- img
      img2_bands <- img2
    }

    if (calc_indices == TRUE) {
      input_raster_1 <- calc_indices(raster = img_bands, indices = indices)
      input_raster_2 <- calc_indices(raster = img2_bands, indices = indices)
      names(input_raster_2) <- paste0(names(input_raster_2), "_2")
      input_raster <- c(input_raster_1, input_raster_2)
    }
    else if (calc_indices == FALSE) {
      input_raster_1 <- img_bands
      input_raster_2 <- img2_bands
      names(input_raster_2) <- paste0(names(input_raster_2), "_2")
      input_raster <- c(input_raster_1, input_raster_2)
    }

    message("Finished pre-processing img and img2.")
  }

  ##############################################
  ###applying feature preprocessing functions###
  ##############################################

  message("Start pre-processing the train_features ...")
  pp_features <- pp_features(trainFeat = train_features,
                             raster = img)
  message("Finished pre-processing the train_features.")

  ############################################################
  ###conduct superClass function depending on geometry type###
  ############################################################

  geometry_type <- st_geometry_type(pp_features)

  # Check the geometry type to chose the correct superClass function and apply it
  message("Start training the classifier ...")

  if (all(geometry_type == "POINT")) {
    superClass <- points_superClass(img = input_raster,
                                    trainPoints = pp_features,
                                    responseCol = responseCol,
                                    trainPartition = trainPartition,
                                    model = model)
  } else if (all(geometry_type == "POLYGON")) {
    superClass <- superClass(img = input_raster,
                             trainData = pp_features,
                             responseCol = responseCol,
                             nSamples = nSamples,
                             trainPartition = trainPartition,
                             nSamplesV = nSamplesV,
                             model = model)
  } else {
    stop("The training data consists of mixed geometry types. Provide single type training data (Polygons OR Points).")
  }

  message("Finished training the classifier.")

  ##################################
  ####Conduct the classification####
  ##################################

  # extract the trained model from the superClass output list
  superClass_model <- superClass$model

  # conduct the classification
  message("Start conducting the classification ...")
  superClass_img <- terra::predict(input_raster, superClass_model, na.rm = TRUE)
  message("Finished conducting the classification .")

  # create a list containing the classified image and the accuracy assessment
  output <- list(superClass_img = superClass_img,
                 modelFit = superClass$modelFit,
                 pp_raster = input_raster,
                 pp_features = pp_features)
  return(output)
}

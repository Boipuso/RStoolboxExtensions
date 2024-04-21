#' Supervised Classification using Point Training Data
#'
#' Essentially the \code{superClass()} function from the RStoolbox but includes the possibility to partition point training data into training- and validation sets.
#'
#' @param img A raster object. Typically remote sensing imagery, which is to be classified.
#' @param trainPoints A sf object containing the training points with locations.
#' @param responseCol The name of the column in \code{trainPoints} containing the response variable.
#' @param trainPartition Optional. A numeric value between 0 and 1 specifying the proportion of points to be used for training. If NULL, all points are used for training.
#' @param model The supervised classification model to be used. Defaults to "rf" (random forest).
#' @param tuneLength The number of parameter settings to explore for model tuning. Defaults to 3.
#' @param kfold Integer. Number of cross-validation resamples during model tuning.Defaults to 5
#' @param mode The mode of the model type, either "classification" or "regression". Defaults to "classification".
#' @param predict Logical. Produce a map (TRUE, default) or only fit and validate the model (FALSE). Defaults to TRUE.
#' @param predType Character. Type of the final output raster. Either "raw" for class predictions or "prob" for class probabilities. Class probabilities are not available for all classification models. Defaults to "raw".
#' @param filename Path to output file (optional). If NULL, standard raster handling will apply, i.e. storage either in memory or in the raster temp directory.
#' @param overwrite Logical indicating whether to overwrite an existing file if \code{filename} is provided. Defaults to TRUE.
#'
#' @return A trained classification model.
#'
#' @details This function trains a supervised classification model both for classification and regression mode using point training data.
#' If \code{trainPartition} is provided, the data is split into training and validation sets according to the specified proportion. Look up ?superClass() for documentation of the base function.
#'
#' @examples
#' \dontrun{
#' # running the example may take up to several minutes as raster processing takes its time.
#'
#' # read sample sf file of the RStoolboxExtensions package
#' trainPoints <- system.file("extdata", "trainPoints.geojson", package = "RStoolboxExtensions")
#' trainPoints <- sf_sample_read(trainPoints)
#'
#' # read sample raster file of the RStoolboxExtensions package
#' Sebangau15 <- system.file("extdata", "Sebangau15.tif", package = "RStoolboxExtensions")
#' Sebangau15 <- rast_sample_read(Sebangau15)
#'
#' # pre-process the training points
#' pp_trainPoints <- pp_features(trainPoints, Sebangau15)
#'
#' # subset relevant raster bands
#' subset_Sebangau15 <- rename_bands(Sebangau15, sensor = "Landsat8", subsetting = TRUE)
#'
#' # train model with data partitioning
#' superClass <- points_superClass(subset_Sebangau15, pp_trainPoints, "landcover", trainPartition = 0.66)
#'
#' # checking the output
#' names(superClass)
#' head(superClass)
#'
#' }
#'
#' @import RStoolbox
#' @import stats
#' @export

points_superClass <- function(img,
                              trainPoints,
                              responseCol,
                              trainPartition = NULL,
                              model = "rf",
                              tuneLength = 3,
                              kfold = 5,
                              mode = "classification",
                              predict = TRUE,
                              predType = "raw",
                              filename = NULL,
                              overwrite = TRUE) {

  #### validating the input and stop if something is wrong ####
  if (missing(img)) {
    stop("'img' input missing")
  }
  if (missing(trainPoints)) {
    stop("'trainPoints' input missing")
  }
  if (missing(responseCol)) {
    stop("'responseCol' input missing")
  }
  ##############################################################

  # set valPoints to NULL if trainPartition is NULL to signalise to the superclass function
  # that no validation is wanted by the user
  if(is.null(trainPartition)) {
    in_trainPoints <- trainPoints
    valPoints <- NULL
  }

  # split features between training and validation features if trainPartition is not NULL
  else {
    trainPoints$random <- runif(nrow(trainPoints))
    in_trainPoints <- subset(trainPoints, random < trainPartition)
    valPoints <- subset(trainPoints, random >= trainPartition)
  }

  # train the model using the superClass function from the RStoolbox
  trained_model <- superClass(img = img,
                              trainData = in_trainPoints,
                              valData = valPoints,
                              model = model,
                              responseCol = responseCol,
                              tuneLength = tuneLength,
                              kfold = kfold,
                              mode = mode,
                              predict = predict,
                              predType = predType,
                              filename = filename,
                              overwrite = overwrite)
  return(trained_model)
}


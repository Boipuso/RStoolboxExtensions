#' Supervised Classification using Point Training Data
#'
#' Essentially the superClass function from the RStoolbox but includes the possibility to partition point training data into training and validation sets.
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
#' @details This function trains a supervised classification model both for classification and regression mode using point training data. If \code{trainPartition} is provided, the data is split into training and validation sets according to the specified proportion. Look up ?superClass() for documentation of the base function.
#'
#' @examples
#' # Load required packages
#' library(RStoolbox)
#'
#' # Load example data
#' data("landsat")

#' # Create point training data
#' trainPoints <- data.frame(x = runif(100), y = runif(100), class = sample(c("class1", "class2"), 100, replace = TRUE))

#' # Train model using all points
#' points_superClass(landsat, trainPoints, "class")

#' # Train model with data partitioning
#' points_superClass(landsat, trainPoints, "class", trainPartition = 0.7)
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


  if(is.null(trainPartition)) {
    in_trainPoints <- trainPoints
    valPoints <- NULL
  }

  # Split features between training and validation features if trainPartition is not NULL
  else {
    trainPoints$random <- runif(nrow(trainPoints))
    in_trainPoints <- subset(trainPoints, random < trainPartition)
    valPoints <- subset(trainPoints, random >= trainPartition)
  }

  # Train the model
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

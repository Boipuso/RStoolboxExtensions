
# function to download sample data for the package from cloud services
# the download works only correctly for the geojson files. The other files are downloaded but damaged.


# .onLoad <- function(libname, pkgname) {
#
#   # Define a function to download the data
#   download_data <- function(url, destfile) {
#     download.file(url, destfile = destfile, method = "auto")
#   }
#
#   # List of file URLs and corresponding filenames
#    files_to_download <- list(
#     "afforestation.gpkg" = "https://drive.google.com/uc?export=download&id=1eP9b1ZUkyMhzqTbJXBdKloQIQMKVMZCj",
#     "deforestation.gpkg" = "https://drive.google.com/uc?export=download&id=1qwRf_GFZbMzsIXp6T-NMCeDym5zLgXXF",
#     "forest.gpkg" = "https://drive.google.com/uc?export=download&id=1pYiRLYo_bDNt1-Sb3YSj9hrPpoS6HRMK",
#     "nonforest.gpkg" = "https://drive.google.com/uc?export=download&id=1SC9Wa2v4YK1CqpxPbH76drJU_e2ZgMDl",
#     "trainPoints.geojson" = "https://drive.google.com/uc?export=download&id=1Pr5wfzFOB-cZed2jZhBnmvXTzJAO2Knc",
#     "trainPolygons.geojson" = "https://drive.google.com/uc?export=download&id=1WBmg4liu9FRBGT1XCkiMBoHDNj4vwUDJ",
#     "Sebangau15.tif" = "https://drive.google.com/uc?export=download&id=1b7HoFh0cvwwwVJagNjnsmacLqyMImjFc",
#     "Sebangau23.tif" = "https://drive.google.com/uc?export=download&id=1BRbFncsSfRwMAMrbASploBh8XftTDMsp",
#     "class_img.tif" = "https://drive.google.com/uc?export=download&id=1ct7NgIgVfsB4JvgNVOVLd-7LYHl2ktex"
#    )
#
#   # Loop over each file URL and filename pair
#   for (filename in names(files_to_download)) {
#     url <- files_to_download[[filename]]
#     systemfile <- system.file("extdata", package = "RStoolboxExtensions")
#     destfile <- paste0(systemfile, "/", filename)
#
#     # Check if the data file exists, if not, download it
#     if (!file.exists(destfile)) {
#       download_data(url, destfile)
#     }
#   }
# }



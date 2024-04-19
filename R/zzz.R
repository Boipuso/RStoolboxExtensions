
# function to download sample data for the package from cloud services

.onLoad <- function(libname, pkgname) {
  # Define a function to download the data
  download_data <- function() {
    # List of file URLs and corresponding filenames
    files_to_download <- list(
      "https://drive.google.com/file/d/1eP9b1ZUkyMhzqTbJXBdKloQIQMKVMZCj/view?usp=sharing" = "afforestation.gpkg",
      "https://drive.google.com/file/d/1qwRf_GFZbMzsIXp6T-NMCeDym5zLgXXF/view?usp=sharing" = "deforestation.gpkg",
      "https://drive.google.com/file/d/1pYiRLYo_bDNt1-Sb3YSj9hrPpoS6HRMK/view?usp=sharing" = "forest.gpkg",
      "https://drive.google.com/file/d/1SC9Wa2v4YK1CqpxPbH76drJU_e2ZgMDl/view?usp=sharing" = "nonforest.gpkg",
      "https://drive.google.com/file/d/1Pr5wfzFOB-cZed2jZhBnmvXTzJAO2Knc/view?usp=sharing" = "trainPoints.geojson",
      "https://drive.google.com/file/d/1WBmg4liu9FRBGT1XCkiMBoHDNj4vwUDJ/view?usp=sharing" = "trainPolygons.geojson",
      "https://drive.google.com/file/d/1b7HoFh0cvwwwVJagNjnsmacLqyMImjFc/view?usp=sharing" = "Sebangau15.tif",
      "https://drive.google.com/file/d/1BRbFncsSfRwMAMrbASploBh8XftTDMsp/view?usp=sharing" = "Sebangau23.tif",
      "https://drive.google.com/file/d/1ct7NgIgVfsB4JvgNVOVLd-7LYHl2ktex/view?usp=sharing" = "class_img.tif"
    )

    # Loop over each file URL and filename pair
    for (url in names(files_to_download)) {
      filename <- files_to_download[[url]]
      download.file(url, destfile = system.file("extdata", filename, package = pkgname), method = "auto")
    }
  }

  # Check if any of the data files exist, if not, download them
  if (any(!file.exists(file.path(system.file("extdata", package = pkgname), files_to_download)))) {
    download_data()
  }
}


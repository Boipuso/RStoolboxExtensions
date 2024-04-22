
<!-- README.md is generated from README.Rmd. Please edit that file -->

## RStoolboxExtensions

<!-- badges: start -->
<!-- badges: end -->

This package offers functions to automise pre-processing steps needed to
conduct unsupervised classifications with the “superClass()” function of
the RStoolbox package. Additionally, post-processing functions to export
landcover classes as (multi-)polygons and to mask, crop and export a
raster to the respective landcover classes of the (multi-)polygons are
included.

## Installation

You can install the development version of RStoolboxExtensions like so:

``` r
install_github("Boipuso/RStoolboxExtensions")
```

## Example

This is a basic example which shows you how to solve a common problem.
Let’s say we want to conduct a supervised classification to map
landcover change. The functions integrated in this package allow for
fast pre-processing of the raster(s) and the training features. For this
example we use the sample data of this package for the Sebangau
Nationalpark in Borneo. You can use the sample_read functions to load
the sample data from this package. The help documents of the sample_read
functions provide everything you need to know abou the sample data and
how to access it.

``` r
# load the package
library(RStoolboxExtensions)

# retrieve the sample raster data from the package
# the sample data consists of 2 rasters of the Sebangau Nationalpark in Borneo from the years 2015 #and 2023 and training points for change detection analyses.
 Sebangau15 <- system.file("extdata", "Sebangau15.tif", package = "RStoolboxExtensions")
 Sebangau15 <- rast_sample_read(Sebangau15)
 Sebangau23 <- system.file("extdata", "Sebangau23.tif", package = "RStoolboxExtensions")
 Sebangau23 <- rast_sample_read(Sebangau23)

 # read sample sf file of the RStoolboxExtensions package
 trainPolygons <- system.file("extdata", "trainPolygons.geojson", package = "RStoolboxExtensions")
 trainPolygons <- sf_sample_read(trainPolygons)
#> Reading layer `trainPolygons' from data source 
#>   `C:\Users\henni\AppData\Local\R\win-library\4.3\RStoolboxExtensions\extdata\trainPolygons.geojson' 
#>   using driver `GeoJSON'
#> Simple feature collection with 80 features and 2 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 113.4024 ymin: -2.702268 xmax: 113.4924 ymax: -2.625239
#> Geodetic CRS:  WGS 84
```

Make sure you know which sensor your raster(s) originate from and which
column in your training features stores the information of the class
assignment.

``` r
# check the colnames of the training features
names(trainPolygons)
#> [1] "id"        "landcover" "geometry"
```

It’s a good idea to change the landcover the descriptions to meaningful
characters.

``` r
# reassigning the landcover values to be a character description of the class instead of numerical values
  landcover <- c('forest','nonforest','afforestation','deforestation')
  trainPolygons$landcover <- landcover[trainPolygons$landcover]
```

You can now use the ‘auto_superClass()’ function to automate the raster
and feature pre-processing. Raster pre-processing options include
subsetting to relevant bands for classification, indices calculation,
and inclusion of a second raster to do conduct a change detection
classification (given according training features). The function works
for both training points or polygons and adjust their object class and
CRS for the classification. The classification itself is conducted via
the superClass() function from the RStoolbox or the slightly adjusted
points_superClass() function from this package (depending on the
geometry type of the training features). Make sure to check whether the
training features and the raster(s) cover the same area and check your
preferred settings.

``` r
# it's good practice to set a seed for reproducability
set.seed(123)

# specify your input features
# here we choose that we do want to rename and subset the bands (renaming the bands is prerequisite #for calculating indices) and also we want to calculate the indices NDVI and NDWI to improve our #classification results
asC <- auto_superClass(img = Sebangau15, 
                       img2 = Sebangau23, 
                       train_features = trainPolygons,
                       responseCol = "landcover", 
                       rename_bands = TRUE, 
                       subsetting = TRUE, 
                       sensor = "Landsat8", 
                       calc_indices = TRUE, 
                       indices = c("ndvi", "savi"),
                       L = 0.5,
                       trainPartition = 0.66
                       )
#> Warning: Paket 'caret' wurde unter R Version 4.3.2 erstellt
#> Warning: Paket 'ggplot2' wurde unter R Version 4.3.3 erstellt
```

You should receive an output list containing 4 elements including the
classified raster, the model accuracy, as well as the pre-processed
raster(s) and features. You can access them from the generated output
list.

``` r
# retrieve the outputs
accuracy <- asC$modelFit
class_img <- asC$superClass_img
pp_features <- asC$pp_features
pp_raster <- asC$pp_raster
```

Let’s plot the classified raster.

``` r
terra::plot(class_img)
```

<img src="man/figures/README-plotting classified raster-1.png" width="100%" />

And check the validation of the model to make sure the results are
sensible.

``` r
accuracy
#> [[1]]
#>   TrainAccuracy TrainKappa method
#> 1     0.9658384  0.9456681     rf
#> 
#> [[2]]
#> Cross-Validated (5 fold) Confusion Matrix 
#> 
#> (entries are average cell counts across resamples)
#>  
#>                Reference
#> Prediction      afforestation deforestation forest nonforest
#>   afforestation          46.2           1.0    0.0       0.0
#>   deforestation           1.0          10.8    0.0       0.0
#>   forest                  0.0           0.0   35.0       1.2
#>   nonforest               0.0           0.0    0.2       4.2
#>                             
#>  Accuracy (average) : 0.9659
```

You can also check whether your raster was truly subsetted by calling
the names of the raster. It will now contain 2 layers for every band,
because sebangau15 and Sebangau23 were stacked by the auto_superClass()
function.

``` r
names(pp_raster)
#>  [1] "Blue"    "Green"   "Red"     "NIR"     "SWIR1"   "SWIR2"   "NDVI"   
#>  [8] "SAVI"    "Blue_2"  "Green_2" "Red_2"   "NIR_2"   "SWIR1_2" "SWIR2_2"
#> [15] "NDVI_2"  "SAVI_2"
```

Let’s verify whether the indices were calculated correctly by plotting
the NDVI

``` r
terra::plot(pp_raster$NDVI)
```

<img src="man/figures/README-plotting ndvi-1.png" width="100%" />

When you are happy with the results, you can extract your landcover
classes as (multi-)polygons and optionally store them locally for
further processing in another software using the extr_polygons()
function. Make sure to specify saveLoc = TRUE to export the output
polygons.

``` r
polygon_list <- extr_polygons(class_img, saveLoc = FALSE, datatype = "gpkg", out_dir = "myPolygons")
```

Let’s plot a polygon layer to validate the results.

``` r
terra::plot(polygon_list$forest)
```

<img src="man/figures/README-plotting polygon-1.png" width="100%" />

You can further use the extr_rasters() function to crop and mask a
raster to the (multi-)polygon list you received from extr_polygons().
The output will be cropped and masked rasters for every list entry of
the (multi-)polygon list and optionally exported to a directory of your
choice. Again, choose saveLoc = TRUE for exporting.

``` r
masked_rasters <- extr_rasters(Sebangau15, polygon_list, saveLoc = FALSE, datatype = "tif", out_dir = "myRasters")
```

Let’s plot a polygon layer to validate the results.

``` r
terra::plot(masked_rasters$forest)
```

<img src="man/figures/README-plotting masked raster-1.png" width="100%" />


                          # ---------------------------------------------- #
                          #  Monitoring Ecosystem Changes and Functioning  #
                          # ---------------------------------------------- #

 
# Prof. Duccio Rocchini
# Univesity: Alma Mater Studiorum University of Bologna.
# Academic year: 2022/2023
 
# Datasets: Landsat images and EEA data

# =========================== #
         ## SUMMARY ##
# =========================== #

# 1. SetWD and packages
# 2. Data import
# 3. False color
# 4. Spectral indices
# 5. Land use and Land Cover Classification
# 6. Change detection
# 7. Birds populations trend

# ============================ #
    ## SetWD and packages ##
# ============================ #

# Prima di tutto imposto la working directory
setwd("C:/lab/exam/")

# Creo una lista che contiene tutti i pacchetti che mi interesanno per le successiva analisi

list.of.packages <- c("tidyverse", "raster", "RStoolbox", "rasterdiv", "rasterVis",
                      "ggplot2", "viridis", "gridExtra", "maptools", "sf")

# il seguente comando verifica che i pacchetti siano installati (se necessario li installa) poi li carica

{
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) install.packages(new.packages)
  
  lapply(list.of.packages, require, character.only = TRUE)
}

# =========================== #
       ## Data import ##
# =========================== #

## I used this piece of code to create stack of bands I need for the further analysis

# list_img <- list.files(pattern = "191029_20050502")
# img_allbands <- lapply(list_img, raster)
# img_stack <- stack(img_allbands)
# writeRaster(img_stack, filename="delta_p191r29_20050502.grd", format="raster")

## Import the multiband images with brick and crop it to the intrested area
## From now on I will use this images for the anlysis

# Images of Sardinia

   ## Oristano

oristano_list <- list.files(pattern = "oristano_")
oristano_img <- lapply(oristano_list, brick)

# oristano_crop_plot <- plotRGB(oristano_img[[1]], 4, 3, 2, stretch="lin")
# drawExtent(show=TRUE, col="red")

# ext_oristano <- extent(447440, 463826.3, 4411123, 4428401)

crop_file <- function(oristano_img) {
 
  # Create the extent object
ext_oristano <- extent(447440, 463826.3, 4411123, 4428401)
  
  # Crop the raster object with the extent
  cropped_img <- crop(oristano_img, ext_cagliari)
  
  # Return the cropped raster object
  return(cropped_img)
}

# Apply the crop function to each file
oristano_cropped <- lapply(oristano_img, crop_file)

# Define the logical index to remove specific elements
remove_index <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)

# Remove the selected elements using the logical index
oristano_cropped <- oristano_cropped[!remove_index]

# plotRGB(oristano87_c, 4, 3, 2, stretch="lin")


    ## Cagliari

cagliari_list <- list.files(pattern = "cagliari_")
cagliari_img <- lapply(cagliari_list, brick)

# cagliari_crop_plot <- plotRGB(cagliari_img[[1]], 4, 3, 2, stretch="lin")
# drawExtent(show=TRUE, col="red")

ext_cagliari <- extent(485040.5, 530489.6, 4330778, 4351353)

crop_file <- function(cagliari_img) {
 
  # Create the extent object
  ext_cagliari <- extent(485040.5, 530489.6, 4330778, 4351353)
  
  # Crop the raster object with the extent
  cropped_img <- crop(cagliari_img, ext_cagliari)
  
  # Return the cropped raster object
  return(cropped_img)
}

# Apply the crop function to each file
cagliari_cropped <- lapply(cagliari_img, crop_file)

# Define the logical index to remove specific elements
remove_index <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)

# Remove the selected elements using the logical index
cagliari_cropped <- cagliari_cropped[!remove_index]


    ## Po delta

delta_list <- list.files(pattern = "delta_")
delta_img <- lapply(delta_list, brick)

# delta_crop_plot <- plotRGB(delta_img[[1]], 4, 3, 2, stretch="lin")
# drawExtent(show=TRUE, col="red")

ext_delta <- extent(266228.3, 304561.4, 4916977, 4979504)

crop_file <- function(delta_img) {
  
  # Create the extent bject
  ext_delta <- extent(266228.3, 304561.4, 4916977, 4979504)
  
  # Crop the raster object with the extent
  cropped_img <- crop(delta_img, ext_delta)
  
  # Return the cropped raster object
  return(cropped_img)
}

# Apply the crop function to each file
delta_cropped <- lapply(delta_img, crop_file)

# Define the logical index to remove specific elements
remove_index <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)

# Remove the selected elements using the logical index
delta_cropped <- delta_cropped[!remove_index]


# =============================== #
       ## Spectral indices ##
# =============================== #

    ## Po delta indeces

delta85_indices <- RStoolbox::spectralIndices(delta_cropped[[1]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
delta93_indices <- RStoolbox::spectralIndices(delta_cropped[[2]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
delta05_indices <- RStoolbox::spectralIndices(delta_cropped[[3]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
delta11_indices <- RStoolbox::spectralIndices(delta_cropped[[4]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
delta20_indices <- RStoolbox::spectralIndices(delta_cropped[[5]], blue = 2, green = 3, red = 4, nir = 5, swir2 = 6, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))

    ## Cagliari indeces

cagliari93_indices <- RStoolbox::spectralIndices(cagliari_cropped[[1]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
cagliari02_indices <- RStoolbox::spectralIndices(cagliari_cropped[[2]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
cagliari84_indices <- RStoolbox::spectralIndices(cagliari_cropped[[3]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
cagliari11_indices <- RStoolbox::spectralIndices(cagliari_cropped[[4]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
cagliari21_indices <- RStoolbox::spectralIndices(cagliari_cropped[[5]], blue = 2, green = 3, red = 4, nir = 5, swir2 = 6, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))


# ===================================================== #
       ## Land Use and Land Cover Classification ##
# ===================================================== #

    ## Po delta

delta85_lcc <- unsuperClass(delta_cropped[[1]], nSamples = 100, nClasses = 5)
delta93_lcc <- unsuperClass(delta_cropped[[2]], nSamples = 100, nClasses = 5)
delta05_lcc <- unsuperClass(delta_cropped[[3]], nSamples = 100, nClasses = 5)
delta11_lcc <- unsuperClass(delta_cropped[[4]], nSamples = 100, nClasses = 5)
delta20_lcc <- unsuperClass(delta_cropped[[5]], nSamples = 100, nClasses = 5)

delta_lcc <- list(delta85_lcc, delta93_lcc, delta05_lcc, delta11_lcc, delta20_lcc)

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# WHY THIS DOESN'T WORK??? #

# delta_lcc <- list()

# for (i in 1:length(delta_cropped)) {
#   
#   image <- delta_cropped[[i]]
#   
#   lcc <- unsuperClass(image, nSamples = 100, nClasses = 5)
#  
#   delta_lcc[[i]] <- lcc
# }

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

    ## Cagliari

set.seed(999)
cagliari93_lcc <- unsuperClass(cagliari_cropped[[1]], nSamples = 100, nClasses = 5)
cagliari02_lcc <- unsuperClass(cagliari_cropped[[2]], nSamples = 100, nClasses = 5)
cagliari84_lcc <- unsuperClass(cagliari_cropped[[3]], nSamples = 100, nClasses = 5)
cagliari11_lcc <- unsuperClass(cagliari_cropped[[4]], nSamples = 100, nClasses = 5)
cagliari21_lcc <- unsuperClass(cagliari_cropped[[5]], nSamples = 100, nClasses = 5)

cagliari_lcc <- list(cagliari84_lcc, cagliari93_lcc, cagliari02_lcc, cagliari11_lcc, cagliari21_lcc)




set.seed(999)
par(mfrow = c(1, 2))
p1 <- plot(cagliari_lcc[[1]]$map)
p2 <- plot(cagliari_lcc[[5]]$map)

freq(cagliari_lcc[[1]]$map)
tot1 <- 122743 + 202457 + 315397 + 174396 + 222551 + 1060

prop1 <- freq(cagliari_lcc[[1]]$map)/tot1

tot2 <- 31649 + 175660 + 140770 + 309907 + 297884 + 82734
prop5 <- freq(cagliari_lcc[[5]]$map)/tot2






# Load the data into a list
cagliari_lcc <- list(cagliari84_lcc, cagliari93_lcc, cagliari02_lcc, cagliari11_lcc, cagliari21_lcc)

# Initialize a counter to keep track of the plot number
plot_num <- 1

# Loop through the data in the list
for (i in 1:length(cagliari_lcc)) {
  # Extract the current data
  data <- cagliari_lcc[[i]]
  
  # Create a plot using ggplot2
  p <- ggplot(data, aes(x = wt, y = mpg)) +
    geom_raster() +
    ggtitle(paste0("Plot ", plot_num))
  
  # Increment the plot number
  plot_num <- plot_num + 1
  
  # Show the plot
  print(p)
}












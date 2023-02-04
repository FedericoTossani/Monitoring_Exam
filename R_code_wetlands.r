
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

# ================================ #
      ## SetWD and packages ##
# ================================ #

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

# I used this piece of code to create stack of bands I need for the further analysis

# list_img <- list.files(pattern = "193032_20200407")
# img_allbands <- lapply(list_img, raster)
# img_stack <- stack(img_allbands)
# writeRaster(img_stack, filename="oristano_p193r32_20200407.grd", format="raster")

## Import the multiband images with brick and crop it to the intrested area
## From now on I will use this images for the anlysis

   ## Oristano

oristano_list <- list.files(pattern = "oristano_")
oristano_img <- lapply(oristano_list, brick)

# oristano_crop_plot <- plotRGB(oristano_img[[1]], 4, 3, 2, stretch="lin")
# drawExtent(show=TRUE, col="red")

# ext_oristano <- extent(447440, 463826.3, 4411123, 4428401)

crop_file <- function(oristano_img) {
 
  # Create the extent object
 ext_oristano <- extent(442938.9, 473086.2, 4389311, 4436271)
  
  # Crop the raster object with the extent
  cropped_img <- crop(oristano_img, ext_oristano)
  
  # Return the cropped raster object
  return(cropped_img)
}

# Apply the crop function to each file
oristano_cropped <- lapply(oristano_img, crop_file)

# Define the logical index to remove specific elements
remove_index <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)

# Remove the selected elements using the logical index
oristano_cropped <- oristano_cropped[!remove_index]

# plotRGB(oristano_cropped[[1]], 4, 3, 2, stretch="lin")


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
  
  # Create the extent object
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


# =========================== #
       ## False color ##
# =========================== #


    # Oristano

# R = nir, G = red, B = green

 oristano_plot_nrg1 <- ggRGB(oristano_cropped[[1]], 4, 3, 2, stretch = "lin") +
   labs(title = "Oristano 1988",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 oristano_plot_nrg2 <- ggRGB(oristano_cropped[[2]], 4, 3, 2, stretch = "lin") +
   labs(title = "Oristano 1995",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 oristano_plot_nrg3 <- ggRGB(oristano_cropped[[3]], 4, 3, 2, stretch = "lin") +
   labs(title = "Oristano 2002",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 oristano_plot_nrg4 <- ggRGB(oristano_cropped[[4]], 4, 3, 2, stretch = "lin") +
   labs(title = "Oristano 2010",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 oristano_plot_nrg5 <- ggRGB(oristano_cropped[[5]], 6, 5, 4, stretch = "lin") +
   labs(title = "Oristano 2020",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat8 OLI/TIRS")

# grid.arrange (oristano_plot_nrg1, oristano_plot_nrg2, oristano_plot_nrg3, oristano_plot_nrg4, oristano_plot_nrg5, nrow=2)

grid.arrange (oristano_plot_nrg1, oristano_plot_nrg5, nrow=1)


# R = swir, G = swir, B = red

 oristano_plot_ssr1 <- ggRGB(oristano_cropped[[1]], 7, 5, 3, stretch = "lin") +
   labs(title = "Oristano 1988",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 oristano_plot_ssr2 <- ggRGB(oristano_cropped[[2]], 7, 5, 3, stretch = "lin") +
   labs(title = "Oristano 1995",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 oristano_plot_ssr3 <- ggRGB(oristano_cropped[[3]], 7, 5, 3, stretch = "lin") +
   labs(title = "Oristano 2002",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 oristano_plot_ssr4 <- ggRGB(oristano_cropped[[4]], 7, 5, 3, stretch = "lin") +
   labs(title = "Oristano 2010",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 oristano_plot_ssr5 <- ggRGB(oristano_cropped[[5]], 8, 7, 5, stretch = "lin") +
   labs(title = "Oristano 2020",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat8 OLI/TIRS")

# grid.arrange (oristano_plot_ssr1, oristano_plot_ssr2, oristano_plot_ssr3, oristano_plot_ssr4,oristano_ plot_ssr5, nrow=2)

grid.arrange (oristano_plot_ssr1, oristano_plot_ssr5, nrow=1)


# R = swir1, G = nir, B = red

oristano_plot_s1nr1 <- ggRGB(oristano_cropped[[1]], 5, 4, 3, stretch = "lin") +
   labs(title = "Oristano 1988",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

oristano_plot_s1nr2 <- ggRGB(oristano_cropped[[2]], 5, 4, 3, stretch = "lin") +
   labs(title = "Oristano 1995",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

oristano_plot_s1nr3 <- ggRGB(oristano_cropped[[3]], 5, 4, 3, stretch = "lin") +
   labs(title = "Oristano 2002",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

oristano_plot_s1nr4 <- ggRGB(oristano_cropped[[4]], 5, 4, 3, stretch = "lin") +
   labs(title = "Oristano 2010",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

oristano_plot_s1nr5 <- ggRGB(oristano_cropped[[5]], 7, 6, 5, stretch = "lin") +
   labs(title = "Oristano 2020",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat8 OLI/TIRS")

# grid.arrange (oristano_plot_s1nr1, oristano_plot_s1nr2, oristano_plot_s1nr3, oristano_plot_s1nr4, oristano_plot_s1nr5, nrow=2)

grid.arrange (oristano_plot_s1nr1, oristano_plot_s1nr5, nrow=1)


# R = swir2, G = nir, B = red

oristano_plot_s2nr1 <- ggRGB(oristano_cropped[[1]], 7, 4, 3, stretch = "lin") +
   labs(title = "Oristano 1988",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

oristano_plot_s2nr2 <- ggRGB(oristano_cropped[[2]], 7, 4, 3, stretch = "lin") +
   labs(title = "Oristano 1995",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

oristano_plot_s2nr3 <- ggRGB(oristano_cropped[[3]], 7, 4, 3, stretch = "lin") +
   labs(title = "Oristano 2002",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

oristano_plot_s2nr4 <- ggRGB(oristano_cropped[[4]], 7, 4, 3, stretch = "lin") +
   labs(title = "Oristano 2010",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

oristano_plot_s2nr5 <- ggRGB(oristano_cropped[[5]], 8, 6, 5, stretch = "lin") +
   labs(title = "Oristano 2020",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat8 OLI/TIRS")

# grid.arrange (oristano_plot_s2nr1, oristano_plot_s2nr2, oristano_plot_s2nr3, oristano_plot_s2nr4, oristano_plot_s2nr5, nrow=2)

grid.arrange (oristano_plot_s2nr1, oristano_plot_s2nr5, nrow=1)


    # Cagliari

# R = nir, G = red, B = green

 cagliari_plot_nrg1 <- ggRGB(cagliari_cropped[[1]], 4, 3, 2, stretch = "lin") +
   labs(title = "Cagliari 1987",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 cagliari_plot_nrg2 <- ggRGB(cagliari_cropped[[2]], 4, 3, 2, stretch = "lin") +
   labs(title = "Cagliari 1992",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 cagliari_plot_nrg3 <- ggRGB(cagliari_cropped[[3]], 4, 3, 2, stretch = "lin") +
   labs(title = "Cagliari 2005",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 cagliari_plot_nrg4 <- ggRGB(cagliari_cropped[[4]], 4, 3, 2, stretch = "lin") +
   labs(title = "Cagliari 2011",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 cagliari_plot_nrg5 <- ggRGB(cagliari_cropped[[5]], 6, 5, 4, stretch = "lin") +
   labs(title = "Cagliari 2022",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat8 OLI/TIRS")

# grid.arrange (cagliari_plot_nrg1, cagliari_plot_nrg2, cagliari_plot_nrg3, cagliari_plot_nrg4, cagliari_plot_nrg5, nrow=2)

grid.arrange (cagliari_plot_nrg1, cagliari_plot_nrg5, nrow=1)


# R = swir, G = swir, B = red

 cagliari_plot_ssr1 <- ggRGB(cagliari_cropped[[1]], 7, 5, 3, stretch = "lin") +
   labs(title = "Cagliari 1987",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 cagliari_plot_ssr2 <- ggRGB(cagliari_cropped[[2]], 7, 5, 3, stretch = "lin") +
   labs(title = "Cagliari 1992",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 cagliari_plot_ssr3 <- ggRGB(cagliari_cropped[[3]], 7, 5, 3, stretch = "lin") +
   labs(title = "Cagliari 2005",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 cagliari_plot_ssr4 <- ggRGB(cagliari_cropped[[4]], 7, 5, 3, stretch = "lin") +
   labs(title = "Cagliari 2011",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 cagliari_plot_ssr5 <- ggRGB(cagliari_cropped[[5]], 8, 7, 5, stretch = "lin") +
   labs(title = "Cagliari 2022",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat8 OLI/TIRS")

# grid.arrange (cagliari_plot_ssr1, cagliari_plot_ssr2, cagliari_plot_ssr3, cagliari_plot_ssr4, cagliari_plot_ssr5, nrow=2)

grid.arrange (cagliari_plot_ssr1, cagliari_plot_ssr5, nrow=1)


# R = swir1, G = nir, B = red

cagliari_plot_s1nr1 <- ggRGB(cagliari_cropped[[1]], 5, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 1987",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

cagliari_plot_s1nr2 <- ggRGB(cagliari_cropped[[2]], 5, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 1992",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

cagliari_plot_s1nr3 <- ggRGB(cagliari_cropped[[3]], 5, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 2005",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

cagliari_plot_s1nr4 <- ggRGB(cagliari_cropped[[4]], 5, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 2011",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

cagliari_plot_s1nr5 <- ggRGB(cagliari_cropped[[5]], 7, 6, 5, stretch = "lin") +
   labs(title = "Cagliari 2022",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat8 OLI/TIRS")

# grid.arrange (cagliari_plot_s1nr1, cagliari_plot_s1nr2, cagliari_plot_s1nr3, cagliari_plot_s1nr4, cagliari_plot_s1nr5, nrow=2)

grid.arrange (cagliari_plot_s1nr1, cagliari_plot_s1nr5, nrow=1)


# R = swir2, G = nir, B = red

cagliari_plot_s2nr1 <- ggRGB(cagliari_cropped[[1]], 7, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 1987",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

cagliari_plot_s2nr2 <- ggRGB(cagliari_cropped[[2]], 7, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 1992",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

cagliari_plot_s2nr3 <- ggRGB(cagliari_cropped[[3]], 7, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 2005",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

cagliari_plot_s2nr4 <- ggRGB(cagliari_cropped[[4]], 7, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 2011",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

cagliari_plot_s2nr5 <- ggRGB(cagliari_cropped[[5]], 8, 6, 5, stretch = "lin") +
   labs(title = "Cagliari 2022",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat8 OLI/TIRS")

# grid.arrange (cagliari_plot_s2nr1, cagliari_plot_s2nr2, cagliari_plot_s2nr3, cagliari_plot_s2nr4, cagliari_plot_s2nr5, nrow=2)

grid.arrange (cagliari_plot_s2nr1, cagliari_plot_s2nr5, nrow=1)


    # Po delta

# R = nir, G = red, B = green

 delta_plot_nrg1 <- ggRGB(delta_cropped[[1]], 4, 3, 2, stretch = "lin") +
   labs(title = "Po delta 1985",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 delta_plot_nrg2 <- ggRGB(delta_cropped[[2]], 4, 3, 2, stretch = "lin") +
   labs(title = "Po delta 1993",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 delta_plot_nrg3 <- ggRGB(delta_cropped[[3]], 4, 3, 2, stretch = "lin") +
   labs(title = "Po delta 2005",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 delta_plot_nrg4 <- ggRGB(delta_cropped[[4]], 4, 3, 2, stretch = "lin") +
   labs(title = "Po delta 2011",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 delta_plot_nrg5 <- ggRGB(delta_cropped[[5]], 6, 5, 4, stretch = "lin") +
   labs(title = "Po delta 2020",
       subtitle = "False color image RGB plot with R = NIR, G = Red and B = Green bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat8 OLI/TIRS")

# grid.arrange (delta_plot_nrg1, delta_plot_nrg2, delta_plot_nrg3, delta_plot_nrg4, delta_plot_nrg5, nrow=2)

grid.arrange (delta_plot_nrg1, delta_plot_nrg5, nrow=1)


# R = swir, G = swir, B = red

 delta_plot_ssr1 <- ggRGB(delta_cropped[[1]], 7, 5, 3, stretch = "lin") +
   labs(title = "Po delta 1985",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 delta_plot_ssr2 <- ggRGB(delta_cropped[[2]], 7, 5, 3, stretch = "lin") +
m   labs(title = "Po delta 1993",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 delta_plot_ssr3 <- ggRGB(delta_cropped[[3]], 7, 5, 3, stretch = "lin") +
   labs(title = "Po delta 2005",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 delta_plot_ssr4 <- ggRGB(delta_cropped[[4]], 7, 5, 3, stretch = "lin") +
   labs(title = "Po delta 2011",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

 delta_plot_ssr5 <- ggRGB(delta_cropped[[5]], 8, 7, 5, stretch = "lin") +
   labs(title = "Po delta 2020",
       subtitle = "False color image RGB plot with R = SWIR1, G = SWIR2 and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat8 OLI/TIRS")

# grid.arrange (delta_plot_ssr1, delta_plot_ssr2, delta_plot_ssr3, delta_plot_ssr4, delta_plot_ssr5, nrow=2)

grid.arrange (delta_plot_ssr1, delta_plot_ssr5, nrow=1)


# R = swir1, G = nir, B = red

delta_plot_s1nr1 <- ggRGB(delta_cropped[[1]], 5, 4, 3, stretch = "lin") +
   labs(title = "Po delta 1985",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

delta_plot_s1nr2 <- ggRGB(delta_cropped[[2]], 5, 4, 3, stretch = "lin") +
   labs(title = "Po delta 1993",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

delta_plot_s1nr3 <- ggRGB(delta_cropped[[3]], 5, 4, 3, stretch = "lin") +
   labs(title = "Po delta 2005",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

delta_plot_s1nr4 <- ggRGB(delta_cropped[[4]], 5, 4, 3, stretch = "lin") +
   labs(title = "Po delta 2011",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

delta_plot_s1nr5 <- ggRGB(delta_cropped[[5]], 7, 6, 5, stretch = "lin") +
   labs(title = "Po delta 2020",
       subtitle = "False color image RGB plot with R = SWIR1, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat8 OLI/TIRS")

# grid.arrange (delta_plot_s1nr1, delta_plot_s1nr2, delta_plot_s1nr3, delta_plot_s1nr4, delta_plot_s1nr5, nrow=2)

grid.arrange (delta_plot_s1nr1, delta_plot_s1nr5, nrow=1)


# R = swir2, G = nir, B = red

delta_plot_s2nr1 <- ggRGB(delta_cropped[[1]], 7, 4, 3, stretch = "lin") +
   labs(title = "Po delta 1985",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

delta_plot_s2nr2 <- ggRGB(delta_cropped[[2]], 7, 4, 3, stretch = "lin") +
   labs(title = "Po delta 1993",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

delta_plot_s2nr3 <- ggRGB(delta_cropped[[3]], 7, 4, 3, stretch = "lin") +
   labs(title = "Po delta 2005",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

delta_plot_s2nr4 <- ggRGB(delta_cropped[[4]], 7, 4, 3, stretch = "lin") +
   labs(title = "Po delta 2011",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat4 TM")

delta_plot_s2nr5 <- ggRGB(delta_cropped[[5]], 8, 6, 5, stretch = "lin") +
   labs(title = "Po delta 2020",
       subtitle = "False color image RGB plot with R = SWIR2, G = NIR and B = Red bands",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat8 OLI/TIRS")

# grid.arrange (delta_plot_s2nr1, delta_plot_s2nr2, delta_plot_s2nr3, delta_plot_s2nr4, delta_plot_s2nr5, nrow=2)

grid.arrange (delta_plot_s2nr1, delta_plot_s2nr5, nrow=1)


# =============================== #
       ## Spectral indices ##
# =============================== #


    ## Oristano indices

oristano88_indices <- RStoolbox::spectralIndices(oristano_cropped[[1]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
oristano95_indices <- RStoolbox::spectralIndices(oristano_cropped[[2]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
oristano02_indices <- RStoolbox::spectralIndices(oristano_cropped[[3]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
oristano10_indices <- RStoolbox::spectralIndices(oristano_cropped[[4]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
oristano20_indices <- RStoolbox::spectralIndices(oristano_cropped[[5]], blue = 3, green = 4, red = 5, nir = 6, swir2 = 7, swir3= 8, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))

oristano_ndvi_diff <- oristano20_indices$NDVI - oristano88_indices$NDVI

oristano_ndvi_plot <-
 ggplot()+
  geom_raster(oristano_ndvi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Oristano",
       subtitle = "Difference in NDVI index between 1988 and 2020",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat 4 and 8 images")
ggsave("oristano_ndvi_diff_plot.jpeg", plot = oristano_ndvi_plot)


oristano_ndwi_diff <- oristano20_indices$NDWI - oristano88_indices$NDWI

oristano_ndwi_plot <-
 ggplot()+
  geom_raster(oristano_ndwi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Oristano",
       subtitle = "Difference in NDWI index between 1988 and 2020",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat 4 and 8 images")
ggsave("oristano_ndwi_diff_plot.jpeg", plot = oristano_ndwi_plot)


oristano_ndwi2_diff <- oristano20_indices$NDWI2 - oristano88_indices$NDWI2

oristano_ndwi2_plot <-
 ggplot()+
  geom_raster(oristano_ndwi2_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Oristano",
       subtitle = "Difference in NDWI2 index between 1988 and 2020",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat 4 and 8 images")
ggsave("oristano_ndwi2_diff_plot.jpeg", plot = oristano_ndwi2_plot)


oristano_slavi_diff <- oristano20_indices$SLAVI - oristano88_indices$SLAVI

oristano_slavi_plot <-
 ggplot()+
  geom_raster(oristano_slavi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Oristano",
       subtitle = "Difference in SLAVI index between 1988 and 2020",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat 4 and 8 images")
ggsave("oristano_slavi_diff_plot.jpeg", plot = oristano_slavi_plot)


    ## Cagliari indices

cagliari87_indices <- RStoolbox::spectralIndices(cagliari_cropped[[1]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
cagliari92_indices <- RStoolbox::spectralIndices(cagliari_cropped[[2]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
cagliari05_indices <- RStoolbox::spectralIndices(cagliari_cropped[[3]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
cagliari11_indices <- RStoolbox::spectralIndices(cagliari_cropped[[4]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
cagliari22_indices <- RStoolbox::spectralIndices(cagliari_cropped[[5]], blue = 3, green = 4, red = 5, nir = 6, swir2 = 7, swir3= 8, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))

cagliari_ndvi_diff <- cagliari22_indices$NDVI - cagliari87_indices$NDVI

cagliari_ndvi_plot <-
 ggplot()+
  geom_raster(cagliari_ndvi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Cagliari",
       subtitle = "Difference in NDVI index between 1987 and 2022",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat 4 and 8 images")
ggsave("cagliari_ndvi_diff_plot.jpeg", plot = cagliari_ndvi_plot)


cagliari_ndwi_diff <- cagliari22_indices$NDWI - cagliari87_indices$NDWI

cagliari_ndwi_plot <-
 ggplot()+
  geom_raster(cagliari_ndwi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Cagliari",
       subtitle = "Difference in NDWI index between 1987 and 2022",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat 4 and 8 images")
ggsave("cagliari_ndwi_diff_plot.jpeg", plot = cagliari_ndwi_plot)


cagliari_ndwi2_diff <- cagliari22_indices$NDWI2 - cagliari87_indices$NDWI2

cagliari_ndwi2_plot <-
 ggplot()+
  geom_raster(cagliari_ndwi2_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Cagliari",
       subtitle = "Difference in NDWI2 index between 1987 and 2022",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat 4 and 8 images")
ggsave("cagliari_ndwi2_diff_plot.jpeg", plot = cagliari_ndwi2_plot)


cagliari_slavi_diff <- cagliari22_indices$SLAVI - cagliari87_indices$SLAVI

cagliari_slavi_plot <-
 ggplot()+
  geom_raster(cagliari_slavi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Cagliari",
       subtitle = "Difference in SLAVI index between 1987 and 2022",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat 4 and 8 images")
ggsave("cagliari_slavi_diff_plot.jpeg", plot = cagliari_slavi_plot)

    ## Po delta indices

delta85_indices <- RStoolbox::spectralIndices(delta_cropped[[1]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
delta93_indices <- RStoolbox::spectralIndices(delta_cropped[[2]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
delta05_indices <- RStoolbox::spectralIndices(delta_cropped[[3]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
delta11_indices <- RStoolbox::spectralIndices(delta_cropped[[4]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
delta20_indices <- RStoolbox::spectralIndices(delta_cropped[[5]], blue = 2, green = 3, red = 4, nir = 5, swir2 = 6, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))

delta_ndvi_diff <- delta20_indices$NDVI - delta85_indices$NDVI

delta_ndvi_plot <-
 ggplot()+
  geom_raster(delta_ndvi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Po delta",
       subtitle = "Difference in NDVI index between 1985 and 2020",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat 4 and 8 images")
ggsave("delta_ndvi_diff_plot.jpeg", plot = delta_ndvi_plot)

delta_ndwi_diff <- delta20_indices$NDWI - delta85_indices$NDWI

delta_ndwi_plot <-
 ggplot()+
  geom_raster(delta_ndwi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Po delta",
       subtitle = "Difference in NDWI index between 1985 and 2020",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat 4 and 8 images")
ggsave("delta_ndwi_diff_plot.jpeg", plot = delta_ndwi_plot)

delta_ndwi2_diff <- delta20_indices$NDWI2 - delta85_indices$NDWI2

delta_ndwi2_plot <-
 ggplot()+
  geom_raster(delta_ndwi2_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Po delta",
       subtitle = "Difference in NDWI2 index between 1985 and 2020",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat 4 and 8 images")
ggsave("delta_ndwi2_diff_plot.jpeg", plot = delta_ndwi2_plot)

delta_slavi_diff <- delta20_indices$SLAVI - delta85_indices$SLAVI

delta_slavi_plot <-
 ggplot()+
  geom_raster(delta_slavi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Po delta",
       subtitle = "Difference in SLAVI index between 1985 and 2020",
       x = "Longitude",
       y = "Latitude",
       caption = "Data source: Landsat 4 and 8 images")
ggsave("delta_slavi_diff_plot.jpeg", plot = delta_slavi_plot)
    

# ===================================================== #
       ## Land Use and Land Cover Classification ##
# ===================================================== #


    ## Oristano

set.seed(999)
oristano88_lcc <- unsuperClass(oristano_cropped[[1]], nSamples = 1000, nClasses = 5)
oristano95_lcc <- unsuperClass(oristano_cropped[[2]], nSamples = 1000, nClasses = 5)
oristano02_lcc <- unsuperClass(oristano_cropped[[3]], nSamples = 1000, nClasses = 5)
oristano10_lcc <- unsuperClass(oristano_cropped[[4]], nSamples = 1000, nClasses = 5)
oristano20_lcc <- unsuperClass(oristano_cropped[[5]], nSamples = 1000, nClasses = 5)

oristano_lcc <- list(oristano88_lcc, oristano95_lcc, oristano02_lcc, oristano10_lcc, oristano20_lcc)

par(mfrow = c(3, 2))
plot(oristano_lcc[[1]]$map)
plot(oristano_lcc[[2]]$map)
plot(oristano_lcc[[3]]$map)
plot(oristano_lcc[[4]]$map)
plot(oristano_lcc[[5]]$map)


    ## Cagliari

cagliari87_lcc <- unsuperClass(cagliari_cropped[[1]], nSamples = 1000, nClasses = 5)
cagliari92_lcc <- unsuperClass(cagliari_cropped[[2]], nSamples = 1000, nClasses = 5)
cagliari05_lcc <- unsuperClass(cagliari_cropped[[3]], nSamples = 1000, nClasses = 5)
cagliari11_lcc <- unsuperClass(cagliari_cropped[[4]], nSamples = 1000, nClasses = 5)
cagliari22_lcc <- unsuperClass(cagliari_cropped[[5]], nSamples = 1000, nClasses = 5)

cagliari_lcc <- list(cagliari84_lcc, cagliari93_lcc, cagliari02_lcc, cagliari11_lcc, cagliari21_lcc)

par(mfrow = c(3, 2))
plot(cagliari_lcc[[1]]$map)
plot(cagliari_lcc[[2]]$map)
plot(cagliari_lcc[[3]]$map)
plot(cagliari_lcc[[4]]$map)
plot(cagliari_lcc[[5]]$map)

    ## Po delta

set.seed(999)
delta85_lcc <- unsuperClass(delta_cropped[[1]], nSamples = 1000, nClasses = 5)
delta93_lcc <- unsuperClass(delta_cropped[[2]], nSamples = 1000, nClasses = 5)
delta05_lcc <- unsuperClass(delta_cropped[[3]], nSamples = 1000, nClasses = 5)
delta11_lcc <- unsuperClass(delta_cropped[[4]], nSamples = 1000, nClasses = 5)
delta20_lcc <- unsuperClass(delta_cropped[[5]], nSamples = 1000, nClasses = 5)

delta_lcc <- list(delta85_lcc, delta93_lcc, delta05_lcc, delta11_lcc, delta20_lcc)

par(mfrow = c(3, 2))
plot(delta_lcc[[1]]$map)
plot(delta_lcc[[2]]$map)
plot(delta_lcc[[3]]$map)
plot(delta_lcc[[4]]$map)
plot(delta_lcc[[5]]$map)

#set.seed(999)
#par(mfrow = c(1, 2))
#p1 <- plot(cagliari_lcc[[1]]$map)
#p2 <- plot(cagliari_lcc[[5]]$map)

#freq(cagliari_lcc[[1]]$map)
#tot1 <- 122743 + 202457 + 315397 + 174396 + 222551 + 1060

#prop1 <- freq(cagliari_lcc[[1]]$map)/tot1

#tot2 <- 31649 + 175660 + 140770 + 309907 + 297884 + 82734
#prop5 <- freq(cagliari_lcc[[5]]$map)/tot2


# Load the data into a list
#cagliari_lcc <- list(cagliari84_lcc, cagliari93_lcc, cagliari02_lcc, cagliari11_lcc, cagliari21_lcc)

# Initialize a counter to keep track of the plot number
#plot_num <- 1

# Loop through the data in the list
#for (i in 1:length(cagliari_lcc)) {
  # Extract the current data
#  data <- cagliari_lcc[[i]]
  
  # Create a plot using ggplot2
#  p <- ggplot(data, aes(x = wt, y = mpg)) +
#    geom_raster() +
#    ggtitle(paste0("Plot ", plot_num))
  
  # Increment the plot number
#  plot_num <- plot_num + 1
  
  # Show the plot
#  print(p)
#}






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

# Errore in (function (classes, fdef, mtable)  : 
#   non è possibile trovare un metodo ereditato per la funzione ‘nlayers’ per la firma ‘"NULL"’

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #










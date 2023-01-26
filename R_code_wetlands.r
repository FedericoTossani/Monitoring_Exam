
                          # ---------------------------------------------- #
                          ## Monitoring Ecosystem Changes and Functioning ##
                          # ---------------------------------------------- #

 
# Prof. Duccio Rocchini
 
# Univesiyà: Alma Mater Studiorum Università di Bologna.

# Anno: 2022/2023
 
# Datasets: Landsat images and EEA data

# ==================================================================================================== #
# ==================================================================================================== #

## SUMMARY ##

# 1. SetWD and packages
# 2. Data import
# 3. False color
# 4. Vegetation indices
# 5. Water indices
# 6. Land use and Land Cover Classification
# 7. Change detection
# 8. Birds populations trend

# ==================================================================================================== #
# ==================================================================================================== #

## SetWD and packages

# Prima di tutto imposto la working directory
setwd("C:/lab/exam/")

# Creo una lista che contiene tutti i pacchetti che mi interesanno per le successiva analisi

list.of.packages <- c("tidyverse", "raster", "RStoolbox", "rasterdiv", "rasterVis",
                      "ggplot2", "viridis", "gridExtra")

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

# list_ori_22 <- list.files(pattern = "2022")
# ori22_all <- lapply(list_ori_22, raster)
# ori22_stack <- stack(ori22_all)
# writeRaster(ori22_stack, filename="ori_p193r32_20220429.grd", format="raster")

# list_ori_02 <- list.files(pattern = "2002")
# ori02_all <- lapply(list_ori_02, raster)
# ori02_stack <- stack(ori02_all)
# writeRaster(ori02_stack, filename="ori_p193r32_20020430.grd", format="raster")

list_ori_84 <- list.files(pattern = "1984")
ori84_all <- lapply(list_ori_84, raster)
ori84_stack <- stack(ori84_all)
writeRaster(ori84_stack, filename="ori_p193r32_19840420.grd", format="raster")

## Import the multiband images with brick and crop it to the intrested area
## From now on I will use this images for the anlysis

ori22t <- brick("ori_p193r32_20220429.grd")

ori02t <- brick("ori_p193r32_20020430.grd")

ori84t <- brick("ori_p193r32_19840420.grd")

p_o22 <- plotRGB(ori22t, 4, 3, 2, stretch="lin")
# drawExtent(show=TRUE, col="red")


# xmin       : 447440 
# xmax       : 463826.3 
# ymin       : 4411123 
# ymax       : 4428401 

extnew <- extent(447440, 463826.3, 4411123, 4428401)

# Let's crop the images by the extent I need

ori22 <- crop(ori22t, extnew)
ori02 <- crop(ori02t, extnew)
ori84 <- crop(ori84t, extnew)

# Let's plot the images in natural colors

p22 <- plotRGB(ori22, 4, 3, 2, stretch = "lin")
p02 <- plotRGB(ori02, 3, 2, 1, stretch = "lin")
p84 <- plotRGB(ori84, 3, 2, 1, stretch = "lin")

grid.arrange(p84, p02, p22, ncol = 1)

# ============================ #
    ## False colors ##
# ============================ #

# Vegetarion Analysis


# Urban area detection


# Shortwave Infrared


# ============================ #
    ## Vegetation indices ##
# ============================ #

# NDVI
# NDVI is obtained by divideing the difference between NIR and red bands by their sum

NDVI_ori22 <- (ori22[[5]]-ori22[[4]])/(ori22[[5]]+ori22[[4]])
NDVI_ori02 <- (ori02[[5]]-ori02[[4]])/(ori02[[5]]+ori02[[4]])
NDVI_ori83 <- (ori83[[4]]-ori83[[3]])/(ori83[[4]]+ori83[[3]])

p_NDVI_22 <- ggplot()+
geom_raster(NDVI_ori22, mapping=aes(x = x, y = y, fill = layer))+
scale_fill_viridis()

p_NDVI_02 <- ggplot()+
geom_raster(NDVI_ori02, mapping=aes(x = x, y = y, fill = layer))+
scale_fill_viridis()

p_NDVI_83 <- ggplot()+
geom_raster(NDVI_ori83, mapping=aes(x = x, y = y, fill = layer))+
scale_fill_viridis()

grid.arrange(p_NDVI_22, p_NDVI_02, p_NDVI_83, ncol = 1)
# EVI

#














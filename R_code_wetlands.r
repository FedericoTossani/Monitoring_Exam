
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
# 3. Vegetation indices
# 4. Water indices
# 5. Land use and Land Cover Classification
# 6. Change detection
# 7. Birds populations trend

# ==================================================================================================== #
# ==================================================================================================== #

## SetWD and packages

# Prima di tutto imposto la working directory
setwd("C:/lab/exam/images/")

# Creo una lista che contiene tutti i pacchetti che mi interesanno per le successiva analisi

list.of.packages <- c("tidyverse", "raster", "RStoolbox", "rasterdiv", "rasterVis",
                      "ggplot2", "viridis", "gridExtra")

# il seguente comando verifica che i pacchetti siano installati (se necessario li installa) poi li carica

{
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) install.packages(new.packages)
  
  lapply(list.of.packages, require, character.only = TRUE)
}

# ==================================================================================================== #
# ==================================================================================================== #

## Data import ##

## I used this piece of code to create stack of bands I need for the further analysis

# list_ori_22 <- list.files(pattern = "2022")
# ori22_all <- lapply(list_ori_22, raster)
# ori22_stack <- stack(ori22_all)
# writeRaster(ori22_stack, filename="ori_p193r32_20220429.grd", format="raster")

# list_ori_02 <- list.files(pattern = "2002")
# ori02_all <- lapply(list_ori_02, raster)
# ori02_stack <- stack(ori02_all)
# writeRaster(ori02_stack, filename="ori_p193r32_20020430.grd", format="raster")

# list_ori_83 <- list.files(pattern = "1983")
# ori83_all <- lapply(list_ori_83, raster)
# ori83_stack <- stack(ori83_all)
# writeRaster(ori83_stack, filename="ori_p193r32_19830410.grd", format="raster")

## Import the multiband images with brick and crop it to the intrested area

ori22t <- brick("ori_p193r32_20220429.grd")

ori02t <- brick("ori_p193r32_20020430.grd")

ori83t <- brick("ori_p193r32_19830410.grd")

p_o22 <- plotRGB(ori22t, 4, 3, 2, stretch="lin")
# drawExtent(show=TRUE, col="red")

# class      : Extent 
# xmin       : 445706.3 
# xmax       : 471400.7 
# ymin       : 4405203 
# ymax       : 4431219 

extnew <- extent(445706.3, 471400.7, 4405203, 4431219)

# Let's crop the images by the extent I need

ori22 <- crop(ori22t, extnew)
ori02 <- crop(ori02t, extnew)
ori83 <- crop(ori83t, extnew)

# ==================================================================================================== #
# ==================================================================================================== #

## Vegetation indices

# NDVI
# NDVI is obtained by divideing the difference between NIR and red bands by their sum

NDVI_ori22 <- (ori22[[5]]-ori22[[4]])/(ori22[[5]]+ori22[[4]])
NDVI_ori02 <- (ori02[[5]]-ori02[[4]])/(ori02[[5]]+ori02[[4]])
NDVI_ori83 <- (ori83[[4]]-ori83[[3]])/(ori83[[4]]+ori83[[3]])

p_NDVI_22 <- ggplot()+
geom_raster(NDVI_ori22, mapping=aes(x = x, y = y, fill = layer))+
scale_fill_viridis()

# EVI

#















                          # ---------------------------------------------- #
                          #  Monitoring Ecosystem Changes and Functioning  #
                          # ---------------------------------------------- #

 
# Prof. Duccio Rocchini
 
# Univesiyà: Alma Mater Studiorum Università di Bologna.

# Anno: 2022/2023
 
# Datasets: Landsat images and EEA data

# =========================== #
# =========================== #

## SUMMARY ##

# 1. SetWD and packages
# 2. Data import
# 3. False color
# 4. Vegetation indices
# 5. Water indices
# 6. Land use and Land Cover Classification
# 7. Change detection
# 8. Birds populations trend

# =========================== #
# =========================== #

## SetWD and packages ##

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

list_ori_87 <- list.files(pattern = "1987")
ori87_all <- lapply(list_ori_87, raster)
ori87_stack <- stack(ori87_all)
writeRaster(ori87_stack, filename="ori_p193r32_19870429.grd", format="raster")

## Import the multiband images with brick and crop it to the intrested area
## From now on I will use this images for the anlysis

ori22t <- brick("ori_p193r32_20220429.grd")

ori02t <- brick("ori_p193r32_20020430.grd")

ori87t <- brick("ori_p193r32_19870429.grd")

p_o22 <- plotRGB(ori22t, 4, 3, 2, stretch="lin")
# drawExtent(show=TRUE, col="red")

extnew <- extent(447440, 463826.3, 4411123, 4428401)

# Let's crop the images by the extent I need

ori22 <- crop(ori22t, extnew)
ori02 <- crop(ori02t, extnew)
ori87 <- crop(ori87t, extnew)

# Let's plot the images in natural colors

p22 <- plotRGB(ori22, 4, 3, 2, stretch = "lin")
p02 <- plotRGB(ori02, 3, 2, 1, stretch = "lin")
p87 <- plotRGB(ori87, 3, 2, 1, stretch = "lin")

grid.arrange(p87, p02, p22, nrow = 2)


### ---> ??? removing water pixel ??? <--- ###


# removing water pixel 
ndwi87 <- (ori87[[3]] - ori87[[5]]) / (ori87[[3]] + ori87[[5]])
ndwi22 <- (ori22[[4]] - ori22[[7]]) / (ori22[[4]] + ori22[[7]])
mndwi87 <- (ori87[[4]] - ori87[[5]]) / (ori87[[4]] + ori87[[5]])

# Set a threshold value for water pixels
threshold <- 0.05

# Create a binary mask of water pixels
water_mask <- (ndwi87 < threshold)

# Set water pixels to NA
ori87[water_mask] <- NA

# Plot the image with water pixels removed
plot(ori87)

# ============================ #
    ## False colors ##
# ============================ #

# Vegetarion Analysis

# ???
pfvs22 <- plotRGB(ori22, 6, 5, 4, stretch = "lin")
pfvs02 <- plotRGB(ori02, 5, 4, 3, stretch = "lin")
pfvs87 <- plotRGB(ori87, 5, 4, 3, stretch = "lin")

grid.arrange(pfvs22, pfvs02, pfvs87, nrow = 2)

# ???
pfvn22 <- plotRGB(ori22, 5, 4, 3, stretch = "lin")
pfvn02 <- plotRGB(ori02, 4, 3, 2, stretch = "lin")
pfvn87 <- plotRGB(ori87, 4, 3, 2, stretch = "lin")

grid.arrange(pfn22, pfn02, pfn87, nrow = 2)

# Urban area detection

# ???
pfu22 <- plotRGB(ori22, 7, 6, 4, stretch = "lin")
pfu02 <- plotRGB(ori02, 7, 5, 3, stretch = "lin")
pfu87 <- plotRGB(ori87, 7, 5, 3, stretch = "lin")

grid.arrange(pfu22, pfu02, pfu87, nrow = 2)

# Shortwave Infrared

pfs22 <- plotRGB(ori22, 7, 5, 4, stretch = "lin")
pfs02 <- plotRGB(ori02, 7, 4, 3, stretch = "lin")
pfs87 <- plotRGB(ori87, 7, 4, 3, stretch = "lin")

grid.arrange(pfs22, pfs02, pfs87, nrow = 2)

# ============================ #
    ## Vegetation indices ##
# ============================ #

# NDVI
# NDVI is obtained by divideing the difference between NIR and red bands by their sum

ndvi22 <- (ori22[[5]] - ori22[[4]]) / (ori22[[5]] + ori22[[4]])
ndvi02 <- (ori02[[5]] - ori02[[4]]) / (ori02[[5]] + ori02[[4]])
ndvi87 <- (ori87[[5]] - ori87[[4]]) / (ori87[[5]] + ori87[[4]])

ndvi_diff <- ndvi87 - ndvi22

pn22 <- ggplot()+
geom_raster(ndvi22, mapping=aes(x = x, y = y, fill = layer))+
scale_fill_viridis()

pn02 <- ggplot()+
geom_raster(ndvi02, mapping=aes(x = x, y = y, fill = layer))+
scale_fill_viridis()

pn87 <- ggplot()+
geom_raster(ndvi87, mapping=aes(x = x, y = y, fill = layer))+
scale_fill_viridis()

pnd <- ggplot()+
geom_raster(ndvi_diff, mapping=aes(x = x, y = y, fill = layer))+
scale_fill_viridis()

grid.arrange(pn22, pn02, pn87, pnd,  nrow = 2)

# EVI

#














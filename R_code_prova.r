
                          # ---------------------------------------------- #
                          #  Monitoring Ecosystem Changes and Functioning  #
                          # ---------------------------------------------- #

# Code pieces, check if there's somethings good

# - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# old code to import and crop

oristano87_f <- brick("ori_p193r32_19870429.grd")
oristano02_f <- brick("ori_p193r32_20020430.grd")
oristano22_f <- brick("ori_p193r32_20220429.grd") # Landsat 8

oristano_crop_plot <- plotRGB(oristano87_f, 4, 3, 2, stretch="lin")
drawExtent(show=TRUE, col="red")

ext_oristano <- extent(447440, 463826.3, 4411123, 4428401)

oristano87_c <- crop(oristano87_f, ext_oristano)
oristano02_c <- crop(oristano02_f, ext_oristano)
oristano22_c <- crop(oristano22_f, ext_oristano) # Landsat 8

cagliari84_f <- brick("cagliari_p193r33_19840506.grd")
cagliari93_f <- brick("cagliari_p192r33_19930508.grd")
cagliari02_f <- brick("cagliari_p192r33_20020517.grd")
cagliari11_f <- brick("cagliari_p193r33_20110517.grd")
cagliari21_f <- brick("cagliari_p193r33_20210512.grd") # Landsat 8

# Let's crop the images by the extent I need

cagliari84_c <- crop(cagliari84_f, ext_cagliari)
cagliari93_c <- crop(cagliari93_f, ext_cagliari)
cagliari02_c <- crop(cagliari02_f, ext_cagliari)
cagliari11_c <- crop(cagliari11_f, ext_cagliari)
cagliari21_c <- crop(cagliari21_f, ext_cagliari) # Landsat 8

# plotRGB(cagliari84_c, 4, 3, 2, stretch="lin")

delta85_f <- brick("delta_p191r29_19850511.grd")
delta93_f <- brick("delta_p191r29_19930517.grd")
delta02_f <- brick("delta_p192r29_20020517.grd")
delta11_f <- brick("delta_p191r29_20110519.grd")
delta20_f <- brick("delta_p191r29_20200527.grd") # Landsat 8

# Let's crop the images by the extent I need

delta85_c <- crop(delta85_f, ext_delta)
delta93_c <- crop(delta93_f, ext_delta)
delta02_c <- crop(delta02_f, ext_delta)
delta11_c <- crop(delta11_f, ext_delta)
delta20_c <- crop(delta20_f, ext_delta) # Landsat 8

# plotRGB(delta85_c, 4, 3, 2, stretch="lin")

# - # - # - # - # - # - # - # - # - # - # - # - # - # - #


# Let's plot the images in natural colors

p22 <- plotRGB(ori22, 4, 3, 2, stretch = "lin")
p02 <- plotRGB(ori02, 3, 2, 1, stretch = "lin")
p87 <- plotRGB(ori87, 3, 2, 1, stretch = "lin")

grid.arrange(p87, p02, p22, nrow = 2)


### ---> ??? detecting water pixel ??? <--- ###


# detecting water pixel 
ndwi87 <- (ori87[[3]] - ori87[[5]]) / (ori87[[3]] + ori87[[5]]) # that's good
ndwi02 <- (ori02[[3]] - ori02[[5]]) / (ori02[[3]] + ori02[[5]]) # that's good
ndwi22 <- (ori22[[4]] - ori22[[7]]) / (ori22[[4]] + ori22[[7]]) # that's good

ndwi87_po <- (po87[[3]] - po87[[5]]) / (po87[[3]] + po87[[5]]) # that's good
ndwi22_po <- (po22[[4]] - po22[[7]]) / (po22[[4]] + po22[[7]]) # that's good

# Set a threshold value for water pixels
threshold <- 0.05

# Create a binary mask of water pixels
water_mask87 <- (ndwi87 < threshold)
water_mask02 <- (ndwi02 < threshold)
water_mask22 <- (ndwi22 < threshold)

water_mask87_po <- (ndwi87_po < threshold)
water_mask22_po <- (ndwi22_po < threshold)

# save a new object to store the results
ori87w <- ori87
ori02w <- ori02
ori22w <- ori22

po87w <- po87
po22w <- po22

# Set water pixels to NA
(ori87w[water_mask87] <- NA)
(ori02w[water_mask02] <- NA)
(ori22w[water_mask22] <- NA)



# Plot the image with water pixels removed
plot(ori87)

pfvs22w <- plotRGB(ori22w, 6, 5, 4, stretch = "lin")
pfvs02w <- plotRGB(ori02w, 5, 4, 3, stretch = "lin")
pfvs87w <- plotRGB(ori87w, 5, 4, 3, stretch = "lin")

pfvs87w_po <- plotRGB(po87w, 5, 4, 3, stretch = "lin")
pfvs22w_po <- plotRGB(po22w, 6, 5, 4, stretch = "lin")

ndvi22w <- (ori22w[[5]] - ori22w[[4]]) / (ori22w[[5]] + ori22w[[4]])
ndvi02w <- (ori02w[[5]] - ori02w[[4]]) / (ori02w[[5]] + ori02w[[4]])
ndvi87w <- (ori87w[[5]] - ori87w[[4]]) / (ori87w[[5]] + ori87w[[4]])

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

# =================================== #
    ## Land Cover Classification ##
# =================================== #


po22_lcc <- unsuperClass(po22, nSamples = 100, nClasses = 50)



delta_nir_bands <- list()

for (i in 1:length(delta_cropped)) {
  # Select the i-th image in the list
  selected_image <- delta_cropped[[i]]
  # Select the fourth band of the selected image
  selected_band <- selected_image[[4]]
  # Add the selected band to the list of selected bands
  delta_nir_bands[[i]] <- selected_band
}

assign("NIR Bands", delta_nir_bands)


delta_red_bands <- list()

for (i in 1:length(delta_cropped)) {
  # Select the i-th image in the list
  selected_image <- delta_cropped[[i]]
  # Select the fourth band of the selected image
  selected_band <- selected_image[[3]]
  # Add the selected band to the list of selected bands
  delta_red_bands[[i]] <- selected_band
}

assign("Red Bands", delta_red_bands)


# Define the function to be repeated

calculate_ndvi <- function(delta_cropped) {
  # Extract the near-infrared band (B4) and red band (B3)

delta_nir_bands <- list()

for (i in 1:length(delta_cropped)) {
  # Select the i-th image in the list
  selected_image <- delta_cropped[[i]]
  # Select the fourth band of the selected image
  selected_band <- selected_image[[4]]
  # Add the selected band to the list of selected bands
  delta_nir_bands[[i]] <- selected_band
}

assign("NIR Bands", delta_nir_bands)


delta_red_bands <- list()

for (i in 1:length(delta_cropped)) {
  # Select the i-th image in the list
  selected_image <- delta_cropped[[i]]
  # Select the fourth band of the selected image
  selected_band <- selected_image[[3]]
  # Add the selected band to the list of selected bands
  delta_red_bands[[i]] <- selected_band
}

assign("Red Bands", delta_red_bands)
  
  # Calculate NDVI
  ndvi <- (delta_nir_bands - delta_red_bands) / (delta_nir_bands + delta_red_bands)
  
  # Return the NDVI raster object
  return(ndvi)
}



# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# FIX THIS #

# remove sea pixel

# Read in the shapefile of the area of interest
aoi <- readShapePoly("C:/lab/exam/ramsar_delta.shp")
ext_delta <- extent(266228.3, 304561.4, 4916977, 4979504)

aoi_cropped <- crop(aoi, ext_delta)

# Crop the image to the area of interest
delta_85_test <- mask(delta_cropped[[1]], aoi)


# Repeat spectralIndeces function over each images of delta_cropped list
  for (i in 1:length(delta_cropped)) {
  delta_indices_list <-  RStoolbox::spectralIndices(delta_cropped[[i]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
  
  }


# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #











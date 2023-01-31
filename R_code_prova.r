
                          # ---------------------------------------------- #
                          #  Monitoring Ecosystem Changes and Functioning  #
                          # ---------------------------------------------- #

# Code pieces, check if there's somethings good

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









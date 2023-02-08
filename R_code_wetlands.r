
                          # ---------------------------------------------- #
                          #  Monitoring Ecosystem Changes and Functioning  #
                          # ---------------------------------------------- #

 
# Prof. Duccio Rocchini
# Univesity: Alma Mater Studiorum University of Bologna.
# Academic year: 2022/2023
 
# Datasets: Landsat images and European Environmental Agency data

# =========================== #
         ## SUMMARY ##
# =========================== #

# 1. SetWD and packages
# 2. Data import
# 3. Birds populations trend
# 4. False color
# 5. Spectral indices
# 6. Land use and Land Cover Classification

# ================================ #
      ## SetWD and packages ##
# ================================ #

# Prima di tutto imposto la working directory
setwd("C:/lab/exam/")

# First I create a list of the packages I need

list.of.packages <- c("tidyverse", 
                      "raster", 
                      "RStoolbox", 
                      "viridis",
                      "gridExtra",
                      "stargazer")

# with this line of code I check if alll the packages are installed and then I load it

{
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) install.packages(new.packages)
  
  lapply(list.of.packages, require, character.only = TRUE)
}

# =========================== #
       ## Data import ##
# =========================== #

# I used this piece of code to create stack of bands I need for the further analysis

# list_img <- list.files(pattern = "20200104_B")
# img_allbands <- lapply(list_img, raster)
# img_stack <- stack(img_allbands)
# writeRaster(img_stack, filename="delta_p191r29_20200104.grd", format="raster")

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


# =================================== #
      ## Bird population trend ##
# =================================== #


data_birds_raw <- read.csv("data_birds.csv")
data_taxbirds <- read.csv("bird_check_list.csv")

source <- data_birds_raw%>%
                dplyr::select(matches("_source"))%>%
                colnames()

data_taxbirds <- data_taxbirds%>%
                      dplyr::select("speciescode", "speciesname","taxOrder", "taxFamily", "taxGroup_en", "taxFamily_en")%>%
                      distinct(.keep_all = TRUE)


variable <- c("country", "season","speciescode", "speciesname", "common_speciesname", "population_date",
              "population_size_unit", "population_size_min", "population_size_max",
              "population_method", "population_trend_period", "population_trend",
              "population_trend_method", "population_trend_long_period", "population_trend_long",
              "population_trend_long_method", "taxGroup_en")

data_birds <- data_birds_raw%>%
                    dplyr::select(-which(names(data_birds_raw) %in% source))%>%  
                    filter(country == "IT", season == "W")%>%
                    left_join(data_taxbirds, by = c("speciescode", "speciesname"), keep = F)%>%
                    dplyr::select(all_of(variable))%>%
                    filter(taxGroup_en != "Hawks and Eagles")%>%
                    arrange(speciescode, sort = T)%>%
                    group_by(population_trend)


birds_pop_trend <- data_birds%>%
count(population_trend)

pop_trend_plot <- ggplot(birds_pop_trend, aes(x = "", y = n, fill = population_trend)) +
                      geom_col(color = "black")+
                      geom_text(aes(label = n),
                                color = "white",
                                position = position_stack(vjust = 0.5)) +
                      coord_polar(theta = "y")+
                      guides(fill = guide_legend(title = "Population trend")) +
                      scale_fill_manual(values = c("#FF4E4E", "#42AF32", "#FFA926", "#000000"),
                                        labels = c("Decreasing", "Increasing", "Stable", "Unknown")) +
                      theme_void()+ 
                      ggtitle("       Populations trends of italian wintering seabirds and shorebirds (2007 - 2015)")

ggsave("pop_trend_plot.jpeg", plot = pop_trend_plot)


birds_pop_trend_long <- data_birds%>%
group_by(population_trend_long)%>%
count(population_trend_long)

pop_trend_long_plot <- ggplot(birds_pop_trend_long, aes(x = "", y = n, fill = population_trend_long)) +
                          geom_col(color = "black")+
                          geom_text(aes(label = n),
                                    color = "white",
                                    position = position_stack(vjust = 0.5)) +
                          coord_polar(theta = "y")+
                          guides(fill = guide_legend(title = "Population trend")) +
                          scale_fill_manual(values = c("#FF4E4E", "#42AF32", "#FFA926", "#000000"),
                                            labels = c("Decreasing", "Increasing", "Stable", "Unknown")) +
                          theme_void()+ 
                          ggtitle("       Populations trends of italian wintering seabirds and shorebirds (1991 - 2015)")

ggsave("pop_trend_long_plot.jpeg", plot = pop_trend_long_plot)


data_birds_list <- data_birds%>%
                        dplyr::select("speciesname", "common_speciesname", "population_trend_period", "population_trend",
                                      "population_trend_long_period", "population_trend_long", "taxGroup_en")%>%
                        rename("species_name" = "speciesname",
                               "italian_name" = "common_speciesname",
                               "pop_trend_period" = "population_trend_period",
                               "pop_trend" = "population_trend",
                               "pop_trend_long_period" = "population_trend_long_period",
                               "pop_trend_long" = "population_trend_long",
                               "taxonomic_group" = "taxGroup_en")%>%
                        arrange(pop_trend_long)
                               

stargazer(data_birds_list,
          summary = FALSE,
          type = "text",
          digit.separator = ".",
          out = "wintering_birds.txt")


# =========================== #
       ## False color ##
# =========================== #


    # Oristano

# R = nir, G = red, B = green

 oristano_plot_nrg1 <- ggRGB(oristano_cropped[[1]], 4, 3, 2, stretch = "lin") +
   labs(title = "Oristano 1989",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 oristano_plot_nrg2 <- ggRGB(oristano_cropped[[2]], 4, 3, 2, stretch = "lin") +
   labs(title = "Oristano 1999",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 oristano_plot_nrg3 <- ggRGB(oristano_cropped[[3]], 4, 3, 2, stretch = "lin") +
   labs(title = "Oristano 2007",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 oristano_plot_nrg4 <- ggRGB(oristano_cropped[[4]], 6, 5, 4, stretch = "lin") +
   labs(title = "Oristano 2017",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

 oristano_plot_nrg5 <- ggRGB(oristano_cropped[[5]], 6, 5, 4, stretch = "lin") +
   labs(title = "Oristano 2023",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat9 OLI/TIRS") +
  theme_void()

oristano_nrg_full_plot <- grid.arrange (oristano_plot_nrg1,  oristano_plot_nrg3, oristano_plot_nrg4, oristano_plot_nrg5, nrow=2)
#ggsave("oristano_nrg_plot.jpeg", plot = oristano_nrg_full_plot)

#grid.arrange (oristano_plot_nrg3, oristano_plot_nrg5, nrow=1)


# R = swir, G = swir, B = red

 oristano_plot_ssr1 <- ggRGB(oristano_cropped[[1]], 7, 5, 3, stretch = "lin") +
   labs(title = "Oristano 1989",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 oristano_plot_ssr2 <- ggRGB(oristano_cropped[[2]], 7, 5, 3, stretch = "lin") +
   labs(title = "Oristano 1999",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 oristano_plot_ssr3 <- ggRGB(oristano_cropped[[3]], 7, 5, 3, stretch = "lin") +
   labs(title = "Oristano 2007",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 oristano_plot_ssr4 <- ggRGB(oristano_cropped[[4]], 8, 7, 5, stretch = "lin") +
   labs(title = "Oristano 2017",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

 oristano_plot_ssr5 <- ggRGB(oristano_cropped[[5]], 8, 7, 5, stretch = "lin") +
   labs(title = "Oristano 2023",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat9 OLI/TIRS") +
  theme_void()

# grid.arrange (oristano_plot_ssr1, oristano_plot_ssr3, oristano_plot_ssr4,oristano_ plot_ssr5, nrow=2)

# grid.arrange (oristano_plot_ssr4, oristano_plot_ssr5, nrow=1)


# R = swir1, G = nir, B = red

oristano_plot_s1nr1 <- ggRGB(oristano_cropped[[1]], 5, 4, 3, stretch = "lin") +
   labs(title = "Oristano 1989",
       subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

oristano_plot_s1nr2 <- ggRGB(oristano_cropped[[2]], 5, 4, 3, stretch = "lin") +
   labs(title = "Oristano 1999",
        subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

oristano_plot_s1nr3 <- ggRGB(oristano_cropped[[3]], 5, 4, 3, stretch = "lin") +
   labs(title = "Oristano 2007",
       subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

oristano_plot_s1nr4 <- ggRGB(oristano_cropped[[4]], 7, 6, 5, stretch = "lin") +
   labs(title = "Oristano 2017",
       subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

oristano_plot_s1nr5 <- ggRGB(oristano_cropped[[5]], 7, 6, 5, stretch = "lin") +
   labs(title = "Oristano 2023",
       subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat9 OLI/TIRS") +
  theme_void()

# grid.arrange (oristano_plot_s1nr1, oristano_plot_s1nr3, oristano_plot_s1nr4, oristano_plot_s1nr5, nrow=2)

# grid.arrange (oristano_plot_s1nr1, oristano_plot_s1nr5, nrow=1)


# R = swir2, G = nir, B = red

oristano_plot_s2nr1 <- ggRGB(oristano_cropped[[1]], 7, 4, 3, stretch = "lin") +
   labs(title = "Oristano 1989",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

oristano_plot_s2nr2 <- ggRGB(oristano_cropped[[2]], 7, 4, 3, stretch = "lin") +
   labs(title = "Oristano 1999",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

oristano_plot_s2nr3 <- ggRGB(oristano_cropped[[3]], 7, 4, 3, stretch = "lin") +
   labs(title = "Oristano 2007",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

oristano_plot_s2nr4 <- ggRGB(oristano_cropped[[4]], 8, 6, 5, stretch = "lin") +
   labs(title = "Oristano 2017",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

oristano_plot_s2nr5 <- ggRGB(oristano_cropped[[5]], 8, 6, 5, stretch = "lin") +
   labs(title = "Oristano 2023",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat9 OLI/TIRS") +
  theme_void()

 grid.arrange (oristano_plot_s2nr1, oristano_plot_s2nr3, oristano_plot_s2nr4, oristano_plot_s2nr5, nrow=2)

grid.arrange (oristano_plot_s2nr1, oristano_plot_s2nr5, nrow=1)


    # Cagliari

# R = nir, G = red, B = green

 cagliari_plot_nrg1 <- ggRGB(cagliari_cropped[[1]], 4, 3, 2, stretch = "lin") +
   labs(title = "Cagliari 1989",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 cagliari_plot_nrg2 <- ggRGB(cagliari_cropped[[2]], 4, 3, 2, stretch = "lin") +
   labs(title = "Cagliari 1996",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 cagliari_plot_nrg3 <- ggRGB(cagliari_cropped[[3]], 4, 3, 2, stretch = "lin") +
   labs(title = "Cagliari 2005",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 cagliari_plot_nrg4 <- ggRGB(cagliari_cropped[[4]], 6, 5, 4, stretch = "lin") +
   labs(title = "Cagliari 2017",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

 cagliari_plot_nrg5 <- ggRGB(cagliari_cropped[[5]], 6, 5, 4, stretch = "lin") +
   labs(title = "Cagliari 2022",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat9 OLI/TIRS") +
  theme_void()

# grid.arrange (cagliari_plot_nrg1, cagliari_plot_nrg3, cagliari_plot_nrg4, cagliari_plot_nrg5, nrow=2)

# grid.arrange (cagliari_plot_nrg1, cagliari_plot_nrg5, nrow=1)


# R = swir, G = swir, B = red

 cagliari_plot_ssr1 <- ggRGB(cagliari_cropped[[1]], 7, 5, 3, stretch = "lin") +
   labs(title = "Cagliari 1989",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 cagliari_plot_ssr2 <- ggRGB(cagliari_cropped[[2]], 7, 5, 3, stretch = "lin") +
   labs(title = "Cagliari 1996",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 cagliari_plot_ssr3 <- ggRGB(cagliari_cropped[[3]], 7, 5, 3, stretch = "lin") +
   labs(title = "Cagliari 2005",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 cagliari_plot_ssr4 <- ggRGB(cagliari_cropped[[4]], 7, 5, 3, stretch = "lin") +
   labs(title = "Cagliari 2017",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

 cagliari_plot_ssr5 <- ggRGB(cagliari_cropped[[5]], 8, 7, 5, stretch = "lin") +
   labs(title = "Cagliari 2022",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat9 OLI/TIRS") +
  theme_void()

# grid.arrange (cagliari_plot_ssr1, cagliari_plot_ssr3, cagliari_plot_ssr4, cagliari_plot_ssr5, nrow=2)

# grid.arrange (cagliari_plot_ssr1, cagliari_plot_ssr5, nrow=1)


# R = swir1, G = nir, B = red

cagliari_plot_s1nr1 <- ggRGB(cagliari_cropped[[1]], 5, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 1989",
       subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

cagliari_plot_s1nr2 <- ggRGB(cagliari_cropped[[2]], 5, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 1996",
       subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

cagliari_plot_s1nr3 <- ggRGB(cagliari_cropped[[3]], 5, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 2005",
       subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

cagliari_plot_s1nr4 <- ggRGB(cagliari_cropped[[4]], 5, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 2017",
       subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

cagliari_plot_s1nr5 <- ggRGB(cagliari_cropped[[5]], 7, 6, 5, stretch = "lin") +
   labs(title = "Cagliari 2022",
       subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat9 OLI/TIRS") +
  theme_void()

grid.arrange (cagliari_plot_s1nr1, cagliari_plot_s1nr3, cagliari_plot_s1nr4, cagliari_plot_s1nr5, nrow=2)

grid.arrange (cagliari_plot_s1nr1, cagliari_plot_s1nr5, nrow=1)


# R = swir2, G = nir, B = red

cagliari_plot_s2nr1 <- ggRGB(cagliari_cropped[[1]], 7, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 1989",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

cagliari_plot_s2nr2 <- ggRGB(cagliari_cropped[[2]], 7, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 1996",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

cagliari_plot_s2nr3 <- ggRGB(cagliari_cropped[[3]], 7, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 2005",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

cagliari_plot_s2nr4 <- ggRGB(cagliari_cropped[[4]], 7, 4, 3, stretch = "lin") +
   labs(title = "Cagliari 2017",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

cagliari_plot_s2nr5 <- ggRGB(cagliari_cropped[[5]], 8, 6, 5, stretch = "lin") +
   labs(title = "Cagliari 2022",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat9 OLI/TIRS") +
  theme_void()

# grid.arrange (cagliari_plot_s2nr1, cagliari_plot_s2nr3, cagliari_plot_s2nr4, cagliari_plot_s2nr5, nrow=2)

# grid.arrange (cagliari_plot_s2nr1, cagliari_plot_s2nr5, nrow=1)


    # Po delta

# R = nir, G = red, B = green

 delta_plot_nrg1 <- ggRGB(delta_cropped[[1]], 4, 3, 2, stretch = "lin") +
   labs(title = "Po delta 1987",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 delta_plot_nrg2 <- ggRGB(delta_cropped[[2]], 4, 3, 2, stretch = "lin") +
   labs(title = "Po delta 1995",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 delta_plot_nrg3 <- ggRGB(delta_cropped[[3]], 4, 3, 2, stretch = "lin") +
   labs(title = "Po delta 2003",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 delta_plot_nrg4 <- ggRGB(delta_cropped[[4]], 6, 5, 4, stretch = "lin") +
   labs(title = "Po delta 2015",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

 delta_plot_nrg5 <- ggRGB(delta_cropped[[5]], 6, 5, 4, stretch = "lin") +
   labs(title = "Po delta 2020",
       subtitle = "R = NIR, G = Red and B = Green",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

# grid.arrange (delta_plot_nrg1, delta_plot_nrg3, delta_plot_nrg4, delta_plot_nrg5, nrow=2)
# grid.arrange (delta_plot_nrg1, delta_plot_nrg5, nrow=1)


# R = swir, G = swir, B = red

 delta_plot_ssr1 <- ggRGB(delta_cropped[[1]], 7, 5, 3, stretch = "lin") +
   labs(title = "Po delta 1987",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 delta_plot_ssr2 <- ggRGB(delta_cropped[[2]], 7, 5, 3, stretch = "lin") +
   labs(title = "Po delta 1995",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 delta_plot_ssr3 <- ggRGB(delta_cropped[[3]], 7, 5, 3, stretch = "lin") +
   labs(title = "Po delta 2005",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

 delta_plot_ssr4 <- ggRGB(delta_cropped[[4]], 8, 7, 5, stretch = "lin") +
   labs(title = "Po delta 2015",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

 delta_plot_ssr5 <- ggRGB(delta_cropped[[5]], 8, 7, 5, stretch = "lin") +
   labs(title = "Po delta 2020",
       subtitle = "R = SWIR1, G = SWIR2 and B = Red",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void() +
  theme_void()

# grid.arrange (delta_plot_ssr1, delta_plot_ssr3, delta_plot_ssr4, delta_plot_ssr5, nrow=2)
# grid.arrange (delta_plot_ssr1, delta_plot_ssr5, nrow=1)


# R = swir1, G = nir, B = red

delta_plot_s1nr1 <- ggRGB(delta_cropped[[1]], 5, 4, 3, stretch = "lin") +
   labs(title = "Po delta 1987",
       subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

delta_plot_s1nr2 <- ggRGB(delta_cropped[[2]], 5, 4, 3, stretch = "lin") +
   labs(title = "Po delta 1995",
       subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

delta_plot_s1nr3 <- ggRGB(delta_cropped[[3]], 5, 4, 3, stretch = "lin") +
   labs(title = "Po delta 2003",
       subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

delta_plot_s1nr4 <- ggRGB(delta_cropped[[4]], 7, 6, 5, stretch = "lin") +
   labs(title = "Po delta 2015",
       subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

delta_plot_s1nr5 <- ggRGB(delta_cropped[[5]], 7, 6, 5, stretch = "lin") +
   labs(title = "Po delta 2020",
       subtitle = "R = SWIR1, G = NIR and B = Red",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

# grid.arrange (delta_plot_s1nr1, delta_plot_s1nr3, delta_plot_s1nr4, delta_plot_s1nr5, nrow=2)
# grid.arrange (delta_plot_s1nr1, delta_plot_s1nr5, nrow=1)


# R = swir2, G = nir, B = red

delta_plot_s2nr1 <- ggRGB(delta_cropped[[1]], 7, 4, 3, stretch = "lin") +
   labs(title = "Po delta 1987",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

delta_plot_s2nr2 <- ggRGB(delta_cropped[[2]], 7, 4, 3, stretch = "lin") +
   labs(title = "Po delta 1995",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

delta_plot_s2nr3 <- ggRGB(delta_cropped[[3]], 7, 4, 3, stretch = "lin") +
   labs(title = "Po delta 2003",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat4 TM") +
  theme_void()

delta_plot_s2nr4 <- ggRGB(delta_cropped[[4]], 8, 6, 5, stretch = "lin") +
   labs(title = "Po delta 2015",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

delta_plot_s2nr5 <- ggRGB(delta_cropped[[5]], 8, 6, 5, stretch = "lin") +
   labs(title = "Po delta 2020",
       subtitle = "R = SWIR2, G = NIR and B = Red",
       caption = "Data source: Landsat8 OLI/TIRS") +
  theme_void()

delta_falsecolor_plot <- grid.arrange (delta_plot_s2nr1, delta_plot_s2nr3, delta_plot_s2nr4, delta_plot_s2nr5, nrow=2)
grid.arrange (delta_plot_s2nr1, delta_plot_s2nr5, nrow=1)


# =============================================================== #

# Modify this code to plot and save false color images

nrg_full_plot <- grid.arrange (oristano_plot_nrg1, cagliari_plot_nrg1, delta_plot_nrg1,
                               oristano_plot_nrg5, cagliari_plot_nrg5, delta_plot_nrg5,
                               nrow=2)
# ggsave("nrg_plot.jpeg", plot = nrg_full_plot)

# =============================================================== #

s2nr_full_plot <- grid.arrange (oristano_plot_s2nr1, cagliari_plot_s2nr1, delta_plot_s2nr1,
                                 oristano_plot_s2nr5, cagliari_plot_s2nr5, delta_plot_s2nr5,
                                 nrow=2)
# ggsave("s2nr_plot.jpeg", plot = s2nr_full_plot)


# =============================== #
       ## Spectral indices ##
# =============================== #


    ## Oristano indices

oristano89_indices <- RStoolbox::spectralIndices(oristano_cropped[[1]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
oristano95_indices <- RStoolbox::spectralIndices(oristano_cropped[[2]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
oristano07_indices <- RStoolbox::spectralIndices(oristano_cropped[[3]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
oristano17_indices <- RStoolbox::spectralIndices(oristano_cropped[[4]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
oristano23_indices <- RStoolbox::spectralIndices(oristano_cropped[[5]], blue = 3, green = 4, red = 5, nir = 6, swir2 = 7, swir3= 8, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))

# Let's 
oristano_ndvi_diff <- oristano23_indices$NDVI - oristano89_indices$NDVI

oristano_ndvi_plot <-
 ggplot()+
  geom_raster(oristano_ndvi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Oristano",
       subtitle = "Difference in NDVI index between 2023 and 1989",
       caption = "Data source: Landsat 4 and 9 images") +
  theme_void()
ggsave("oristano_ndvi_diff_plot.jpeg", plot = oristano_ndvi_plot)


oristano_ndwi_diff <- oristano23_indices$NDWI - oristano89_indices$NDWI

oristano_ndwi_plot <-
 ggplot()+
  geom_raster(oristano_ndwi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Oristano",
       subtitle = "Difference in NDWI index between 2023 and 1989",
       caption = "Data source: Landsat 4 and 9 images") +
  theme_void()
ggsave("oristano_ndwi_diff_plot.jpeg", plot = oristano_ndwi_plot)


oristano_ndwi2_diff <- oristano23_indices$NDWI2 - oristano89_indices$NDWI2

oristano_ndwi2_plot <-
 ggplot()+
  geom_raster(oristano_ndwi2_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Oristano",
       subtitle = "Difference in NDWI2 index between 2023 and 1989",
       caption = "Data source: Landsat 4 and 9 images") +
  theme_void()
ggsave("oristano_ndwi2_diff_plot.jpeg", plot = oristano_ndwi2_plot)


oristano_slavi_diff <- oristano23_indices$SLAVI - oristano89_indices$SLAVI

oristano_slavi_plot <-
 ggplot()+
  geom_raster(oristano_slavi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Oristano",
       subtitle = "Difference in SLAVI index between 2023 and 1989",
       caption = "Data source: Landsat 4 and 9 images") +
  theme_void()
ggsave("oristano_slavi_diff_plot.jpeg", plot = oristano_slavi_plot)


    ## Cagliari indices

cagliari89_indices <- RStoolbox::spectralIndices(cagliari_cropped[[1]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
cagliari96_indices <- RStoolbox::spectralIndices(cagliari_cropped[[2]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
cagliari05_indices <- RStoolbox::spectralIndices(cagliari_cropped[[3]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
cagliari17_indices <- RStoolbox::spectralIndices(cagliari_cropped[[4]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
cagliari22_indices <- RStoolbox::spectralIndices(cagliari_cropped[[5]], blue = 3, green = 4, red = 5, nir = 6, swir2 = 7, swir3= 8, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))

# let's create plots of differences between indices of different years
# NDVI difference
cagliari_ndvi_diff <- cagliari22_indices$NDVI - cagliari89_indices$NDVI

cagliari_ndvi_plot <-
 ggplot()+
  geom_raster(cagliari_ndvi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Cagliari",
       subtitle = "Difference in NDVI index between 2022 and 1989",
       caption = "Data source: Landsat 4 and 9 images") +
  theme_void()
ggsave("cagliari_ndvi_diff_plot.jpeg", plot = cagliari_ndvi_plot)

# NDWI difference
cagliari_ndwi_diff <- cagliari22_indices$NDWI - cagliari89_indices$NDWI

cagliari22_NDWI <- stretch(cagliari22_indices$NDWI, minv=-1, maxv=1)

  cagliari_ndwi_plot <-
   ggplot()+
    geom_raster(cagliari_ndwvi_diff, mapping = aes(x=x, y=y, fill = layer))+
    scale_fill_viridis("viridis") + 
    labs(title = "Cagliari",
         subtitle = "Difference in NDWI index between 2022 and 1989",
         caption = "Data source: Landsat 4 and 9 images") #+
    theme_void()
ggsave("cagliari_ndwi_diff_plot.jpeg", plot = cagliari_ndwi_plot)

# NDWI2 difference
cagliari_ndwi2_diff <- cagliari22_indices$NDWI2 - cagliari89_indices$NDWI2

cagliari_ndwi2_plot <-
 ggplot()+
  geom_raster(cagliari_ndwi2_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Cagliari",
       subtitle = "Difference in NDWI2 index between 2022 abnd 1989",
       caption = "Data source: Landsat 4 and 9 images") +
  theme_void()
ggsave("cagliari_ndwi2_diff_plot.jpeg", plot = cagliari_ndwi2_plot)

# SLAVI difference
cagliari_slavi_diff <- cagliari22_indices$SLAVI - cagliari89_indices$SLAVI

cagliari_slavi_plot <-
 ggplot()+
  geom_raster(cagliari_slavi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Cagliari",
       subtitle = "Difference in SLAVI index between 2022 and 1989",
       caption = "Data source: Landsat 4 and 9 images") +
  theme_void()
ggsave("cagliari_slavi_diff_plot.jpeg", plot = cagliari_slavi_plot)


    ## Po delta indices

delta87_indices <- RStoolbox::spectralIndices(delta_cropped[[1]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
delta95_indices <- RStoolbox::spectralIndices(delta_cropped[[2]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
delta03_indices <- RStoolbox::spectralIndices(delta_cropped[[3]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
delta15_indices <- RStoolbox::spectralIndices(delta_cropped[[4]], blue = 1, green = 2, red = 3, nir = 4, swir2 = 5, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))
delta20_indices <- RStoolbox::spectralIndices(delta_cropped[[5]], blue = 2, green = 3, red = 4, nir = 5, swir2 = 6, swir3= 7, indices = c("NDVI", "NDWI", "NDWI2", "SLAVI"))

# let's create plots of differences between indices of different years
# NDVI difference
delta_ndvi_diff <- delta20_indices$NDVI - delta87_indices$NDVI

delta_ndvi_plot <-
 ggplot()+
  geom_raster(delta_ndvi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Po delta",
       subtitle = "Difference in NDVI index between 2020 and 1987",
       caption = "Data source: Landsat 4 and 8 images") +
  theme_void()
ggsave("delta_ndvi_diff_plot.jpeg", plot = delta_ndvi_plot)

# NDWI difference
delta_ndwi_diff <- delta20_indices$NDWI - delta87_indices$NDWI

delta_ndwi_plot <-
 ggplot()+
  geom_raster(delta_ndwi_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Po delta",
       subtitle = "Difference in NDWI index between 2020 and 1987",
       caption = "Data source: Landsat 4 and 8 images") +
  theme_void()
ggsave("delta_ndwi_diff_plot.jpeg", plot = delta_ndwi_plot)

# NDWI2 difference
delta_ndwi2_diff <- delta20_indices$NDWI2 - delta87_indices$NDWI2

delta_ndwi2_plot <-
 ggplot()+
  geom_raster(delta_ndwi2_diff, mapping = aes(x=x, y=y, fill = layer))+
  scale_fill_viridis("viridis") + 
  labs(title = "Po delta",
       subtitle = "Difference in NDWI2 index between 2020 and 1987",
       caption = "Data source: Landsat 4 and 8 images") +
  theme_void()
ggsave("delta_ndwi2_diff_plot.jpeg", plot = delta_ndwi2_plot)

# SLAVI difference
delta_slavi_diff <- delta20_indices$SLAVI - delta87_indices$SLAVI

delta_slavi_plot <-
 ggplot() +
  geom_raster(delta_slavi_diff, mapping = aes(x=x, y=y, fill = layer)) +
  scale_fill_viridis() + 
  labs(title = "Po delta",
       subtitle = "Difference in SLAVI index between 2020 and 1987",
       caption = "Data source: Landsat 4 and 8 images") +
  theme_void()
ggsave("delta_slavi_diff_plot.jpeg", plot = delta_slavi_plot)
    

# ===================================================== #
       ## Land Use and Land Cover Classification ##
# ===================================================== #


    ## Oristano

set.seed(999)
oristano89_lcc <- unsuperClass(oristano_cropped[[1]], nSamples = 1000, nClasses = 7)
oristano99_lcc <- unsuperClass(oristano_cropped[[2]], nSamples = 1000, nClasses = 7)
oristano07_lcc <- unsuperClass(oristano_cropped[[3]], nSamples = 1000, nClasses = 7)
oristano17_lcc <- unsuperClass(oristano_cropped[[4]], nSamples = 1000, nClasses = 7)
oristano23_lcc <- unsuperClass(oristano_cropped[[5]], nSamples = 1000, nClasses = 7)

oristano_lcc <- list(oristano89_lcc, oristano99_lcc, oristano07_lcc, oristano17_lcc, oristano23_lcc)

par(mfrow = c(1, 4))
plot(oristano_lcc[[1]]$map)
plot(oristano_lcc[[3]]$map)
plot(oristano_lcc[[4]]$map)
plot(oristano_lcc[[5]]$map)


freq(oristano_lcc[[1]]$map)

#      value  count
# [1,]     1  53309 Agriculture field
# [2,]     2 257931 
# [3,]     3 659368 Water
# [4,]     4  30029 Sand and bare soil
# [5,]     5 202108
# [6,]     6 161778
# [7,]     7 208302

freq(oristano_lcc[[5]]$map)


    ## Cagliari

cagliari89_lcc <- unsuperClass(cagliari_cropped[[1]], nSamples = 1000, nClasses = 7)
cagliari96_lcc <- unsuperClass(cagliari_cropped[[2]], nSamples = 1000, nClasses = 7)
cagliari05_lcc <- unsuperClass(cagliari_cropped[[3]], nSamples = 1000, nClasses = 7)
cagliari17_lcc <- unsuperClass(cagliari_cropped[[4]], nSamples = 1000, nClasses = 7)
cagliari22_lcc <- unsuperClass(cagliari_cropped[[5]], nSamples = 1000, nClasses = 7)

cagliari_lcc <- list(cagliari89_lcc, cagliari96_lcc, cagliari05_lcc, cagliari17_lcc, cagliari22_lcc)

par(mfrow = c(2, 2))
plot(cagliari_lcc[[1]]$map)
plot(cagliari_lcc[[3]]$map)
plot(cagliari_lcc[[4]]$map)
plot(cagliari_lcc[[5]]$map)


    ## Po delta

set.seed(999)
delta87_lcc <- unsuperClass(delta_cropped[[1]], nSamples = 1000, nClasses = 7)
delta95_lcc <- unsuperClass(delta_cropped[[2]], nSamples = 1000, nClasses = 7)
delta03_lcc <- unsuperClass(delta_cropped[[3]], nSamples = 1000, nClasses = 7)
delta15_lcc <- unsuperClass(delta_cropped[[4]], nSamples = 1000, nClasses = 7)
delta20_lcc <- unsuperClass(delta_cropped[[5]], nSamples = 1000, nClasses = 7)

delta_lcc <- list(delta87_lcc, delta95_lcc, delta03_lcc, delta15_lcc, delta20_lcc)

par(mfrow = c(1, 4))
plot(delta_lcc[[1]]$map)
plot(delta_lcc[[3]]$map)
plot(delta_lcc[[4]]$map)
plot(delta_lcc[[5]]$map)


# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# WHY THIS DOESN'T WORK??? #

# for loop to create all the graph of a region with ggplot2

# Load the data into a list
#cagliari_lcc <- list(cagliari84_lcc, cagliari93_lcc, cagliari02_lcc, cagliari11_lcc, cagliari21_lcc)

# Initialize a counter to keep track of the plot number
#plot_num <- 1

# Loop through the data in the list
#for (i in 1:length(cagliari_lcc)) {
#  # Extract the current data
#  data <- cagliari_lcc[[i]]
#  
#  # Create a plot using ggplot2
#  p <- ggplot(data, aes(x = wt, y = mpg)) +
#    geom_raster() +
#    ggtitle(paste0("Plot ", plot_num))
#  
#  # Increment the plot number
#  plot_num <- plot_num + 1
#  
#  # Show the plot
#  print(p)
#}


# ALSO THIS DOESN'T WORK!!!

# for loop to create all the Land Civer Classification of a region with ggplot2

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

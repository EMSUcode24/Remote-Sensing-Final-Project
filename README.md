# Remote-Sensing-Final-Project
###Final Project
---
title: "Final Project"
author: "Eme Morgan"
date: "2024-04-22"
# output: pdf_document
output: html_document
---

# Load required packages
library(raster)
#install.packages("sf")
library(sf)
#install.packages("randomForest")
library(randomForest)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)


###################################NDVI Code
# Set the directory containing the TIFF images
image_folder <- "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/Scripts/Final_Project/Browser_images"

# List all TIFF files in the folder
tif_files <- list.files(image_folder, pattern = "Sentinel-2_L2A.*\\.tiff$", full.names = TRUE)

# Load TIFF files as a stacked raster
image_stack <- stack(tif_files)

# Calculate NDVI for the stacked raster
red_band <- image_stack[[4]]
nir_band <- image_stack[[8]]
ndvi <- (nir_band - red_band) / (nir_band + red_band)

# Plot NDVI
plot(ndvi, main = "NDVI Model")

# Save NDVI image
writeRaster(ndvi, filename = "ndvi_output.tif", format = "GTiff")


###################################EVI Code
# Set the directory containing the TIFF images
image_folder <- "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/Scripts/Final_Project/Browser_images"

# List all TIFF files in the folder
tif_files <- list.files(image_folder, pattern = "Sentinel-2_L2A.*\\.tiff$", full.names = TRUE)

# Load TIFF files as a stacked raster
image_stack <- stack(tif_files)

# Calculate EVI for the stacked raster
blue_band <- image_stack[[2]]
red_band <- image_stack[[4]]
nir_band <- image_stack[[8]]

# Calculate EVI
evi <- 2.5 * ((nir_band - red_band) / (nir_band + 6 * red_band - 7.5 * blue_band + 1))

# Plot EVI
plot(evi)
plot(evi, col =topo.colors(100), main = "EVI Model")

# Save EVI image
writeRaster(evi, filename = "evi_output.tif", format = "GTiff")
## Export original file to ArcGIS to make cal and val points then export as CSV


################################### Read in Cal and Val data  
# File paths for calibration and validation data
calibration_files <- c("C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/brown_cal.csv", 
                       "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/lightgrn_cal.csv",
                       "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/darkgrn_cal.csv")

validation_files <- c("C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/brown_val.csv",
                      "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/lightgrn_val.csv",
                      "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/darkgrn_val.csv")

### Read in using lapply
calibration_data <- lapply(calibration_files, read.csv)
validation_data <- lapply(validation_files, read.csv)

### Combine all cal and val data
cals <- do.call(rbind, calibration_data)
vals <- do.call(rbind, validation_data)

### Add the land cover color as class to cal and val data 
cals$class<-rep(c("brown","lightgrn","darkgrn"),
                times=c(nrow(calibration_data[[1]]),nrow(calibration_data[[2]]),nrow(calibration_data[[3]])))
vals$class<-rep(c("brown","lightgrn","darkgrn"),
                times=c(nrow(validation_data[[1]]),nrow(validation_data[[2]]),nrow(validation_data[[3]])))


################################### Extract data from NDVI and add data to column in CSV files   
# File paths for NDVI TIFF file and CSV files
ndvi_tif_path <- "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/R_outputs/Tif_image/ndvi_output.tif"
calibration_files <- c("C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/brown_cal.csv", 
                       "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/lightgrn_cal.csv",
                       "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/darkgrn_cal.csv")
validation_files <- c("C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/brown_val.csv",
                      "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/lightgrn_val.csv",
                      "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/darkgrn_val.csv")

# Read NDVI raster file
ndvi_raster <- raster(ndvi_tif_path)

# Make the cals spatial points
plot(ndvi_raster)
points(cals$POINT_X,cals$POINT_Y)
points(cals_sp)
cals_sp <- SpatialPoints(coords = cals[,c(2,3)],proj4string = CRS("+init=epsg:32612"))

# Make the vals spatial points
library(sp)
vals_sp <- SpatialPointsDataFrame(coords = vals[, c("POINT_X", "POINT_Y")], data = vals)
proj4string(vals_sp) <- CRS("+init=epsg:32612")
plot(ndvi_raster)
points(vals_sp, col = "red")  # You can specify color if needed

# Function to extract NDVI values based on X and Y coordinates
cals$extract_ndvi <- extract(ndvi_raster, cals_sp)
vals$extract_ndvi <- extract(ndvi_raster, vals_sp)

# Display extracted NDVI values
print(cals)
print(vals)


################################### Extract data from EVI and add data to column in CSV files 
# File paths for EVI TIFF file and CSV files
evi_tif_path <- "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/R_outputs/Tif_image/evi_output.tif"
calibration_files <- c("C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/brown_cal.csv", 
                       "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/lightgrn_cal.csv",
                       "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/darkgrn_cal.csv")
validation_files <- c("C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/brown_val.csv",
                      "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/lightgrn_val.csv",
                      "C:/Users/f78s561/OneDrive - Montana State University/LRES525/Final Project/ArcGIS_csv_files/darkgrn_val.csv")

# Read EVI raster file
evi_raster <- raster(evi_tif_path)


# Make the cals spatial points
plot(evi_raster)
points(cals$POINT_X,cals$POINT_Y)
points(cals_sp)
cals_sp <- SpatialPoints(coords = cals[,c(2,3)],proj4string = CRS("+init=epsg:32612"))

# Make the vals spatial points
library(sp)
vals_sp <- SpatialPointsDataFrame(coords = vals[, c("POINT_X", "POINT_Y")], data = vals)
proj4string(vals_sp) <- CRS("+init=epsg:32612")
plot(evi_raster)
points(vals_sp, col = "red")  # You can specify color if needed

# Function to extract EVI values based on X and Y coordinates
cals$extract_evi <- extract(evi_raster, cals_sp)
vals$extract_evi <- extract(evi_raster, vals_sp)

# Display extracted EVI values
print(cals)
print(vals)


###################################Train Model
# Load required packages
library(randomForest)
library(dplyr)

# Check the column names in train_data
print(colnames(train_data))

# Prepare training data
train_data <- cals  # Assuming cals contains X, Y, NDVI, EVI, and Class columns
class_labels <- train_data$class  # Assuming Class column contains categories like "darkgrn", "lightgrn", "brown"
train_data <- train_data[, c("OID_","POINT_X","POINT_Y", "class", "extract_ndvi", "extract_evi")]  # Selecting relevant columns as features

# Convert 'class' column to factor
train_data$class <- factor(train_data$class)

# Split training data into training and validation sets
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(train_data), 0.7 * nrow(train_data))
train_data_split <- train_data[train_index, ]
validation_data <- train_data[-train_index, ]
calibration_data <- train_data[-train_index, ]

# Train NDVI model
ndvi_model <- randomForest(class ~ extract_ndvi + POINT_X + POINT_Y, data = train_data_split)

# Train EVI model
evi_model <- randomForest(class ~ extract_evi + POINT_X + POINT_Y, data = train_data_split)

# Predictions on validation set
ndvi_pred <- predict(ndvi_model, validation_data)
evi_pred <- predict(evi_model, validation_data)

# Predictions on calibration set
ndvi_pred <- predict(ndvi_model, calibration_data)
evi_pred <- predict(evi_model, calibration_data)


###################################Evaluate Performance 
# Load required packages
library(caret)

# Function to compute classification metrics
compute_metrics <- function(pred, actual) {
  confusion <- confusionMatrix(data = pred, reference = actual)
  accuracy <- confusion$overall['Accuracy']
  precision <- confusion$table['brown', 'brown'] / sum(confusion$table['brown', ])
  recall <- confusion$table['brown', 'brown'] / sum(confusion$table[, 'brown'])
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  metrics <- list(accuracy = accuracy, precision = precision, recall = recall, f1_score = f1_score)
  return(metrics)
}

# Compute metrics for NDVI model
ndvi_metrics <- compute_metrics(ndvi_pred, validation_data$class)

# Compute metrics for EVI model
evi_metrics <- compute_metrics(evi_pred, validation_data$class)

# Print metrics
print("NDVI Model Metrics:")
print(ndvi_metrics)

print("EVI Model Metrics:")
print(evi_metrics)


################################# NDVI Confusion Table Graphic
table(validation_data$class)

# confusion matrix
(confu <- confusionMatrix(data = ndvi_pred, reference =  validation_data$class))
confutab <- as.data.frame.matrix(confu$table)
sums <- colSums(confutab)

tabs_perc <- matrix(NA, length(sums),length(sums))
for (i in 1:length(sums)){
  tabs_perc[,i] <- confutab[,i]/sums[i]
}
col <- colorRampPalette(c("black","brown","gold","forestgreen")) 

library(corrplot)
# pdf("./PLSDA_corrplot.pdf",width = 7,height = 6,pointsize = 11)
corrplot(tabs_perc, p.mat = tabs_perc, insig = "p-value", sig.level = -1, addCoef.col = 1,
         tl.srt = 70,col = col(20),cl.lim = c(0, 1),tl.col = 1, tl.offset =1.5, 
         cl.ratio = 0.2, cl.align.text = "l", cl.cex = 0.9, 
         mar=c(1,3,3,3))
mtext("Prediction",2, line=0, cex=1.2)
mtext("Reference",at = 2, line = 1.5, cex=1.2)
# dev.off()
confusionMatrix(data = evi_pred, reference =  validation_data$class)


################################# EVI Confusion Table Graphic
table(validation_data$class)

# confusion matrix
(confu <- confusionMatrix(data = evi_pred, reference =  validation_data$class))
confutab <- as.data.frame.matrix(confu$table)
sums <- colSums(confutab)

tabs_perc <- matrix(NA, length(sums),length(sums))
for (i in 1:length(sums)){
  tabs_perc[,i] <- confutab[,i]/sums[i]
}

col <- colorRampPalette(c("black","brown","gold","forestgreen")) 

library(corrplot)
# pdf("./PLSDA_corrplot.pdf",width = 7,height = 6,pointsize = 11)
corrplot(tabs_perc, p.mat = tabs_perc, insig = "p-value", sig.level = -1, addCoef.col = 1,
         tl.srt = 70,col = col(20),cl.lim = c(0, 1),tl.col = 1, tl.offset =1.5, 
         cl.ratio = 0.2, cl.align.text = "l", cl.cex = 0.9, 
         mar=c(1,3,3,3))
mtext("Prediction",2, line=0, cex=1.2)
mtext("Reference",at = 2, line = 1.5, cex=1.2)
# dev.off()

confusionMatrix(data = evi_pred, reference =  validation_data$class)

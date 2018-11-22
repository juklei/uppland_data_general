## Field forest measurements are summarised for analysis. 
## 
## Further this script creates ALS data used in the uppland
## project. At the moment only ElevP95,  PercentAbove0.5m and 
## PercentAbove5m are implemented.
## 
## Field forest data and ALS data are merged to two files:
##
## forest_data_uppland_plot.csv contains all data at the plot level.
## forest_data_uppland_subplot.csv contains all data at the subplot level.
##

## First edit: 20181031
## Last edit: 20181105

## Author: Julian Klein

## 1. Clear environment and load libraries -------------------------------------

rm(list = ls())

library(data.table)
library(reshape)
library(raster)
library(rgdal)
library(sp)
library(rgeos)

## 2. Load and explore data ----------------------------------------------------

dir()
forest <- read.csv("data/uppland_forest.csv")
laser <- read.csv("data/uppland_forest_laser.csv")
coord <- read.csv("data/coordinates_from_master_file.csv")

head(forest)
head(coord)
head(laser)

## 3. Process forest data ------------------------------------------------------

## Categoriese into spruce, pine and deciduous and dead wood:

levels(forest$species)[c(1:4, 6)] <- "lov" 

forest <- as.data.table(forest)

## Calculate forest measurment at the plot level:

f_t1 <- forest[forest$dbh_cm >= 5, 
               list("nr" = nrow(.SD), 
                    "average_dbh" = mean(dbh_cm)),
               by = c("plot", "species", "experiment")]

f_t1 <- dcast(f_t1, 
              plot + experiment ~ species, 
              value.var = c("nr", "average_dbh"))

## Replace NA in tree counts with 0's in f_t1:
f_t1[, 3:6][is.na(f_t1[, 3:6])] <- 0

f_t2 <- forest[, list("nr_skarm" = sum(umbrella == "yes")), 
               by = c("plot", "experiment")]

f_plot <- merge(f_t1, f_t2, by = c("plot", "experiment"))

## Calculate forest measurment at the circle_10m level:

f_subplot <- forest[, list("nr_skarm" = sum(umbrella == "yes")), 
                    by = c("plot", "circle_10m", "experiment")]

## 4. Process ground laser data ------------------------------------------------

## Replace -9999 (laser of measurement device beyond max distance) values with 
## the maximum value for "leaves" and "angle_vertical" seperately: 

laser <- data.table(na.omit(laser)) ## Makes sure max is not NA

laser[, "dist_m" := max(distance), 
      by = c("angle_vertical", "leaves", "experiment")]

laser$distance[laser$distance == -9999] <- laser$dist_m[laser$distance == -9999]

## Calculate mean distance plot and angle:

laser[, "laser_mean" := mean(distance), by = c("plot", "experiment")]

laser <- unique(laser[, c("plot", "laser_mean", "experiment")])

## 5. Create ALS data ----------------------------------------------------------

# ## Specify directory in which the .asc/.tif files are found and to be stored:
# dir_ALS <- "N:/Uppland project/Laserdata/"
# dir(dir_ALS)
# 
# ## File names
# n <- c("ElevP95", "PercentAbove0.5m", "PercentAbove5m")
# 
# ## Load base files into a raster stack:
# base <- stack(paste0(dir_ALS, "all_plots_", n, ".asc"))
# names(base) <- n
# 
# ## Add undergrowth density:
# base$PercentBelow5m <- base[["PercentAbove0.5m"]] -  base[["PercentAbove5m"]]
# 
# ## Reduce data set:
# red <- base[[-which(n == "PercentAbove0.5m")]]
# 
# ## Store length of red for later use
# lr <- length(red[1])
# 
# ## Extract data:
# 
# buffer <- c(10, 50) ## Chose extraction buffers here
# 
# ## Create data set which will be filled by the loop:
# 
# extracted <- cbind(coord,
#                    matrix(NA, ncol = lr),
#                    sort(rep(buffer, nrow(coord))))
# colnames(extracted)[6:length(extracted)] <- c(names(red), "buffer")
# 
# ## Run the loop across all buffers (time consuming):
# 
# for (i in buffer) {
# 
#   bol1 <- extracted$buffer == i
#   extracted[bol1, 6:(5+lr)] <- extract(red,
#                                        extracted[bol1, c("east", "north")],
#                                        fun = mean,
#                                        na.rm = TRUE,
#                                        buffer = i)
# 
# }
# 
# dir.create("temp")
# write.csv(extracted, "temp/forest_ALS_uppland.csv", row.names = FALSE)
extracted <- read.csv("temp/forest_ALS_uppland.csv")

## 6. Merge data sets and export -----------------------------------------------

forest_plot <- merge(f_plot, laser, by = c("plot", "experiment"))
forest_plot <- merge(extracted[extracted$buffer == 50 & 
                               extracted$circle_10m == "middle", ],
                     forest_plot,
                     by = "plot") 

forest_subplot <- merge(extracted[extracted$buffer == 10, ], 
                        f_subplot,
                        by = c("plot", "circle_10m")) 

dir.create("clean")
write.csv(forest_plot, "clean/forest_data_uppland_plot.csv", row.names = FALSE)
write.csv(forest_subplot, "clean/forest_data_uppland_subplot.csv", 
          row.names = FALSE)

## -----------------------------------END---------------------------------------
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
## Last edit: 20191106

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
ffmf <- read.csv("data/ffmf_derived.csv")

head(forest)
head(ffmf)
head(laser)

## 3. Process forest data ------------------------------------------------------

forest <- as.data.table(forest)

## Calculate plot level tree species richness for later use:

plsr <- forest[forest$species != "staende_dodved", ]
plsr <- plsr[, list("tspr" = length(unique(species))), 
             by = c("plot", "experiment")]

## Categoriese into spruce, pine and deciduous, alive trees and dead wood:

levels(forest$species)[c(1:4, 6)] <- "lov"

forest_t1 <- forest[forest$species != "staende_dodved", ]
forest_t1$species <- "all_alive"
forest <- rbind(forest, forest_t1)

## Calculate forest measurements at the plot level:

f_t1 <- forest[forest$dbh_cm >= 5, 
               list("nr" = nrow(.SD), 
                    "average_dbh" = mean(dbh_cm),
                    "sd_dbh" = sd(dbh_cm)),
               by = c("plot", "species", "experiment")]

f_t1 <- dcast(f_t1, 
              plot + experiment ~ species, 
              value.var = c("nr", "average_dbh", "sd_dbh"))

## Replace NA in tree counts and dbh with 0's in f_t1:
f_t1[, 3:17][is.na(f_t1[, 3:17])] <- 0

f_t2 <- forest[, list("nr_skarm" = sum(umbrella == "yes")), 
               by = c("plot", "experiment")]

f_plot <- merge(f_t1, f_t2, by = c("plot", "experiment"))

## Calculate forest measurment at the circle_10m level:

f_t3 <- forest[forest$dbh_cm >= 5, 
               list("nr" = nrow(.SD), 
                    "average_dbh" = mean(dbh_cm),
                    "sd_dbh" = sd(dbh_cm)),
               by = c("plot", "circle_10m", "species", "experiment")]

f_t3 <- dcast(f_t3, 
              plot + circle_10m + experiment ~ species, 
              value.var = c("nr", "average_dbh", "sd_dbh"))

## Replace NA in tree counts with 0's in f_t1:
f_t3[, 4:18][is.na(f_t3[, 4:18])] <- 0

f_t4 <- forest[, list("nr_skarm" = sum(umbrella == "yes")), 
               by = c("plot", "circle_10m", "experiment")]

f_subplot <- merge(f_t3, f_t4, by = c("plot", "circle_10m", "experiment"))

## 4. Process ground laser data ------------------------------------------------

## Replace -9999 (laser of measurement device beyond max distance) values with 
## the maximum value for "leaves" and "angle_vertical" seperately: 

laser <- data.table(na.omit(laser)) ## Makes sure max is not NA

laser[, "dist_m" := max(distance), 
      by = c("angle_vertical", "leaves", "experiment")]

laser$distance[laser$distance == -9999] <- laser$dist_m[laser$distance == -9999]

## Calculate median distance plot and angle (Median to avoid dominance of 
## extreme values):

laser[, "laser_median" := median(distance), by = c("plot", "experiment")]

laser <- unique(laser[, c("plot", "laser_median", "experiment")])

## 5. Create remote sensing data -----------------------------------------------

# ## Specify directory in which the .asc/.tif files are found and to be stored:
# dir_rs <- "N:/Uppland project/Remote sensing data/"
# dir(dir_rs)
# 
# ## Lidar File names
# n <- c("ElevP95", "PercentAbove0.5m", "PercentAbove2m", "PercentAbove3m", 
#        "PercentAbove5m", "PercentAbove7m")
# 
# ## Load Lidar base files into a raster stack:
# base <- stack(paste0(dir_rs, "all_plots_", n, ".asc"))
# names(base) <- n
# 
# ## Add undergrowth density:
# base$PercentPntrt2m <- 100 - base[["PercentAbove2m"]]
# base$PercentBelow3m <- base[["PercentAbove0.5m"]] -  base[["PercentAbove3m"]]
# base$PercentBelow5m <- base[["PercentAbove0.5m"]] -  base[["PercentAbove5m"]]
# base$PercentBelow7m <- base[["PercentAbove0.5m"]] -  base[["PercentAbove7m"]]
# 
# ## Store length of red for later use
# lr <- length(base[1])
# 
# ## Extract data:
# 
# buffer <- c(10, 50) ## Chose extraction buffers here
# 
# ## Create data set which will be filled by the loop:
# 
# ## Make coordinates file from ffmf:
# coord <- ffmf[is.na(ffmf$insect_trap), c(1:2,7,12:13)]
# colnames(coord)[3] <- "circle_10m"
# coord$circle_10m <- as.character(coord$circle_10m)
# coord$circle_10m[is.na(coord$circle_10m)] <- "middle"
# 
# extracted <- cbind(coord,
#                    matrix(NA, ncol = lr),
#                    sort(rep(buffer, nrow(coord))))
# colnames(extracted)[6:length(extracted)] <- c(names(base), "buffer")
# 
# ## Run the loop across all buffers (time consuming):
# 
# for (i in buffer) {
# 
#   bol1 <- extracted$buffer == i
#   extracted[bol1, 6:(5+lr)] <- extract(base,
#                                        extracted[bol1, c("east", "north")],
#                                        fun = mean,
#                                        na.rm = TRUE,
#                                        buffer = i)
# 
# }
# 
# ## Import the stem diameter data:
# sdbh_rs <- raster(paste0(dir_rs, "sksSkogligaGrunddataDgv03.tif"))
# 
# ## Add the stem diameter to the extracted file:
# for (i in buffer) {
#   
#   bol1 <- extracted$buffer == i
#   extracted$sdbh_rs[bol1] <- extract(sdbh_rs,
#                                      extracted[bol1, c("east", "north")],
#                                      fun = mean,
#                                      na.rm = TRUE,
#                                      buffer = i)
#   
# }
# 
# ## Add "before" because no LiDAR data from after the experiment available:
# extracted$experiment <- "before"
#
# dir.create("temp")
# write.csv(extracted, "temp/forest_RS_uppland.csv", row.names = FALSE)
extracted <- read.csv("temp/forest_RS_uppland.csv")

## 6. Merge data sets and export -----------------------------------------------

forest_plot <- merge(f_plot, 
                     laser, 
                     all.y = TRUE,
                     by = c("plot", "experiment"))
forest_plot <- merge(extracted[extracted$buffer == 50 & 
                               extracted$circle_10m == "middle", -1],
                     forest_plot,
                     all.y = TRUE,
                     by = c("plot", "experiment")) 

forest_subplot <- merge(extracted[, -1], 
                        f_subplot, 
                        all.y = TRUE,
                        by = c("plot", "circle_10m", "experiment")) 

## Add experimental treatment to both:
forest_plot <- merge(forest_plot, 
                     unique(ffmf[, c("plot", 
                                     "block", 
                                     "treatment", 
                                     "effect_year")]), 
                     by = "plot")
forest_subplot <- merge(forest_subplot, 
                        unique(ffmf[, c("plot", 
                                        "block", 
                                        "treatment", 
                                        "effect_year")]), 
                        by = "plot")

## Add tree species richness to plot:
forest_plot <- merge(forest_plot, 
                     plsr, 
                     all.x = TRUE, 
                     by = c("plot", "experiment")) 

## Rearrange columns befor export:
forest_plot <- forest_plot[, c(35, 1:2, 36:37, 6:27, 32:34 ,38)]
forest_subplot <- forest_subplot[, c(34, 1:3, 35:36, 6:27, 32:33)]

dir.create("clean")
write.csv(forest_plot, "clean/forest_data_uppland_plot.csv", row.names = FALSE)
write.csv(forest_subplot, "clean/forest_data_uppland_subplot.csv", 
          row.names = FALSE)

## -----------------------------------END---------------------------------------
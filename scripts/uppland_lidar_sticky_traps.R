## Lidar data for sticky traps 50m and 5m around the traps is calculated.

## First edit: 20200625
## Last edit: 20200625

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

ffmf <- read.csv("data/ffmf.csv")
head(ffmf)

## 3. Create remote sensing data -----------------------------------------------

## Specify directory in which the .asc/.tif files are found and to be stored:
dir_rs <- "N:/Uppland project/Remote sensing data/"
dir(dir_rs)

## Lidar File names
n <- c("PercentAbove0.5m", "PercentAbove5m")

## Load Lidar base files into a raster stack:
base <- stack(paste0(dir_rs, "all_plots_", n, ".asc"))
names(base) <- n

## Calculate understory density below 5m:
ud5 <- base[["PercentAbove0.5m"]] -  base[["PercentAbove5m"]]

## Make coordinates file from ffmf:
out <- ffmf[ffmf$insect_trap %in% c("trap_0", "trap_120", "trap_240"), 
            c("plot", "insect_trap", "north", "east")]

## Extract data:
out <- cbind(out[, 1:2], 
             "ud5_5m_buffer" = extract(ud5, 
                                       out[, c("east", "north")], 
                                       fun = mean, 
                                       buffer = 5),
             "ud5_50m_buffer" = extract(ud5, 
                                        out[, c("east", "north")], 
                                        fun = mean, 
                                        buffer = 50))

## Add "before" because no LiDAR data from after the experiment available yet:
out$experiment <- "before"

dir.create("clean")
write.csv(out, "clean/lidar_data_sticky_trap.csv", row.names = FALSE)

## -----------------------------------END---------------------------------------
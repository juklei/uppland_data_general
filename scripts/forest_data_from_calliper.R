## Forest data from calliper import.

## First edit: 20170901
## Last edit: 20181113


## Author: Julian Klein

## 1. Clear environment and load libraries -------------------------------------

rm(list = ls())

library(data.table)
library(tidyr)
library(plyr)

## 2. Import of trees.csv from Haglöfs device ----------------------------------

trees <- read.csv("L:/DATA/HagFTax/trees.csv", sep = ";")
head(trees)

trees$StandId <- gsub(" ", "", trees$StandId) ## Removes all spaces

trees <- separate(trees, StandId,
                  into = c("measurement_month", "plot"),
                  sep = c(6, -2))[, c(3, 4, 6, 8, 10, 18)]

trees$granskarm <- ifelse(trees$Species == 10, yes = "yes", no = "no")

trees$plot <- as.factor(trees$plot)
trees$Plotname <- as.factor(trees$Plotname)
trees$Species <- as.factor(trees$Species)
levels(trees$plot) <- paste0("plot_", levels(trees$plot))
levels(trees$Plotname) <- c("west", "middle", "east")
trees$Species <- revalue(trees$Species, c("1" = "tall", "2" = "gran", 
                                          "3" = "bjork", "4" = "asp","5" = "ek",
                                          "7" = "lov_ovriga",
                                          "9" = "adellov_ovriga", "10" = "gran",
                                          "11" = "staende_dodved"))

colnames(trees)[3] <- "circle_10m"
colnames(trees)[4] <- "species"
colnames(trees)[5] <- "dbh_cm"
colnames(trees)[6] <- "distance_m"

View(trees)

#unique(trees[,2:3]) ## Check if all plots have three measurments.
                     ## 33 west contains also the measurments from middle

## Write .csv to drive
#write.csv(trees, "data/uppland_forest_year.csv")

## -----------------------------------END---------------------------------------
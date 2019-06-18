## Distance to central point, time since start, block and time since sunrise
## is added to bird obs. One file with distance sampling data and one file
## with occupancy sampling data is the output.
##
## First edit: 20180824
## Last edit: 20181112
##
## Author: Julian Klein

## 1. Clear environment and load libraries -------------------------------------

rm(list = ls())

library(data.table)
library(drc)

## 2. Define or source functions used in this script ---------------------------

## The following function calculates the minutes that have passed from the start
## of a bird survey to the observation. -9999 is returned for not 1 start time
time_calc <- function(x) {
  
  if(sum(!is.na(x$start)) != 1) {
    
    return(-9999)
    
  } else {
    
    return(as.numeric(x$Rts_obs - x$Rts_obs[!is.na(x$start)])/60)
  
  }
    
}

## 3. Load and explore data ----------------------------------------------------

dir("data")
obs <- read.csv("data/bird_obs_2016to18.csv"); head(obs)
coord <- read.csv("data/coordinates_from_master_file.csv"); head(coord)
sunrise <- read.csv("data/sunrise_uppsala.csv"); head(sunrise)

## 4. Calculate and add distance of observation to central point ---------------

## Join obs with central positions:
obs <- merge(obs, coord[coord$circle_10m == "middle", ], by = "plot")

## Calculate distances to central point:
obs$dist <- sqrt((obs$X - obs$east)^2 + (obs$Y - obs$north)^2)

## Check all obs > 50m, correct, rerun script and continue!

dir.create("temp")
# write.csv(obs, "temp/dist_uncorrected.csv")

## 5. Add time after start to all observations ---------------------------------

## Combine date and time to R timestamp:
obs$Rts_obs <- as.POSIXct(strptime(paste(obs$date, obs$time), 
                                   format = "%d/%m/%Y %H:%M:%S"))

## Calculate time until observation since start of survey:

obs <- as.data.table(obs)
obs$obs_year <- as.factor(obs$obs_year)
obs[, minutes_to_obs := time_calc(.SD), by = c("plot", 
                                               "obs_year", 
                                               "method", 
                                               "visit")]

## Check all minutes_to_obs, rerun script and continue!
# write.csv(obs, "temp/minutes_to_obs_uncorrected.csv")

## 6. Add time after sunrise ---------------------------------------------------

sunrise$Rts_sunrise <- as.POSIXct(strptime(paste(sunrise$date,
                                                 sunrise$sunrise_summertime), 
                                           format = "%d/%m/%Y %H:%M:%S"))

obs <- merge(obs, sunrise[, c("date", "Rts_sunrise")], by = "date")

obs$min_post_sunrise <- as.numeric(obs$Rts_obs - obs$Rts_sunrise)/60

## 7. Add days after March 31 --------------------------------------------------

obs$dp_march <- NA
for(i in unique(obs$obs_year)) {
  
  obs$dp_march[obs$obs_year == i] <- 
    floor(as.numeric(obs$Rts_obs[obs$obs_year == i]-
                       as.POSIXct(paste0(i,"-03-31"))))
  
}

## 8. Make a file for distance sampling ----------------------------------------

dist_end <- obs[obs$method == "ds", ]
dist_end$sampling_period <- "second"

dist_start <- obs[obs$method == "tm" & 
                  obs$obs_year != "2016" & 
                  obs$minutes_to_obs <= 5, ]
dist_start$sampling_period <- "first"

dist <- rbind(dist_start, dist_end)

## Chose columns important for analysis and exclude NA's in species for "start:

dir.create("clean")

dist <- as.data.frame(dist)
write.csv(dist[!is.na(dist$species), c("block",
                                       "plot",
                                       "observer",
                                       "visit",
                                       "sampling_period",
                                       "Rts_obs",
                                       "obs_year",
                                       "dp_march",
                                       "min_post_sunrise",
                                       "species",
                                       "behaviour",
                                       "sex",
                                       "height",
                                       "dist",
                                       "minutes_to_obs")], 
          "clean/ds_2017to2018.csv",
          row.names = FALSE)

## 9. Make a file for occupancy modelling based on 5 min and two --------------
##    replicates per visit. The file will be unstructured in its visits

head(obs)

occ_double_end <- obs[obs$method == "ds", ]
occ_double_end$sampling_period <- "second"

occ_double_start <- obs[obs$method == "tm" & 
                    obs$obs_year != "2016" & 
                    obs$minutes_to_obs <= 5, ]
occ_double_start$sampling_period <- "first"

occ_double <- rbind(occ_double_start, occ_double_end)

occ_double <- unique(occ_double[is.na(occ_double$start), ],
                     by = c("plot", 
                            "obs_year", 
                            "visit", 
                            "sampling_period", 
                            "species"))

occ_double <- as.data.frame(occ_double)
write.csv(occ_double[, c("block",
                         "observer",
                         "plot",
                         "visit",
                         "sampling_period",
                         "obs_year",
                         "dp_march",
                         "min_post_sunrise",
                         "species")], 
          "clean/occ_double_2017to2018.csv",
          row.names = FALSE)

## 10. Make a file for occupancy modelling based on the whole obs time ---------

occ <- obs

## Add tm time to obs time of ds:
bolean1 <- occ$obs_year == 2017 & occ$method == "ds"
occ$minutes_to_obs[bolean1] <- occ$minutes_to_obs[bolean1] + 25 
bolean2 <- occ$obs_year == 2018 & occ$method == "ds"
occ$minutes_to_obs[bolean2] <- occ$minutes_to_obs[bolean2] + 10 
# bolean3 <- occ$obs_year == 2019 & occ$method == "ds"
# occ$minutes_to_obs[bolean3] <- occ$minutes_to_obs[bolean3] + 10 

## Add survey time to data set:

occ$obs_time <- as.numeric(occ$obs_time)

occ$obs_time[occ$obs_year == 2016] <- 60
occ$obs_time[occ$obs_year == 2017] <- 30
occ$obs_time[occ$obs_year == 2018] <- 15
#occ$obs_time[occ$obs_year == 2019] <- 15

## Exclude all but first obs:

## Order according to obs_time:
setorder(occ, minutes_to_obs)

## Chose unique obs per year/plot/visit/species
occ <- unique(occ[is.na(occ$start), ], 
              by = c("plot", "obs_year", "visit", "species"))

## Chose columns important for analysis and exclude NA's in species for "start":

dir.create("clean")

occ <- as.data.frame(occ)
write.csv(occ[, c("block",
                  "plot",
                  "observer",
                  "visit",
                  "Rts_obs",
                  "obs_year",
                  "obs_time",
                  "dp_march",
                  "min_post_sunrise",
                  "species",
                  "behaviour",
                  "sex",
                  "height",
                  "dist",
                  "minutes_to_obs")], 
          "clean/occ_2016to2018.csv",
          row.names = FALSE)

## --------------------------END------------------------------------------------

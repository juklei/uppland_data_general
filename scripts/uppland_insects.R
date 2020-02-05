## Arrange all sorts of useable data from the sticky traps
## 
## First edit: 20191017
## Last edit: 20191017
##
## Author: Julian Klein

## 1. Clear environment and load libraries -------------------------------------

rm(list = ls())

library("data.table")
library("tidyr")
library("ggplot2")

## 2. Define or source functions used in this script ---------------------------

## This function copies specific comments between side a and b
comment.exchange <- function(x) {
  if("changed" %in% x) {return("changed")}
  if("on_ground" %in% x) {return("on_ground")}
}

## This function standardises values to between 0 and 1
standardise <- function(x) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

## This funtion calculates the daily increment on the traps
cover.calc <- function(x){
  x <- x[order(x$date), ]
  ## Calculate nr. off days difference:
  for(i in 2:nrow(x)) {
    x[i, "days_diff"] <- as.numeric(x[i, "date"] - x[i-1, "date"])
  }
  ## Calculate cover difference:
  for(i in 2:nrow(x)){
    if(x[i-1, "comment_both"] %in% c("changed", "on_ground")){
      x[i, c("cover_diff", "cd_stand")] <- x[i, c("mean_cover", "mc_stand")] 
    } else{
      x[i, c("cover_diff", "cd_stand")] <- (x[i, c("mean_cover", "mc_stand")] -  
                                        x[(i-1), c("mean_cover", "mc_stand")]) /
                                     (1 - x[(i-1), c("mean_cover", "mc_stand")])
  }}
  return(x) 
}

## 3. Load and explore data ----------------------------------------------------

dir("data")

names <- read.csv("data/uppland_insect_sticky_names_2018.csv")
measure <- read.csv("data/uppland_insect_sticky_measurments_2018.csv")
upp <- read.csv("data/uppland_insect_sticky_up_2018.csv")
dupl <- read.csv("data/uppland_insect_sticky_measurments_duplicates.csv")

head(names)
head(measure)
head(upp)
head(dupl)

## Where are the insect pictures located?
pic_dir <- "N:/Uppland project/Data/2018/Insect traps/Traps"

## 4. Prepare and merge all files ----------------------------------------------

## Add duplicates to measure: The duplicates that do not have a match in names
## for example because they are from another year, will be dropped when merging
## with "names".
measure <- rbind(measure, dupl)

## Remove (red) from all file names in the measurment file:
measure$Slice <- as.character(measure$Slice)
tmp <- strsplit(measure$Slice, split = " ")
measure$Slice <- sapply(tmp,"[[", 1)
rm(tmp)

colnames(measure)[5] <- "cover" ## Change measurment name

## Merge measure and names. All names should remain because pictures in names
## without a match in measure often indicate that a trap was changed.
comb <- merge(names, measure, all.x = TRUE, by.x = "file_name", by.y = "Slice")

## Adjust plot and trap to the usual format:
comb$plot <- paste0("plot_", comb$plot)
comb$trap <- paste0("trap_", comb$trap)

## Adjust date to R format:
comb$date <- as.Date(comb$date, format = "%Y:%m:%d %H:%M:%S")

## Merge comments to one column:
comb$comment <- paste(comb$comment, "/", comb$Comment, "/ Dupl?", comb$duplikat)

## Adjust format in the upp data frame to fit comb:
upp$file_name <- "none"
upp$comment <- NA ## If comment exists in upp, take away !!!
upp$cover <- 0
upp$date <- as.Date(upp$date_upp, format = "%d/%m/%Y")
upp$side <- "a"
tmp <- upp
tmp$side <- "b"
upp <- rbind(upp, tmp)
rm(tmp)

## Add "upp" to "comb" and reduce to needed columns:
col_order <- c("plot", "trap", "side", "date", "cover", "file_name", "comment")
all_comb <- rbind(comb[, col_order], upp[, col_order])

## Indicate duplicated measurments:
all_comb$id <- paste0(all_comb$plot, "_",
                      all_comb$trap, "_",
                      all_comb$side, "_",
                      as.character(all_comb$date))
select <- names(which(table(all_comb$id) > 1))
all_comb$duplicated <- ifelse(all_comb$id %in% select, "yes", "no")
rm(select)

## Remove all duplicated which are NA in cover and rerun code above:
all_comb <- all_comb[!(all_comb$duplicated == "yes" & is.na(all_comb$cover)), ]

## Export the file and clean it up trap by trap:
all_comb <- all_comb[order(all_comb$id), ]
write.csv(all_comb, "temp/insect_2018_cleanup.csv", row.names = FALSE)

## Rename all files with plot_trap_side_date to go through and identify
## When a trap was changed: Do this only for one side.
side_a <- all_comb[all_comb$side == "a" & all_comb$file_name != "none", ]
new_files <- paste0(side_a$id, ".jpg")
dir.create(paste0(pic_dir, "/renamed"))
file.copy(from = paste0(pic_dir, "/", side_a$file_name),
          to = paste0(pic_dir, "/renamed/", new_files))

## 5. Now check for when new traps were raised and add/correct in --------------
##    insect_xxxx_cleaned.csv. Then reimport this file:
ac_corr <- read.csv("temp/insect_2017_cleaned.csv")

## Define duplicates once more:
select <- names(which(table(ac_corr$id) > 1))
ac_corr$duplicated <- ifelse(ac_corr$id %in% select, "yes", "no")
rm(select)

## Make sure the comments for a trap on a certain date are in the comment 
## column for both sides:
ac_corr <- as.data.table(ac_corr)
ac_corr <- ac_corr[, "comment_both":= comment.exchange(comment), 
                   by = c("plot", "trap", "date")]

## Calculate the mean of all duplicates:
ac_corr[, "mean_cover" := mean(cover, na.rm = TRUE), 
        by = c("plot", "trap", "side", "date")]

## What is the size of the residuals among the duplicates:
dupl <- droplevels(ac_corr[ac_corr$duplicated == "yes", ])
dupl$resid <- abs(dupl$mean_cover - dupl$cover)
plot(density(dupl$resid, na.rm = TRUE))

dir.create("results/")
capture.output(summary(dupl$resid)) %>% write("results/insect_dupl_2017.txt")

## 6. Create a function that calculates the difference in % area between a  
##    picture of a trap and the picture of the same trap at the preceeding visit

## Keep only relevant columns, exclude duplicates and reformat date:
acc_red <- unique(ac_corr[, c(1:4, 11, 10)])
acc_red$date <- as.Date(acc_red$date, format = "%d/%m/%Y")

## Make % to rate and standardise the cover variable to the 0-1 range, 
## to correct for the effect of  the person that did the analysis and year 
## effects: 
acc_red$mean_cover <- acc_red$mean_cover/100
acc_red$mc_stand <- standardise(acc_red$mean_cover)

## Calculate the differences and the cumulative sums:
acc_red$days_diff <- 0
acc_red$cover_diff <- 0
acc_red$cd_stand <- 0
accr_incr <- acc_red[, cover.calc(.SD), by = c("plot", "trap", "side")]

## All cover differences that are < 0 must become 0:
accr_incr$cover_diff <- ifelse(accr_incr$cover_diff < 0, 
                               0,
                               accr_incr$cover_diff)
accr_incr$cd_stand <- ifelse(accr_incr$cd_stand < 0, 0, accr_incr$cd_stand)

## Calculate the daily increment:
accr_incr$daily_incr <- accr_incr$cover_diff/accr_incr$days_diff
accr_incr$di_stand <- accr_incr$cd_stand/accr_incr$days_diff
accr_incr$daily_incr <- ifelse(accr_incr$days_diff == 0, 
                               0, 
                               accr_incr$daily_incr) 
accr_incr$di_stand <- ifelse(accr_incr$days_diff == 0, 0, accr_incr$di_stand) 

## Add numeric date:
accr_incr$post_march <- as.numeric(accr_incr$date - as.Date("2017-03-31")) 
accr_incr$pm_mean  <- accr_incr$post_march - accr_incr$days_diff/2

## Calculate the mean daily increment and expand for all preceeding days until
## previous trap picture:
ai_mean <- accr_incr[order(accr_incr$date), 
                     list("mean_incr" = mean(daily_incr, na.rm = TRUE),
                          "mi_stand" = mean(di_stand, na.rm = TRUE),
                          "post_march" = mean(post_march, na.rm = TRUE)), 
                     by = c("plot", "date")]
aim_ext <- ai_mean[, list("post_march" = seq(min(post_march) + 1, 
                                             max(post_march), 
                                             1)),
                   by = "plot"]
aim_ext <- merge(aim_ext, 
                 ai_mean[, c("plot", "post_march", "mean_incr", "mi_stand")], 
                 all.x = TRUE,
                 by = c("plot", "post_march"))
aim_ext <- as.data.table(aim_ext)
aim_ext <- aim_ext[, fill(.SD, c("mean_incr", "mi_stand"), .direction = "up"), 
                   by = "plot"]

## Add year, observer and export:

aim_ext$obs_year <- 2017
aim_ext$obs <- "bkn"

dir.create("clean")
write.csv(na.omit(aim_ext), "clean/insects_2017.csv", row.names = FALSE)

## -------------------------------END-------------------------------------------


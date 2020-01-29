## Arrange all sorts of useable data from the sticky traps
## 
## First edit: 20191017
## Last edit: 20191017
##
## Author: Julian Klein

## 1. Clear environment and load libraries -------------------------------------

rm(list = ls())

library("data.table")

## 2. Define or source functions used in this script ---------------------------

standardise <- function(x) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

## 3. Load and explore data ----------------------------------------------------

dir("data")

names <- read.csv("data/uppland_insect_sticky_names_2017.csv")
measure <- read.csv("data/uppland_insect_sticky_measurments_2017.csv")
upp <- read.csv("data/uppland_insect_sticky_up_2017&bearing.csv")
dupl <- read.csv("data/uppland_insect_sticky_measurments_duplicates.csv")

head(names)
head(measure)
head(upp)
head(dupl)

## Where are the insect pictures located?
pic_dir <- "N:/Uppland project/Data/2017/Archive 2017/Insect traps"

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

## Merge measure and names. All names should remain beacuase pictures in names 
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

## Export the file and clean it up trap by trap:
all_comb <- all_comb[order(all_comb$date), ]
write.csv(all_comb, "temp/insect_2017_cleanup.csv", row.names = FALSE)

## Rename all files with plot_trap_side_date to go through and identify
## When a trap was changed:
old_files <- list.files(pic_dir)
all_comb <- all_comb[match(old_files, all_comb$file_name), ]
new_files <- paste0(all_comb$plot, "_", 
                    all_comb$trap, "_",
                    all_comb$side, "_",
                    print(all_comb$date), 
                    ".jpg")
dir.create(paste0(pic_dir, "/renamed"))
file.copy(from = paste0(pic_dir, "/", old_files),
          to = paste0(pic_dir, "/renamed/", new_files))

## Now check for when new traps were raised and add/correct in 
## insect_xxxx_cleanup.csv. Then reimport this file:

ac_corr <- read.csv("temp/insect_2017_cleanup.csv")

## 5. Create a function that calculates the difference in % area between a  
##    picture of a trap and the picture of the same trap at the preceeding visit

ac_corr <- as.data.table(ac_corr)

## Calculate the mean among all duplicates:
#...

## Standardise the cover variable to the 0-1 range, to correct for the effect of 
## the person that did the analysis and year effects: 
ac_corr$cover <- standardise(ac_corr$cover)

cover.calc <- function(x){
  x <- x[order(x$date), ]
  for(i in 2:nrow(x)){
    x[i, "cover_diff"] <- (x[i, "cover"] - x[(i-1), "cover"]) /
                          (1 - x[(i-1), "cover"])
  }
  return(x)
}

ac_corr$cover_diff <- 0







## 5. ...

1) Merge names and measurments
2) All duplicates (same date) calculate mean
3) On date y (following date x) calculate (y-x)/(100-x) -> If y-x<0 ->  
  -1<if(y-x)<0 replace with 0, if(y-x)<-1 take raw percentage and check whether its true
  4) If on ground or other comment return "do manually"
5) Per plot and visit ideally there are six data points
6) Think of which model to use to calculate AUC and peak. Alternatively conect mean per date and calculate AUC
without a model.

7) To analyse only insect traps and how well they function, Download Lidar data for the traps coordinates and calculate forest 
openness.

## -------------------------------END-------------------------------------------

## Arrange all sorts of useable data from the sticky traps
## 
## First edit: 20191017
## Last edit: 20191017
##
## Author: Julian Klein

## 1. Clear environment and load libraries -------------------------------------

rm(list = ls())

library("data.table")
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

## THis funtion calculates the daily increment on the traps
cover.calc <- function(x){
  x <- x[order(x$date), ]
  ## Calculate nr. off days difference:
  for(i in 2:nrow(x)) {
    x[i, "days_diff"] <- as.numeric(x[i, "date"] - x[i-1, "date"])
  }
  ## Calculate cover difference:
  for(i in 2:nrow(x)){
    if(x[i-1, "comment_both"] %in% c("changed", "on_ground")){
      x[i, "daily_incr"] <- x[i, "mean_cover"] / x[i, "days_diff"]
    } else{
      x[i, "daily_incr"] <- ((x[i, "mean_cover"] - x[(i-1), "mean_cover"]) /
                               (1 - x[(i-1), "mean_cover"])) / x[i, "days_diff"]
    }
  }
  return(x)
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
write.csv(all_comb, "temp/insect_2017_cleanup.csv", row.names = FALSE)

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

## 6. Create a function that calculates the difference in % area between a  
##    picture of a trap and the picture of the same trap at the preceeding visit

## Keep only relevant columns, exclude duplicates and reformat date:
acc_red <- unique(ac_corr[, c(1:4, 11, 10)])
acc_red$date <- as.Date(acc_red$date, format = "%d/%m/%Y")

## Standardise the cover variable to the 0-1 range, to correct for the effect of 
## the person that did the analysis and year effects: 
acc_red$mean_cover <- standardise(acc_red$mean_cover)

## Calculate the daily increment:
acc_red$days_diff <- as.numeric(NA)
acc_red$daily_incr <- 0
accr_incr <- acc_red[, cover.calc(.SD), by = c("plot", "trap", "side")]

## All increments that are < 0 must become 0:
accr_incr$daily_incr <- ifelse(accr_incr$daily_incr < 0,
                               0,
                               accr_incr$daily_incr)

## Why so many (390) minus ????????

## 6. Make different plots to evaluate the data quality ------------------------

accr_incr$post_march <- as.numeric(accr_incr$date - as.Date("2017-03-31"))

dir.create("figures")
pdf("figures/insects_2017.pdf")
for(i in levels(accr_incr$plot)){
  G <- ggplot(droplevels(accr_incr[accr_incr$plot == i, ]), 
              aes(post_march, daily_incr)) +#, color = plot)) +
       geom_point() + 
       geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
       # theme(legend.position = "none")
       ggtitle(print(i)) +
       theme_classic(15)
  plot(G)
}
dev.off()

## Make figure for cumultative cover with daily rate i for time (i-1):i

## ...










## -------------------------------END-------------------------------------------
...

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


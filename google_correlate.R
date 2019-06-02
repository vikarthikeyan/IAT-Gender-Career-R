## Installing required packages
install.packages("fitdistrplus", dependencies = TRUE) 
install.packages("logspline", dependencies = TRUE) 
install.packages("mgcv", dependencies = TRUE) 
install.packages("nlme", dependencies = TRUE)
install.packages("voxel", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("reshape", dependencies = TRUE)
install.packages("ggthemes")
install.packages("xts", dependencies = TRUE)

## Loading required packages
library(fitdistrplus)
library(logspline)
library(mgcv)
library(nlme)
library(ggplot2)
library(plyr)
library(reshape)
library(ggthemes)
library(xts)
source("../config.R")

aggregate_scores <- function(data) {
  # Get logic to aggregate using weeks
  iat_score <- aggregate(D_biep.Male_Career_all ~ day + month + year, data, FUN = mean)
  iat_score$date <- as.Date(paste(as.character(iat_score$year), as.character(iat_score$month),  as.character(iat_score$day), sep = "-"))
  return(iat_score)
}

data_2007_2017_file <- paste(base_path, "dataset/cleaned-2007-2017.csv", sep="")
data = read.csv(data_2007_2017_file, header = TRUE)

aggregated_raw_scores <- aggregate_scores(data)

data <- as.xts(aggregated_raw_scores$D_biep.Male_Career_all,order.by=as.Date(aggregated_raw_scores$date))
weekly <- apply.weekly(data,sum)
weekly <- data.frame(weekly)

outfile <- paste(base_path, "dataset/weekly_2007_2017.csv", sep="")
write.csv(weekly, file = outfile)

weekly$weekly <- -weekly$weekly
outfile <- paste(base_path, "dataset/inv_weekly_2007_2017.csv", sep="")
write.csv(weekly, file = outfile)


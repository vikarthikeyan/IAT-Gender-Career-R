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



ssetwd("Documents/Kenny/IAT-Gender-Career-R/plots")

aggregate_scores <- function(data) {
  # Get logic to aggregate using weeks
  iat_score <- aggregate(D_biep.Male_Career_all ~ day + month + year, data, FUN = mean)
  iat_score$date <- as.Date(paste(as.character(iat_score$year), as.character(iat_score$month),  as.character(iat_score$day), sep = "-"))
  return(iat_score)
}

data = read.csv("/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/cleaned-2007-2017.csv", header = TRUE)

aggregated_raw_scores <- aggregate_scores(data)

data <- as.xts(aggregated_raw_scores$D_biep.Male_Career_all,order.by=as.Date(aggregated_raw_scores$date))
weekly <- apply.weekly(data,sum)
weekly <- data.frame(weekly)

write.csv(weekly, file = "/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/weekly_2007_2017.csv")

weekly$weekly <- -weekly$weekly
write.csv(weekly, file = "/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/inv_weekly_2007_2017.csv")






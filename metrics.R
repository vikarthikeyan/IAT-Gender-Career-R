## Installing required packagesyes
install.packages("foreign", dependencies = TRUE) # for importing SPSS data
install.packages("car", dependencies = TRUE) # for recoding functions
install.packages("chron", dependencies = TRUE) # for changing time information
install.packages("pastecs", dependencies = TRUE) # for basic descriptives
install.packages("dplyr", dependencies = TRUE) # for basic descriptives
install.packages("ggplot2", dependencies = TRUE) # for basic descriptives

## Loading required packages
require(foreign)
require(car)
require(chron)
require(pastecs)
library("dplyr")
require(ggplot2)
library(zoo) 

iat.2007_2018 = read.csv("/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/cleaned-2007-2018.csv", header = TRUE)

#  Convert to date if not already
iat.2007_2018$date <- as.Date(iat.2007_2018$date)

iat.timeseries <- aggregate( D_biep.Male_Career_all ~ month + year , iat.2007_2018 , mean )

iat.timeseries$date <- paste(iat.timeseries$year, iat.timeseries$month, sep="-")
iat.timeseries$date <- paste(iat.timeseries$date, "01", sep="-")
iat.timeseries$date <- as.Date(iat.timeseries$date)

iat.timeseries <- subset(iat.timeseries, select = -c(year, month))

iat.population <- aggregate( age ~ month + year , iat.2007_2018 , mean )
iat.timeseries$age <- iat.population$age

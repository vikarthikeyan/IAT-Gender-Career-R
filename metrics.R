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

# Aggregate D score
iat.timeseries <- aggregate( D_biep.Male_Career_all ~ month + year , iat.2007_2018 , mean )
iat.timeseries$date <- paste(iat.timeseries$year, iat.timeseries$month, sep="-")
iat.timeseries$date <- paste(iat.timeseries$date, "01", sep="-")
iat.timeseries$date <- as.Date(iat.timeseries$date)
iat.timeseries <- subset(iat.timeseries, select = -c(year, month))

# Aggregate age
iat.population <- aggregate( age ~ month + year , iat.2007_2018 , mean )
iat.timeseries$age <- iat.population$age

# Multiple line plot
plot( iat.timeseries$date, iat.timeseries$D_biep.Male_Career_all, type="l", col="red", ylab="" )
par(new=TRUE)
plot( iat.timeseries$date, iat.timeseries$age, type="l", col="green", ylab="IAT score & Average age", replace=TRUE )

# Aggregate sex
levels(iat.2007_2018$sex)[levels(iat.2007_2018$sex)=="." | levels(iat.2007_2018$sex)== " " | levels(iat.2007_2018$sex)== "n"] <- NA

# Male
indices <- (iat.2007_2018$sex=="Male")
indices <- replace(indices, is.na(indices), FALSE)
iat.2007_2018$sex.male[indices] <- iat.2007_2018$sex[indices]
iat.2007_2018$sex.male       <- lapply(iat.2007_2018$sex.male, `/`, 2)

# Female
indices <- (iat.2007_2018$sex=="Female")
indices <- replace(indices, is.na(indices), FALSE)
iat.2007_2018$sex.female[indices] <- iat.2007_2018$sex[indices]

# Total
iat.2007_2018$countcol <- rep(1,nrow(iat.2007_2018)) 

# Get female ratio
iat.population <- aggregate(sex.female ~ month + year, iat.2007_2018, sum)
iat.count <- aggregate(countcol ~ month + year, iat.2007_2018, sum)

iat.population$total <- iat.count$countcol
iat.population$femaleratio <- iat.population$sex.female/iat.population$total

iat.timeseries$femaleratio <- iat.population$femaleratio

# Multiple line plot
plot( iat.timeseries$date, iat.timeseries$D_biep.Male_Career_all, type="l", col="red", ylab="" )
par(new=TRUE)
plot( iat.timeseries$date, iat.timeseries$femaleratio, type="l", col="green", ylab="IAT score & Female Ratio", replace=TRUE )

write.csv(iat.timeseries, file = "/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/monthly-aggregated.csv")




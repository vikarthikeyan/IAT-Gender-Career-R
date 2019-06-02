## Installing required packagesyes
install.packages("foreign", dependencies = TRUE) # for importing SPSS data
install.packages("car", dependencies = TRUE) # for recoding functions
install.packages("chron", dependencies = TRUE) # for changing time information
install.packages("pastecs", dependencies = TRUE) # for basic descriptives
install.packages("dplyr", dependencies = TRUE) # for basic descriptives

## Loading required packages
require(foreign)
require(car)
require(chron)
require(pastecs)
library("dplyr")
source("../config.R")

data_2007_2015 <- paste(base_path, "dataset/2007-2015.csv", sep="")
data_2016 <- paste(base_path, "dataset/2016.csv", sep="")
data_2017 <- paste(base_path, "dataset/2017.csv", sep="")

iat.2007_2015 <- read.csv(data_2007_2015, header = TRUE)

iat.2007_2015 <- iat.2007_2015[c("session_id", "session_status", "date", "year", "month", "day", "age",
             "sex","D_biep.Male_Career_all", "countrycit", 
             "countryres","ethnicityomb","raceomb", "hour", "STATE", "assocareer", "assofamily")]

iat.2016 = read.csv(data_2016, header = TRUE)
iat.2017 = read.csv(data_2017, header = TRUE)

# We have not used 2018 data anywhere in this analysis. Might be used to validate predictions later.
#iat.2018 = read.csv("/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/2018.csv", header = TRUE)

iat.2007_2016 <- bind_rows(iat.2007_2015, iat.2016)
iat.2017 <- within(iat.2017, rm(X))
iat.2007_2017 <- bind_rows(iat.2007_2016, iat.2017)

iat.2007_2017 <- iat.2007_2017[is.na(iat.2007_2017$age)==FALSE,]
iat.2007_2017 <- iat.2007_2017[is.na(iat.2007_2017$sex)==FALSE,]

# Filter samples with age 18-80
iat.2007_2017 <- iat.2007_2017[iat.2007_2017$age > 17, ]
iat.2007_2017 <- iat.2007_2017[iat.2007_2017$age < 81, ]

# State filters
iat.2007_2017 <- iat.2007_2017[is.na(iat.2007_2017$STATE)==FALSE,]

# Remove union territories
iat.2007_2017 <- filter(iat.2007_2017, STATE != "American Samoa")
iat.2007_2017 <- filter(iat.2007_2017, STATE != "Virgin Islands")
iat.2007_2017 <- filter(iat.2007_2017, STATE != "Guam")
iat.2007_2017 <- filter(iat.2007_2017, STATE != "Puerto Rico")
iat.2007_2017 <- filter(iat.2007_2017, !grepl('Armed', STATE))

# Some pre-processing that wasn't done before. TODO: move to preprocessing file
iat.2007_2017 <- iat.2007_2017[is.na(iat.2007_2017$D_biep.Male_Career_all)==FALSE,,]
iat.2007_2017 <- iat.2007_2017[iat.2007_2017$sex!=".",]
iat.2007_2017$sex <- factor(iat.2007_2017$sex)

outfile <- paste(base_path, "dataset/cleaned-2007-2017.csv", sep="")
write.csv(iat.2007_2017, file = outfile)





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

iat.2007_2015 = read.csv("/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/2007-2015.csv", header = TRUE)

iat.2007_2015 <- iat.2007_2015[c("session_id", "session_status", "date", "year", "month", "day", "age",
             "sex","D_biep.Male_Career_all", "countrycit", 
             "countryres","ethnicityomb","raceomb", "hour", "STATE")]

iat.2016 = read.csv("/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/2016.csv", header = TRUE)
iat.2017 = read.csv("/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/2017.csv", header = TRUE)
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


write.csv(iat.2007_2017, file = "/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/cleaned-2007-2017.csv")





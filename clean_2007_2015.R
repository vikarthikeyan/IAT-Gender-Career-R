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

# Read 2005-2015 IAT data
iat.2005.2015 <- read.spss("/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/sav/Gender-Career IAT.public.2005-2015.sav",
                     to.data.frame = TRUE)

# convert date to ISO format
iat.2005.2015$date <- as.chron(ISOdate(1582, 10, 14) + iat.2005.2015$date) 
iat.2005.2015$date <- as.Date(iat.2005.2015$date)

# get entries which has dates between 2007 and 2015
start_date <- as.Date("2007-01-01")  
end_date <- as.Date("2015-12-31") 
iat.2007.2015 <- iat.2005.2015[iat.2005.2015$date >= "2007-01-01" & iat.2005.2015$date <= "2015-12-31",]

# Get only completed entries
iat.2007.2015 <- iat.2007.2015[iat.2007.2015$session_status=="C   ",] 

# Get only US-born US residents
iat.2007.2015 <- iat.2007.2015[iat.2007.2015$countryres=="US",]
iat.2007.2015 <- iat.2007.2015[iat.2007.2015$countrycit=="US",]

# Remove those entries in which important fields are not present - Age, Race, Ethnicity, Gender
iat.2007.2015 <- iat.2007.2015[is.na(iat.2007.2015$age)==FALSE,]
iat.2007.2015 <- iat.2007.2015[is.na(iat.2007.2015$ethnicityomb)==FALSE,]
iat.2007.2015 <- iat.2007.2015[is.na(iat.2007.2015$raceomb)==FALSE,]
iat.2007.2015 <- iat.2007.2015[is.na(iat.2007.2015$sex)==FALSE,]

# Save entries
write.csv(iat.2007.2015, file = "/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/2007-2015.csv")





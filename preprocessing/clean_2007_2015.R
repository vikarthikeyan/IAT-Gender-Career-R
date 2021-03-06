## Installing required packages
install.packages("foreign", dependencies = TRUE) # for importing SPSS data
install.packages("chron", dependencies = TRUE) # for changing time information
install.packages("pastecs", dependencies = TRUE) # for basic descriptives
install.packages("dplyr", dependencies = TRUE) # for basic descriptives

## Loading required packages
require(foreign)
require(chron)
require(pastecs)
library("dplyr")
source("../config.R")

# Read 2005-2015 IAT data
iat.2005.2015 <- read.spss(paste(base_path, "dataset/sav/Gender-Career IAT.public.2005-2015.sav", sep=""),
                     to.data.frame = TRUE)

# convert date to ISO format
iat.2005.2015$date <- as.chron(ISOdate(1582, 10, 14) + iat.2005.2015$date) 
iat.2005.2015$date <- as.Date(iat.2005.2015$date)

# get entries which has dates between 2007 and 2015
start_date <- as.Date("2007-01-01")  
end_date <- as.Date("2015-12-31") 
iat.2005.2015 <- iat.2005.2015[iat.2005.2015$date >= "2007-01-01" & iat.2005.2015$date <= "2015-12-31",]

# Get only completed entries
iat.2005.2015 <- iat.2005.2015[iat.2005.2015$session_status=="C   ",] 

# Get only US-born US residents
iat.2005.2015 <- iat.2005.2015[iat.2005.2015$countryres=="US",]
iat.2005.2015 <- iat.2005.2015[iat.2005.2015$countrycit=="US",]

# Remove those entries in which important fields are not present - Age, Race, Ethnicity, Gender
iat.2005.2015 <- iat.2005.2015[is.na(iat.2005.2015$age)==FALSE,]
iat.2005.2015 <- iat.2005.2015[is.na(iat.2005.2015$ethnicityomb)==FALSE,]
iat.2005.2015 <- iat.2005.2015[is.na(iat.2005.2015$raceomb)==FALSE,]
iat.2005.2015 <- iat.2005.2015[is.na(iat.2005.2015$sex)==FALSE,]

# Get only those entries who submitted their explicit bias
iat.2005.2015 <- iat.2005.2015[is.na(iat.2005.2015$assocareer)==FALSE,]
iat.2005.2015 <- iat.2005.2015[is.na(iat.2005.2015$assofamily)==FALSE,]

# Save entries
outfile <- paste(base_path, "dataset/2007-2015.csv", sep="")
write.csv(iat.2005.2015, file = outfile)

## Installing required packagesyes
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

clean_data <- function(year) {
  
  file_path <- paste(base_path, "dataset/sav/", sep="")
  file_name <- paste(file_path, year, sep="")
  file_name <- paste(file_name, ".sav", sep="")
  
  
  # Read IAT data
  iat <- read.spss(file_name,
                   to.data.frame = TRUE)
  
  ## to optimize, first reduce the number of variables...
  iat <- iat[c("session_id", "session_status", "date", "year", "month", "day" ,"age", "birthyear",
                 "sex", "birthsex", "genderidentity","D_biep.Male_Career_all", "countrycit", 
                 "countryres","ethnicityomb",  "raceomb", "raceomb_002", "hour", "STATE", "assofamily", "assocareer")]
  
  iat.population <- aggregate( birthyear ~ month + year , iat , mean )

  # convert date to ISO format
  iat$date <- as.chron(ISOdate(1582, 10, 14) + iat$date) 
  iat$date <- as.Date(iat$date)
  
  # Get only completed entries
  iat <- iat[iat$session_status=="C   ",] 
  
  # First fuse ambiguous columns, god this is painful
  if("sex" %in% colnames(iat) & "birthsex" %in% colnames(iat)) {
    iat$birthsex <- factor(iat$birthsex, labels = c("Male", "Female"))
    iat$sex <- with(iat, coalesce(sex, factor(birthsex)))
  } else {
    iat$sex <- iat$birthsex
  }
  
  iat <- iat[iat$sex != ".",]
  
  
  # Remove those entries in which important fields are not present - Race, Ethnicity, Gender
  iat <- iat[is.na(iat$ethnicityomb)==FALSE,]
  
  if("raceomb" %in% colnames(iat))
  {
    iat <- iat[is.na(iat$raceomb)==FALSE,]
  }

  iat <- iat[is.na(iat$countryres)==FALSE,]
  iat <- iat[is.na(iat$countrycit)==FALSE,]
  
  # Get those entries only from U.S.A
  levels(iat$countrycit)
  iat$countrycit <- gsub("  ", "", iat$countrycit)
  iat$countrycit <- gsub(" ", "", iat$countrycit)
  iat$countrycit <- factor(iat$countrycit)
  
  levels(iat$countryres)
  iat$countryres <- gsub("  ", "", iat$countryres)
  iat$countryres <- gsub(" ", "", iat$countryres)
  iat$countryres <- factor(iat$countryres)
  
  levels(iat$countryres)
  
  iat$countrycit <- recode(iat$countrycit, '1'= 'US')
  iat$countryres <- recode(iat$countryres, '1'= 'US')
  
  iat <- iat[iat$countrycit=="US",]
  iat <- iat[iat$countryres=="US",]
  
  ## change birthyear to age------------
  
  # Problem with birthyear, they have accidentally added age also, filter this pain out. 
  indices <- (iat$date > as.Date("2016-05-19"))
  indices <- replace(indices, is.na(indices), FALSE)
  
  iat$age.temp[indices] <- iat$birthyear[indices]
  
  indices <- (iat$age.temp > 1800)
  indices <- replace(indices, is.na(indices), FALSE)
  
  iat$age.temp[indices] <- (year - iat$age.temp)[indices]
  
  ## now combine
  iat$age.temp <- paste(iat$age, iat$age.temp)

  iat$age.temp <- gsub("NA ", "", iat$age.temp)
  iat$age.temp <- gsub(" NA", "", iat$age.temp)
  
  iat$age.temp <- as.character(iat$age.temp)
  iat$age.temp <- as.numeric(iat$age.temp)
  
  iat$age <- iat$age.temp
  
  iat <- iat[is.na(iat$assocareer)==FALSE,]
  iat <- iat[is.na(iat$assofamily)==FALSE,]
 
  return(iat)
}

result <- clean_data(2016)

result <- subset(result, select = -c(birthyear, birthsex, genderidentity, age.temp, raceomb_002))

outfile <- paste(base_path, "dataset/2016.csv", sep="")
write.csv(result, file = outfile)






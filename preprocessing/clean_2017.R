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

clean_data <- function(year) {
  year=2017
  file_name <- paste("/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/sav/", year, sep="")
  file_name <- paste(file_name, ".sav", sep="")
  
  # Read IAT data
  iat <- read.spss(file_name,
                   to.data.frame = TRUE)
  
  ## to optimize, first reduce the number of variables...
  iat <- iat[c("session_id", "session_status", "date", "year", "month", "day", "birthyear",
                 "birthsex", "genderidentity","D_biep.Male_Career_all", "countrycit_num",
                 "countryres_num","ethnicityomb","raceomb_002", "hour")]
  
  # convert date to ISO format
  iat$date <- as.chron(ISOdate(1582, 10, 14) + iat$date) 
  iat$date <- as.Date(iat$date)
  
  # Get only completed entries
  iat <- iat[iat$session_status=="C   ",] 

  iat$birthsex <- factor(iat$birthsex, labels = c("Male", "Female"))
  iat$sex <- iat$birthsex
  iat <- iat[is.na(iat$sex)==FALSE,,]
  
  # Remove those entries in which important fields are not present - Race, Ethnicity, Gender
  iat <- iat[is.na(iat$ethnicityomb)==FALSE,]
  iat <- iat[is.na(iat$raceomb_002)==FALSE,]
  
  # 2017 has some issues with countryres_num and countrycit_num
  iat <- iat[is.na(iat$birthyear)==FALSE,]
  
  # Get those entries only from U.S.A
  iat$countrycit <- factor(iat$countrycit_num)
  iat$countrycit <- gsub(" ", "", iat$countrycit)
  
  iat$countryres <- factor(iat$countryres_num)
  iat$countryres <- gsub(" ", "", iat$countryres)
  

  ## change birthyear to age------------

  table(iat$birthyear, iat$year) # this one picks up...
  table(iat$birthyear) # only uses real year values...
  
  iat$age <- year - iat$birthyear
  
  return(iat)
}


result <- clean_data(2017)

colnames(result)[colnames(result)=="raceomb_002"] <- "raceomb"

result <- subset(result, select = -c(birthyear, birthsex, genderidentity, countrycit_num, countryres_num))

write.csv(result, file = "/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/2017.csv")






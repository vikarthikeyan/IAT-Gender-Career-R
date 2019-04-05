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
  file_name <- paste("/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/sav/", year, sep="")
  file_name <- paste(file_name, ".sav", sep="")
  
  
  # Read IAT data
  iat <- read.spss(file_name,
                   to.data.frame = TRUE)
  
  ## to optimize, first reduce the number of variables...
  iat <- iat[c("session_id", "session_status", "date", "year", "month", "day" ,"age", "birthyear",
                 "sex", "birthsex", "genderidentity","D_biep.Male_Career_all", "countrycit", 
                 "countryres","ethnicityomb",  "raceomb", "raceomb_002")]

  # convert date to ISO format
  iat$date <- as.chron(ISOdate(1582, 10, 14) + iat$date) 
  iat$date <- as.Date(iat$date)
  
  # Get only completed entries
  iat <- iat[iat$session_status=="C   ",] 
  
  # First fuse ambiguous columns, god this is painful
  if("sex" %in% colnames(iat) & "birthsex" %in% colnames(iat)) {
    print("BOTH")
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

  table(iat$birthyear, iat$year) # this one picks up...
  table(iat$birthyear) # only uses real year values...
  
  iat$birthyear.new <- 2016 - iat$birthyear
  
  table(iat$birthyear.new)
  
  ## now combine
  iat$age.new <- paste(iat$age, iat$birthyear.new)
  
  table(iat$age.new)
  iat$age.new <- gsub("NA ", "", iat$age.new)
  iat$age.new <- gsub(" NA", "", iat$age.new)
  
  iat$age.new <- factor(iat$age.new)

  iat$age.new <- factor(iat$age.new, levels = c(6:100))
  iat$age.new <- droplevels(iat$age.new)
  
  table(iat$age.new)
  
  iat$age.new <- as.character(iat$age.new)
  iat$age.new <- as.numeric(iat$age.new)
  
  table(iat$age.new[iat$age.new>25], iat$month[iat$age.new>25]) # complete data
  
  iat$age <- iat$age.new
 
  return(iat)
}

result <- clean_data(2016)

result <- subset(result, select = -c(birthyear, birthsex, genderidentity, birthyear.new, age.new, raceomb_002))

write.csv(result, file = "/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/2016.csv")






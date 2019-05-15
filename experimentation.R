## Installing required packages
install.packages("fitdistrplus", dependencies = TRUE) 
install.packages("logspline", dependencies = TRUE) 
install.packages("mgcv", dependencies = TRUE) 
install.packages("nlme", dependencies = TRUE)
install.packages("voxel", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)

## Loading required packages
library(fitdistrplus)
library(logspline)
library(mgcv)
library(nlme)
library(ggplot2)
library(voxel)
library(plyr)

data = read.csv("/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/cleaned-2007-2017.csv", header = TRUE)

data <- data[is.na(data$D_biep.Male_Career_all)==FALSE,,]

data$date <- as.numeric(data$date)

# Convert sex to a numeric value Male=1, Female=2, remove others
data$sex <- gsub("Male", 1, data$sex)
data$sex <- gsub("Female", 2, data$sex)
data$sex <- as.numeric(data$sex)
data <- data[is.na(data$sex)==FALSE,,]

################ Model that compensates for seasonality ################
# Cyclic cubic spline function to accomodate Dec-Jan smooth transition
# 12 knots for 12 months
model_time_age_sex_month <- gam(D_biep.Male_Career_all ~ s(date) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = data)

par(mfrow= c(2,1))
plot(model_time_age_sex_month, scale = 0, shade=TRUE)

################ Model with state-wise filters for all 50 states + DC ####################

model <- gam(D_biep.Male_Career_all ~ STATE + s(date) + s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = data)

# Individual plot
plot(model, shade = TRUE, shade.col = "lightblue", scale = 0)

# state-wise plot
par(mfrow= c(1,1))
state_wise_smooth <- plotGAM(gamFit = model, smooth.cov = "date", groupCovs = "STATE", plotCI=F)

################# Region-wise models based on US census regions ################

northeast_states <- c('Maine', 'New Hampshire', 'Vermont', 'Massachusetts', 'Rhode Island', 'Connecticut', 'New York', 'New Jersey', 'Pennsylvania')
midwest_states <- c('Ohio', 'Michigan', 'Indiana', 'Wisconsin', 'Illinois', 'Minnesota', 'Iowa', 'Missouri', 'North Dakota', 'South Dakota', 'Nebraska', 'Kansas')
south_states <- c('Delaware', 'Maryland', 'Virginia', 'West Virginia', 'Kentucky', 'North Carolina', 'South Carolina', 'Tennessee', 'Georgia', 'Florida', 'Alabama', 'Mississippi', 'Arkansas', 'Louisiana', 'Texas', 'Oklahoma')
west_states <- c('Montana', 'Idaho', 'Wyoming', 'Colorado', 'New Mexico', 'Arizona', 'Utah', 'Nevada', 'California', 'Oregon', 'Washington', 'Alaska', 'Hawaii')

# Remove those samples in those states which have less than 5k samples (nrow: 405424)
filtered_data <- ddply(data, "STATE", function(d) {if(nrow(d)>5000) d else NULL})

# Subset state-wise samples, get list of states after sample thresholding
northeast <- subset(filtered_data, STATE %in% northeast_states)
northeast_states <- names(table(northeast$STATE)[table(northeast$STATE) > 0])

midwest <- subset(filtered_data, STATE %in% midwest_states)
midwest_states <- names(table(midwest$STATE)[table(midwest$STATE) > 0])

south <- subset(filtered_data, STATE %in% south_states)
south_states <- names(table(south$STATE)[table(south$STATE) > 0])

west <- subset(filtered_data, STATE %in% west_states)
west_states <- names(table(west$STATE)[table(west$STATE) > 0])

# Generate models
northeast_model <- gam(D_biep.Male_Career_all ~ STATE + s(date) + s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = northeast)
midwest_model <- gam(D_biep.Male_Career_all ~ STATE + s(date) + s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = midwest)
south_model <- gam(D_biep.Male_Career_all ~ STATE + s(date) + s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = south)
west_model <- gam(D_biep.Male_Career_all ~ STATE + s(date) + s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = west)

# state-wise plot
par(mfrow= c(1,1))
northeast_statewise_gam <- plotGAM(gamFit = northeast_model, smooth.cov = "date", groupCovs = "STATE", plotCI=F)
midwest_statewise_gam <- plotGAM(gamFit = midwest_model, smooth.cov = "date", groupCovs = "STATE", plotCI=F)
south_statewise_gam <- plotGAM(gamFit = south_model, smooth.cov = "date", groupCovs = "STATE", plotCI=F)
west_statewise_gam <- plotGAM(gamFit = west_model, smooth.cov = "date", groupCovs = "STATE", plotCI=F)

# Get the plot values and store separately for the date-range plot (TODO: can we do better?)
get_state_fits <- function(statewise_gam, states){
  rows_per_state <- nrow(statewise_gam$data) / length(states)
  statewise_gam_data <- statewise_gam$data[1:rows_per_state,]

  for(state in states[1:length(states)]) {
    fit_for_state <- statewise_gam$data[statewise_gam$data$STATE == state,]["fit"]
    statewise_gam_data[state] <- fit_for_state
  }
  
  columns_needed <- append(states, "date")
  result <- statewise_gam_data[columns_needed]
  result$date <- as.Date(result$date, origin = "2007-01-01")
  return(result)
}

northeast_statewise_fits <- get_state_fits(northeast_statewise_gam, northeast_states)
midwest_statewise_fits <- get_state_fits(midwest_statewise_gam, midwest_states)
south_statewise_fits <- get_state_fits(south_statewise_gam, south_states)
west_statewise_fits <- get_state_fits(west_statewise_gam, west_states)

ggplot(data=northeast_statewise_fits, aes(x=date, y=Pennsylvania, group=1)) + geom_line()







####### Plot with correct date labels #########
model_time_age_sex_month <- gam(D_biep.Male_Career_all ~ s(date) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = data)

combined_date_plot <- plotGAM(gamFit = model_time_age_sex_month, smooth.cov = "date", groupCovs = NULL, plotCI=F, orderedAsFactor = FALSE)

fitted_dates_scores <- combined_date_plot$data
fitted_dates_scores$date <- as.Date(fitted_dates_scores$date, origin = "2007-01-01")

ggplot(data=fitted_dates_scores, aes(x=date, y=fit, group=1)) + geom_line()



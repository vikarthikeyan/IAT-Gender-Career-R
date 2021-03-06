## Installing required packages
install.packages("fitdistrplus", dependencies = TRUE) 
install.packages("logspline", dependencies = TRUE) 
install.packages("mgcv", dependencies = TRUE) 
install.packages("nlme", dependencies = TRUE)
install.packages("voxel", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("reshape", dependencies = TRUE)
install.packages("ggthemes")

## Loading required packages
library(fitdistrplus)
library(logspline)
library(mgcv)
library(nlme)
library(ggplot2)
library(voxel)
library(plyr)
library(reshape)
library(ggthemes)
source("../config.R")

# Set local path for portability
plots_path <- paste(base_path, "plots", sep="")
setwd(plots_path)

update_dates <- function(gam_plot) {
  result <- gam_plot$data 
  result$date <- as.Date(result$date, origin = "2007-01-01") 
  return(result)
}

aggregate_scores <- function(data) {
  iat_score <- aggregate(explicit_score ~ month + year, data, FUN = mean)
  iat_score$date <- as.Date(paste(as.character(iat_score$year), as.character(iat_score$month), "01", sep = "-"))
  return(iat_score)
}

data_2007_2017_file <- paste(base_path, "dataset/cleaned-2007-2017.csv", sep="")
data = read.csv(data_2007_2017_file, header = TRUE)

# Calculate explicit bias
data$assocareer <- as.numeric(data$assocareer)
data$assofamily <- as.numeric(data$assofamily)

# A quick conversion to change the range to [-2,2]
data$assocareer <- (data$assocareer - 4) * (2/3)
data$assofamily <- (data$assofamily - 4) * (2/3)

# Calculate explicit bias
data$explicit_score <- data$assocareer - data$assofamily
data$explicit_score <- data$explicit_score/2

# perform the raw date-wise aggregated plot
aggregated_raw_scores <- aggregate_scores(data)
aggregated_raw_plot <- ggplot(aggregated_raw_scores, aes(date, explicit_score, group=1)) + geom_line(size=1) + theme_economist() + ggtitle("Overall RAW Gender-Career bias in the US") +xlab("Date") + ylab("Explicit Bias")
ggsave(filename="explicit_bias/rawAggregatedScores.png", plot=aggregated_raw_plot)

# Prepare date for GAM
data$date <- as.numeric(data$date)

################ Model that compensates for seasonality ################
# Cyclic cubic spline function to accomodate Dec-Jan smooth transition
# 12 knots for 12 months
####### Overall plot without state filter #########
model_time_age_sex_month_exp <- gam(explicit_score ~ s(date) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = data)

combined_date_plot_exp <- plotGAM(gamFit = model_time_age_sex_month_exp, smooth.cov = "date", groupCovs = NULL, plotCI=F, orderedAsFactor = FALSE)

combined_date_plot_exp <- update_dates(combined_date_plot_exp)

combined_plot_exp <- ggplot(data=combined_date_plot_exp, aes(x=date, y=fit, group=1)) + geom_line(size=1) + theme_economist() + ggtitle("Overall Gender-Career bias in the US") +xlab("Date") + ylab("Explicit Bias")
ggsave(filename="explicit_bias/OverallNoFilter.png", plot=combined_plot_exp)

# test

combined_plot_exp <- ggplot(data=combined_date_plot_exp, aes(x=date, y=fit, group=1, colour=0)) + geom_line(size=1) + 
  theme_economist() + 
  ggtitle("Overall Gender-Career bias in the US") +xlab("Date") + 
  ylab("Biases") + geom_line(data=combined_date_plot, aes(x=date, y=fit, group=1, colour=3), size=1)


################ Model with state-wise filters for all 50 states + DC ####################

model <- gam(explicit_score ~ STATE + s(date) + s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = data)

# Individual plots for each category/smooth
plot(model, shade = TRUE, shade.col = "lightblue", scale = 0)

# state-wise plot
par(mfrow= c(1,1))
state_wise_smooth <- plotGAM(gamFit = model, smooth.cov = "date", groupCovs = "STATE", plotCI=F)

state_wise_smooth <- update_dates(state_wise_smooth)
all_states_plot <- ggplot(state_wise_smooth, aes(x = date, y = fit, colour = group)) + geom_line(size=1) + theme_economist() + ggtitle("US State-wise Gender-Career bias") +xlab("Date") + ylab("Explicit Bias")
ggsave(filename="explicit_bias/allStates.png", plot=all_states_plot)

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
northeast_model <- gam(explicit_score ~ STATE + s(date) + s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = northeast)
midwest_model <- gam(explicit_score ~ STATE + s(date) + s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = midwest)
south_model <- gam(explicit_score ~ STATE + s(date) + s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = south)
west_model <- gam(explicit_score ~ STATE + s(date) + s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = west)

# state-wise plot
par(mfrow= c(1,1))
northeast_statewise_gam <- plotGAM(gamFit = northeast_model, smooth.cov = "date", groupCovs = "STATE", plotCI=F)
midwest_statewise_gam <- plotGAM(gamFit = midwest_model, smooth.cov = "date", groupCovs = "STATE", plotCI=F)
south_statewise_gam <- plotGAM(gamFit = south_model, smooth.cov = "date", groupCovs = "STATE", plotCI=F)
west_statewise_gam <- plotGAM(gamFit = west_model, smooth.cov = "date", groupCovs = "STATE", plotCI=F)

# Update the date numeric to actual dates
northeast_statewise_gam <- update_dates(northeast_statewise_gam)
midwest_statewise_gam <- update_dates(midwest_statewise_gam)
south_statewise_gam <- update_dates(south_statewise_gam)
west_statewise_gam <- update_dates(west_statewise_gam)

# Plot 
northeast_plot <- ggplot(northeast_statewise_gam, aes(x = date, y = fit, colour = group)) + geom_line(size=1) + theme_economist() + ggtitle("US North-east Region Gender-Career bias") +xlab("Date") + ylab("Explicit Bias")
midwest_plot <- ggplot(midwest_statewise_gam, aes(x = date, y = fit, colour = group)) + geom_line(size=1) + theme_economist() + ggtitle("US Midwest Region Gender-Career bias") +xlab("Date") + ylab("Explicit Bias")
south_plot <- ggplot(south_statewise_gam, aes(x = date, y = fit, colour = group)) + geom_line(size=1) + theme_economist() + ggtitle("US South Region Gender-Career bias") +xlab("Date") + ylab("Explicit Bias")
west_plot <- ggplot(west_statewise_gam, aes(x = date, y = fit, colour = group)) + geom_line(size=1) + theme_economist() + ggtitle("US West Region Gender-Career bias") +xlab("Date") + ylab("Explicit Bias")

ggsave(filename="explicit_bias/northeast.png", plot=northeast_plot)
ggsave(filename="explicit_bias/midwest.png", plot=midwest_plot)
ggsave(filename="explicit_bias/south.png", plot=south_plot)
ggsave(filename="explicit_bias/west.png", plot=west_plot)

####### Gender-wise model ##########

gender_wise_model <- gam(explicit_score ~ s(date, by=sex) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = data)
gender_wise_plot <- plotGAM(gamFit = gender_wise_model, smooth.cov = "date", groupCovs = "sex", plotCI=F, orderedAsFactor = FALSE)

gender_wise_plot <- update_dates(gender_wise_plot)
gender_wise_plot <- ggplot(gender_wise_plot, aes(x = date, y = fit, colour = group)) + geom_line(size=1) + theme_economist() + ggtitle("Gender-wise Gender-Career bias") +xlab("Date") + ylab("Explicit Bias")

ggsave(filename="explicit_bias/genderwise.png", plot=gender_wise_plot)

######

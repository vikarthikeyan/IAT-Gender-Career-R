## Installing required packages
install.packages("fitdistrplus", dependencies = TRUE) 
install.packages("logspline", dependencies = TRUE) 
install.packages("mgcv", dependencies = TRUE) 
install.packages("nlme", dependencies = TRUE)
install.packages("voxel", dependencies = TRUE)
install.packages("remotes")
remotes::install_github("mfasiolo/mgcViz", dependencies = TRUE)

## Loading required packages
library(fitdistrplus)
library(logspline)
library(mgcv)
library(nlme)
library(visreg)
library(mgcViz)
library(ggplot2)
library(voxel)


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

################ Model with state-wise filters ####################

model <- gam(D_biep.Male_Career_all ~ STATE + s(date) + s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = data)

plot(model, shade = TRUE, shade.col = "lightblue", scale = 0)

# state-wise plot
par(mfrow= c(1,1))
state_wise_smooth <- plotGAM(gamFit = model, smooth.cov = "date", groupCovs = "STATE", plotCI=F)

# Get all additive values
pp <- predict(model, type = "terms")
predicted_outputs <- as.data.frame(pp)
predicted_outputs$predicted <- rowSums(predicted_outputs[,-1])
predicted_outputs$actual <- data$D_biep.Male_Career_all
write.csv(predicted_outputs, file = "/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/results/additive_values.csv")


NY <- predicted_outputs[predicted_outputs$`s(date):STATENew York` > 0, ]
NY <- subset(NY, select = c(STATE, sex, `s(date)`, `s(date):STATENew York`, `s(age)`, `s(month)`, predicted, actual))

# Region-wise plot

# West: California, Oregon, Washington, Nevada, Idaho, Utah, Arizona, Montana, Alaska, Hawaii
# 94381 data samples 
west_coast <- subset(data, STATE %in% c('California', 'Oregon', 'Washington', 'Nevada', 'Idaho', 'Utah', 'Arizona', 'Montana', 'Alaska', 'Hawaii'))

# East: Michigan, Indiana, Kentucky, Tennessee, Alabama, Ohio, Georgia, Florida, South Carolina, North Carolina, Virginia, West Virginia, Delaware, 
# Maryland, New Jersey, Pennsylvania, New York, Connecticut, Rhode Island, Massachusetts, Vermont, New Hampshire, Maine
# 232143 data samples
east_coast <- subset(data, STATE %in% c('Michigan', 'Indiana', 'Kentucky', 'Tennessee', 'Alabama', 'Ohio', 'Georgia', 'Florida', 'South Carolina', 'North Carolina', 'Virginia', 'West Virginia', 'Delaware', 'Maryland', 'New Jersey', 'Pennsylvania', 'New York', 'Connecticut', 'Rhode Island', 'Massachusetts', 'Vermont', 'New Hampshire', 'Maine'))

# Central: Wyoming, Colorado, New Mexico, North Dakota, South Dakota, Nebraska, Kansas, Oklahoma, Texas, Minnesota, Iowa, Missouri, Arkansas, Louisiana, Wisconsin, Illinois, Mississippi
# 128173 data samples
central <- subset(data, STATE %in% c('Wyoming', 'Colorado', 'New Mexico', 'North Dakota', 'South Dakota', 'Nebraska', 'Kansas', 'Oklahoma', 'Texas', 'Minnesota', 'Iowa', 'Missouri', 'Arkansas', 'Louisiana', 'Wisconsin', 'Illinois', 'Mississippi'))


# Generate models

west_model <- gam(D_biep.Male_Career_all ~ STATE + s(date) + s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = west_coast)
east_model <- gam(D_biep.Male_Career_all ~ STATE + s(date) + s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = east_coast)
central_model <- gam(D_biep.Male_Career_all ~ STATE + s(date) + s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = central)

# state-wise plot
par(mfrow= c(1,1))
west_state_wise <- plotGAM(gamFit = west_model, smooth.cov = "date", groupCovs = "STATE", plotCI=F)
east_state_wise <- plotGAM(gamFit = east_model, smooth.cov = "date", groupCovs = "STATE", plotCI=F)
central_state_wise <- plotGAM(gamFit = central_model, smooth.cov = "date", groupCovs = "STATE", plotCI=F)


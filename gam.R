## Installing required packages
install.packages("fitdistrplus", dependencies = TRUE) 
install.packages("logspline", dependencies = TRUE) 
install.packages("mgcv", dependencies = TRUE) 
install.packages("nlme", dependencies = TRUE) 

## Loading required packages
library(fitdistrplus)
library(logspline)
library(mgcv)
library(nlme)
library(visreg)

data = read.csv("/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/cleaned-2007-2017.csv", header = TRUE)

data$date <- as.numeric(data$date)

# Fit along with gender
data$sex <- gsub("Male", 1, data$sex)
data$sex <- gsub("Female", 2, data$sex)
data$sex <- as.numeric(data$sex)

####################################

# Fit a gam by smoothing only time
model_time <- gam(D_biep.Male_Career_all ~ s(date), data = data)

layout(matrix(1:2, ncol = 2))
plot(model_time, scale = 0)

# Fit a gam by smoothing time and age
model_time_age <- gam(D_biep.Male_Career_all ~ s(date) + s(age) + s(sex), select=TRUE, method='GCV.Cp', data = data)

layout(matrix(1:2, ncol = 2))
plot(model_time_age, scale = 0, shade=TRUE)

#Check AIC
AIC(model_time_age)

# Experiment with REML
model_time_age_reml <- gam(D_biep.Male_Career_all ~ s(date) + s(age), select=TRUE, method='REML', data = data)

layout(matrix(1:2, ncol = 2))
plot(model_time_age, scale = 0, shade=TRUE)

#Check AIC
AIC(model_time_age_reml)

################# Best model till now###################

data$scaled <- scale(data$D_biep.Male_Career_all)[,]
model_time_age_sex <- gam(D_biep.Male_Career_all ~ s(date) + s(age) + sex, select=TRUE, method='GCV.Cp', data = data)

layout(matrix(1:2, ncol = 2))
plot(model_time_age_sex, scale = 0, shade=TRUE)

################ Model that compensates for seasonality ################
# Cyclic cubic spline function to accomodate Dec-Jan smooth transition
# 12 knots for 12 months
model_time_age_sex_month <- gam(D_biep.Male_Career_all ~ s(date) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = data)

par(mfrow= c(2,1))
plot(model_time_age_sex_month, scale = 0, shade=TRUE)

## Experimentation

model_time_age_sex_month_state <- gam(D_biep.Male_Career_all ~ s(date) + s(age, by=STATE) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = data)

par(mfrow= c(3,1))
plot(model_time_age_sex_month, scale = 0, shade=TRUE)

## Installing required packages
install.packages("fitdistrplus", dependencies = TRUE) 
install.packages("logspline", dependencies = TRUE) 
install.packages("mgcv", dependencies = TRUE) 
install.packages("nlme", dependencies = TRUE)
install.packages("remotes")
remotes::install_github("mfasiolo/mgcViz", dependencies = TRUE)

## Loading required packages
library(fitdistrplus)
library(logspline)
library(mgcv)
library(nlme)
library(visreg)
library(mgcViz)


data = read.csv("/Users/bobbyda/IAT-Gender-Career-R/dataset/cleaned-2007-2017.csv", header = TRUE)

data$date <- as.numeric(data$date)

# Convert sex to a numeric value (Experiment for filtering, not necessarily needed)
data$sex <- gsub("Male", 1, data$sex)
data$sex <- gsub("Female", 2, data$sex)
data$sex <- as.numeric(data$sex)

################ Model that compensates for seasonality ################
# Cyclic cubic spline function to accomodate Dec-Jan smooth transition
# 12 knots for 12 months
model_time_age_sex_month <- gam(D_biep.Male_Career_all ~ s(date) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = data)

par(mfrow= c(2,1))
plot(model_time_age_sex_month, scale = 0, shade=TRUE)

################ Model with state-wise filters ####################

model_time_age_sex_month_state <- gam(D_biep.Male_Career_all ~ s(date, by=STATE) + s(age) + s(month, bs = "cc", k = 12) + sex, select=TRUE, method='GCV.Cp', data = data)


pp <- predict(model_time_age_sex_month_state, type = "terms")
write.csv(pp, file = "additive_values.csv")




#par(mfrow= c(5,2))
#plot(model_time_age_sex_month_state, scale = 0, shade=TRUE)

#model <- getViz(model_time_age_sex_month_state)
#mgcv::plot.gam(model)

#plot( sm(model, 1), type = "l", col="red" )
#points( sm(model, 2), type="l", col="green",add=TRUE )
#print(plot(model), ask = FALSE)

#visreg(model_time_age_sex_month_state)
#visreg(model_time_age_sex_month_state, "date", "STATE", gg=TRUE, collapse=TRUE, overlay=TRUE, ylab="IAT Score")











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

# This analysis was simply done to understand the data distribution we'll be dealing with. 

training_aggregated_data <- paste(base_path, "dataset/aggregated/training.csv", sep="")
training <- read.csv(training_aggregated_data, header = TRUE)

x <- training$D_biep.Male_Career_all

# Understand the data distribution
plotdist(training$D_biep.Male_Career_all, histo = TRUE, demp = TRUE)

# Shows that distribution is close to Weibull, Gamma, lognormal
descdist(x, boot=500)

# Check how the data fits the mentioned distributions
fw <- fitdist(training$D_biep.Male_Career_all, "weibull")
fg <- fitdist(training$D_biep.Male_Career_all, "gamma")
fl <- fitdist(training$D_biep.Male_Career_all, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "gamma", "log normal")
denscomp(list(fw, fg, fl), legendtext = plot.legend)
qqcomp(list(fw, fg, fl), legendtext = plot.legend)
cdfcomp(list(fw, fg, fl), legendtext = plot.legend)
ppcomp(list(fw, fg, fl), legendtext = plot.legend)

# Weibull seems like a good fit as it has the lowest goodness-of-fit characteristics
gofstat(list(fw,fg,fl))

training$date <- as.numeric(training$date)

# Fit a straightforward gam by smoothing only time now
model1 <- gamm(D_biep.Male_Career_all ~ s(date), data = training)

layout(matrix(1:2, ncol = 2))
plot(model1$gam, scale = 0)

# Fit a gam by smoothing both time and age
model2 <- gam(D_biep.Male_Career_all ~ s(date) + s(age), data = training)

layout(matrix(1:2, ncol = 2))
plot(model2$gam, scale = 0)

layout(matrix(1:2, ncol = 2))
acf(resid(mmodel1$lme), lag.max = 36, main = "ACF")
pacf(resid(model1$lme), lag.max = 36, main = "pACF")
layout(1)






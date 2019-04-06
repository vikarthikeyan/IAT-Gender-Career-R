## Installing required packages
install.packages("fitdistrplus", dependencies = TRUE) 
install.packages("logspline", dependencies = TRUE) 

## Loading required packages
library(fitdistrplus)
library(logspline)

training = read.csv("/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/aggregated/training.csv", header = TRUE)

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






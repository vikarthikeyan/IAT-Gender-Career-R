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

data = read.csv("/Users/vikramkarthikeyan/Documents/Kenny/IAT-Gender-Career-R/dataset/cleaned-2007-2017.csv", header = TRUE)

data$date <- as.numeric(data$date)

# Fit a gam by smoothing only time
model_time <- gam(D_biep.Male_Career_all ~ s(date), data = data)

layout(matrix(1:2, ncol = 2))
plot(model_time, scale = 0)

# Fit a gam by smoothing time and age
model_time_age <- gam(D_biep.Male_Career_all ~ s(date) + s(age, k=108), data = data)

layout(matrix(1:2, ncol = 2))
plot(model_time_age, scale = 0)

# Fit both
library(data.table)
datas <- rbindlist(list(data.table(value = data$D_biep.Male_Career_all, data_time = data$date), data.table(value = model_time_age$fitted.values,
                                                                      data_time = data$date)))

datas[, type := c(rep("Real", nrow(data)), rep("Fitted", nrow(data)))]

ggplot(data = datas, aes(data_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "IAT scores",
       title = "Fit from GAM n.1")

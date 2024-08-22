setwd("C:/Users/Chris/Desktop/CS598_BayseanModeling/DataAnalysisReport")
data <- read.csv("marijuanause.csv")
#Data summary
summary(data)
quantile(data$female, c(0.025, 0.975))
quantile(data$use1976, c(0.025, 0.975))
quantile(data$use1977, c(0.025, 0.975))
quantile(data$use1978, c(0.025, 0.975))
quantile(data$use1979, c(0.025, 0.975))
quantile(data$use1980, c(0.025, 0.975))
#Plots for Data Summary
percentage_use <- colMeans(data[, -1]) * 100  # Exclude the 'female' column
female_data <- data[data$female == 1, ]
male_data <- data[data$female == 0, ]
female_percentage_use <- colMeans(female_data[, -1]) * 100
male_percentage_use <- colMeans(male_data[, -1]) * 100
par(mfrow = c(1, 2))
plot(1976:1980, female_percentage_use, type = "o", xlab = "Year", ylab = "Percentage of Marijuana Use amongst female adolescents", main = "Marijuana Use Percentage Over Years (Females)")
lines(1976:1980, female_percentage_use, type = "o", col = "blue")
plot(1976:1980, male_percentage_use, type = "o", xlab = "Year", ylab = "Percentage of Marijuana Use amongst male adolescents", main = "Marijuana Use Percentage Over Years (Males)")
lines(1976:1980, male_percentage_use, type = "o", col = "red")
par(mfrow = c(1, 1))
#Model 1 - Model Setup
centered_female <- data$female - mean(data$female)
years <- 1976:1980
yearscaled = as.vector(scale(years, scale = 2*sd(years)))
response_matrix <- data[, c("use1976", "use1977", "use1978", "use1979", "use1980")]
response_matrix2 <- as.matrix(response_matrix)
d1 <- list(usesMarijuana = response_matrix2,
           female = centered_female,
           yearscaled=yearscaled)
inits1 <- list(list(betafemale=-10, betayear=-10),
               list(betafemale=-10, betayear=10),
               list(betafemale=10, betayear=-10),
               list(betafemale=10, betayear=10))
library(rjags)
#Model 1 - Model Run
m1 <- jags.model("marijuanamodel2.bug", d1, inits1, n.chains=4, n.adapt=1000)
update(m1, 1000)
x1 <- coda.samples(m1, c("betaintercept","betafemale","betayear"), n.iter=2000)
#Model 1 - Check convergence
gelman.diag(x1, autoburnin=FALSE)
autocorr.plot(x1[[1]])
plot(x1, smooth=FALSE)
effectiveSize(x1)
#Model 1 - Results
summary(x1)$statistics
summary(x1)$quantiles
#Model 1 - Calculate probability female indicator exceed 0
betafemale <- as.matrix(x1)[, paste0("betafemale")]
mean(betafemale>0)
dic.samples(m1,100000)
#Model 2 - Model Setup
d2 <- list(usesMarijuana = response_matrix2,
           female = centered_female,
           yearscaled=yearscaled)
inits2 <- list(list(betafemale=-10, betayear=-10, sigmaepsilon = 0.01),
               list(betafemale=-10, betayear=10, sigmaepsilon = 9),
               list(betafemale=10, betayear=-10, sigmaepsilon = 9),
               list(betafemale=10, betayear=10, sigmaepsilon = 0.01))
#Model 2 - Model Run
m2 <- jags.model("marijuanamodel3.bug", d1, inits1, n.chains=4, n.adapt=1000)
update(m2, 1000)
x2 <- coda.samples(m2, c("betaintercept","betafemale","betayear","sigmaepsilon"), n.iter=16000)
#Model 2 - Check convergence
gelman.diag(x2, autoburnin=FALSE)
autocorr.plot(x2[[1]])
plot(x2, smooth=FALSE)
effectiveSize(x2)
#Model 2 - Results
summary(x2)$statistics
summary(x2)$quantiles
dic.samples(m2,100000)




betafemale2 <- as.matrix(x2)[, paste0("betafemale")]
mean(betafemale2>0)
###DAÐILIM TESTI
library(fitdistrplus)
library(logspline)
mydata=read.csv("d.csv")
x <- mydata$MaxIter
descdist(x, discrete = FALSE)
fit.weibull <- fitdist(x, "weibull")
fit.norm <- fitdist(x, "norm")
fit.norm$aic
fit.weibull$aic
##################################
library(mle.tools)
 library(fitdistrplus)
## Normal distribution
pdf <- quote(1 / (sqrt(2 * pi) * sigma) * exp(-0.5 / sigma ^ 2 * (x - mu) ^ 2))
lpdf <- quote(-log(sigma) - 0.5 / sigma ^ 2 * (x - mu) ^ 2)
mydata=read.csv("d.csv")
x <- mydata$Alpha
expected.varcov(density = pdf, logdensity = lpdf, n = length(x), parms = c("mu", "sigma"),
mle = c(mean(x), sd(x)), lower = '-Inf', upper = 'Inf')
####################################
library("rlist")
library("NMOF")
library("gradDescent")
library("Metrics")
## get z-factor Data
mydata=read.csv("albrecht.csv")
dataSet <- mydata
## do variance scaling to dataset
featureScalingResult <- varianceScaling(dataSet)
## split dataset
splitedDataSet <- splitData(featureScalingResult$scaledDataSet)
## built model using GD
model <- SGD(splitedDataSet$dataTrain,alpha = 0.25, maxIter = 14, seed = NULL)
## separate testing data with input only
dataTestInput <- (splitedDataSet$dataTest)[,1:ncol(splitedDataSet$dataTest)-1]
## predict testing data using GD model
prediction <- prediction(model,dataTestInput)
## show result()
prediction
##########RMSE calculation
errorValue <- RMSE(splitedDataSet$dataTest[,ncol(splitedDataSet$dataTest)],
prediction[,ncol(prediction)])
## show result
print(errorValue)


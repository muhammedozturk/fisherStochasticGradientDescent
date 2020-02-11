###DAÐILIM TESTI
library(fitdistrplus)
library(logspline)
mydata=read.csv("kitchenham.csv")
x <- mydata$learningRate
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
x <- mydata$learningRate
expected.varcov(density = pdf, logdensity = lpdf, n = length(x), parms = c("mu", "sigma"),
mle = c(mean(x), sd(x)), lower = '-Inf', upper = 'Inf')
####################################
library("rlist")
require(rnn)
library("NMOF")
library("gradDescent")
library("Metrics")

mydata=read.csv("kitchenham.csv")

train <- 1:60
test <- 61:120
X=as.matrix(mydata)
Y=as.matrix(mydata)
model <- trainr(X = X[train,],
                Y = Y[test,],
                learningrate = 0.24,
                hidden_dim = 14,
                numepochs = 2)
#EKLENEN SONU#
X=X[121:145,]
Yp <- predictr(model, X)
time2 <- Sys.time()
errorValue <- rmse(Yp,X)
print(errorValue)
print(",,,,,")
#######################################3

 x <- rnorm(n = 1000, mean = 5, sd = 2)
 negll <- function(par, x) -sum(dnorm(x = x, mean = par[1], sd = par[2], log = TRUE))
 o1 <- optim(par = c(1, 1), fn = negll, x = x, method = "L-BFGS-B",
 lower = c(-Inf, 0.0001))
o1$par




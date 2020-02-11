library("rlist")
require(rnn)
library("NMOF")
library("gradDescent")
# Transpose
X <- t(X)
Y <- t(Y)
library("Metrics")
# NOT RUN {
testFun <- function(x){
liste <-c(x[1L],x[2L],x[3L])
   return(liste)
}
par1 <- c(0.1,0.2,0.3,0.35,0.4,0.5,0.6,0.7,0.8,0.9)
par2 <- c(5, 10,15,20,25,30,40,50)
par3 <- c(1,2,3,4,5)
sol <- gridSearch(fun = testFun, levels = list(par1, par2,par3))
sol$minfun
sol$minlevels
i <- 1
j <- 2
k <- 3
sonucList <- list("RMSE")
sonucList2 <- list("learningRate")
sonucList3 <- list("EPOCH")
sonucList4 <- list("hiddenDim")
# Training-testing sets
train <- 1:50
test <- 51:100
time1 <- Sys.time()
#EKLENEn#
mydata=read.csv("kitchenham.csv")
while(i<1199){
X=as.matrix(mydata)
Y=as.matrix(mydata)
model <- trainr(X = X[train,],
                Y = Y[test,],
                learningrate = sol$values[i],
                hidden_dim = sol$values[k],
                numepochs = sol$values[j])
#EKLENEN SONU#
X=X[101:145,]
Yp <- predictr(model, X)
time2 <- Sys.time()
errorValue <- rmse(Yp,X)
print(errorValue)
print(sol$values[i])
print(sol$values[j])
print(",,,,,")
sonucList <- list.append(sonucList,errorValue)
sonucList2 <- list.append(sonucList2,sol$values[i])
sonucList3 <- list.append(sonucList3,sol$values[j])
sonucList4 <- list.append(sonucList4,sol$values[k])
i <- i+3
j <- j+3
k <- k+3
}

genel <-   cbind(sonucList,sonucList2,sonucList3,sonucList4)
genel
# Predicted values

write.csv(genel,"d.csv")

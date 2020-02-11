library("rlist")
library("NMOF")
library("gradDescent")
library("Metrics")
# NOT RUN {
testFun <- function(x){
liste <-c(x[1L],x[2L])
   return(liste)
}
par1 <- c(0.1,0.2,0.3,0.35,0.4,0.5,0.6,0.7,0.8,0.9)
par2 <- c(5, 10,15,20,25,30,40,50)

sol <- gridSearch(fun = testFun, levels = list(par1, par2))
sol$minfun
sol$minlevels
i <- 1
j <- 2
k <- 0
sonucList <- list("rmse")
sonucList2 <- list("alpha")
sonucList3 <- list("iterations")
mydata=read.csv("china.csv")
while(i<160){
## get z-factor Data
dataSet <- mydata
## do variance scaling to dataset
featureScalingResult <- varianceScaling(dataSet)
## split dataset
splitedDataSet <- splitData(featureScalingResult$scaledDataSet)
## built model using GD
model <- SGD(splitedDataSet$dataTrain,alpha = sol$values[i], maxIter = sol$values[j], seed = NULL)
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
print(sol$values[i])
print(sol$values[j])
print(",,,,,")
sonucList <- list.append(sonucList,errorValue)
sonucList2 <- list.append(sonucList2,sol$values[i])
sonucList3 <- list.append(sonucList3,sol$values[j])
i <- i+2
j <- j+2
}
genel <- cbind(sonucList,sonucList2,sonucList3)
write.csv(genel,"d.csv")
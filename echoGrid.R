library("rlist")
library("NMOF")
library("Metrics")
# NOT RUN {
testFun <- function(x){
liste <-c(x[1L],x[2L])
   return(liste)
}

par1 <- c(0.1,0.2,0.3,0.35,0.4,0.5,0.6,0.7,0.8,0.9)


sol <- gridSearch(fun = testFun, levels = list(par1, c(1, 1.1, 1.2,1.3,1.4,1.5,1.6,1.7)))
sol$minfun
sol$minlevels

i <- 1
j <- 2
k <- 0
mydata=read.csv("china.csv")
net_u = as.matrix(mydata[1:64,1])
net_Yt = matrix(mydata[1:64,16])

sonucList <- list("x")
while(i<155){
net <- createESN(leaking.rate = 0.1,
								lambda = 1.1,
								n.neurons =5 ,
								wash.out = 10,
								feedback = FALSE,
								regCoef = 1e-8,
								resCon = 1,
								U = net_u,
								Y = net_Yt)

trained_net <- train(net)

net_test=matrix(mydata[64:128,16])
Ypred <- predict(trained_net,
								U = as.matrix(net_test),
								generative = FALSE,
								genNum = 2000)

error <- rmse(Ypred,net_test)
print(error)
print(sol$values[i])
print(",,,,,")
print(sol$values[j])
sonucList <- list.append(sonucList,error,sol$values[i],sol$values[j])
i <- i+2
j <- j+2
}

write.csv(sonucList,"d.csv")


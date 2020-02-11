##############RÖSSLER
 parameters <- c(a = 0.2, b = 0.2, c = 5.7)
state <- c(X = -2,  Y = -10, Z = 0.2)

 Rössler<-function(t, state, parameters) {
 with(as.list(c(state, parameters)),{
 # rate of change
 dX <- -(Y+Z)
 dY <- X+(a*Y)
 dZ <- b+(X-c)*Z
list(c(dX, dY, dZ))
 }) # end with(as.list ...
 }
##################
times <- seq(0, 100, by = 0.01)
library(deSolve)
out <- ode(y = state, times = times, func = Rössler, parms = parameters)
head(out)
write.csv(out,"d.csv")
########################################
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

mydata=read.csv("d.csv")
net_u = as.matrix(mydata[1:3000,2])
net_Yt = matrix(mydata[1:3000,3])

sonucList <- list("RMSE")
sonucList2 <- list("leaking")
sonucList3 <- list("lambda")
while(i<155){
net <- createESN(leaking.rate =sol$values[i] ,
								lambda = sol$values[j],
								n.neurons =5 ,
								wash.out = 10,
								feedback = FALSE,
								regCoef = 1e-8,
								resCon = 1,
								U = net_u,
								Y = net_Yt)

trained_net <- train(net)

net_test=matrix(mydata[9001:9999,3])
Ypred <- predict(trained_net,
								U = as.matrix(net_test),
								generative = FALSE,
								genNum = 2000)

error <- rmse(Ypred,net_test)
print(error)
print(sol$values[i])
print(",,,,,")
print(sol$values[j])
sonucList <- list.append(sonucList,error)
sonucList2 <- list.append(sonucList2,sol$values[i])
sonucList3 <- list.append(sonucList3,sol$values[j])
i <- i+2
j <- j+2
}
i<-1
j<-2
genel <-   cbind(sonucList,sonucList2,sonucList3)
write.csv(genel,"d2.csv")


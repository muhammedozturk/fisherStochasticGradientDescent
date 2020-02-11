# Clear workspace
rm(list=ls())

# Load libraries
require(rnn)

# Set seed for reproducibility purposes
set.seed(10)

# Set frequency
f <- 5
w <- 2*pi*f

# Create sequences
t <- seq(0.005,2,by=0.005)
x <- sin(t*w) + rnorm(200, 0, 0.25)
y <- cos(t*w)

# Samples of 20 time series
X <- matrix(x, nrow = 40)
Y <- matrix(y, nrow = 40)

# Plot noisy waves
plot(as.vector(X), col='blue', type='l', ylab = "X,Y", main = "Noisy waves")
lines(as.vector(Y), col = "red")
legend("topright", c("X", "Y"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))

# Standardize in the interval 0 - 1
X <- (X - min(X)) / (max(X) - min(X))
Y <- (Y - min(Y)) / (max(Y) - min(Y))

# Transpose
X <- t(X)
Y <- t(Y)

# Training-testing sets
train <- 1:100
test <- 1:100
time1 <- Sys.time()
#EKLENEn#
mydata=read.csv("china1.csv")
mydata2=read.csv("china4.csv")
X=as.matrix(mydata)
Y=as.matrix(mydata2)
model <- trainr(X = X[train,],
                Y = Y[test,],
                learningrate = 0.05,
                hidden_dim = 5,
                numepochs = 10)
#EKLENEN SONU#
X=X[201:400,]
Yp <- predictr(model, X)
time2 <- Sys.time()
# Train model. Keep out the last two sequences.
model <- trainr(Y[train,],
                X[train,],
                learningrate = 0.05,
                hidden_dim = 15,
                numepochs = 10)

# Predicted values
write.csv(Yp,"d.csv")


# Plot predicted vs actual. Training set + testing set
plot(as.vector(t(Y)), col = 'red', type = 'l', main = "Actual vs predicted", ylab = "Y,Yp")
lines(as.vector(t(Yp)), type = 'l', col = 'blue')
legend("topright", c("Predicted", "Real"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))

# Plot predicted vs actual. Testing set only.
plot(as.vector(t(Y[test,])), col = 'red', type='l', main = "Actual vs predicted: testing set", ylab = "Y,Yp")
lines(as.vector(t(Yp[test,])), type = 'l', col = 'blue')
legend("topright", c("Predicted", "Real"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
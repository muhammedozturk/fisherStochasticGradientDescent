m<-seq(-100, 100, 5)
c<-seq(-100, 100, 5)
Y2<-sum(y^2)
X2<-sum(x^2)
XY<-sum(x*y)
X<-sum(x)
Y<-sum(y)
loss<-Y2+X2*m^2+c^2*length(y)+2*XY*m+2*Y*c-2*X*m*c
f <- function(m, c) {Y2+X2*m^2+c^2*length(y)+2*XY*m+2*Y*c-2*X*m*c}
z <- outer(m, c, f)

persp(m, c, z, phi = 30, theta = 30,col = "orange",xlab = "m (Slope of the Line)",ylab = "c (Intercept on the Y-axis)",zlab = "Loss Function")


#split data into two subset
en = subset(batteries2, Brand == 'Energizer')
ult = subset(batteries2, Brand == 'Ultracell')

library(splines)
attach(batteries2)
#Energizer model
#create a grid for time 
timelims=range(en$Time) 
Time.grid=seq(from=timelims[1],to=timelims[2])
Time.grid
timelims
#bs function
bs(en$Time, df=6)

attr(bs(en$Time,df=6),"knots")
# build a model by using natural splines
fit=lm(Voltage~bs(Time, knots = c(3,6,8)),data=en)
#prediction

pred=predict(fit,newdata=list(Time=Time.grid),se=T)

#plot
plot(en$Time,en$Voltage,col="gray")
lines(Time.grid,pred$fit,lwd=2)
lines(Time.grid,pred$fit+2*pred$se ,lty="dashed")
lines(Time.grid,pred$fit-2*pred$se ,lty="dashed")

#splines
all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = glm(Voltage~bs(Time, df =i),data=en)
  all.cv[i] = cv.glm(en, lm.fit, K = 10)$delta[2]
}
which.min(all.cv[-c(1, 2)])

plot(en$Time,en$Voltage,col="gray")
fit2=lm(Voltage~bs(Time,df=13),data=en)
pred2=predict(fit2,newdata=list(Time=Time.grid),se=T)
lines(Time.grid,pred2$fit,col="green",lwd=2)
legend("topright",legend=c("df=13"), col=c("green"),lty=1,lwd=2,cex=.8)

#Natural splines
plot(en$Time,en$Voltage,col="gray")
all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = glm(Voltage~ns(Time, df =i),data=en)
  all.cv[i] = cv.glm(en, lm.fit, K = 10)$delta[2]
}
which.min(all.cv[-c(1, 2)])

#degrees of freedom 14
fit3=lm(Voltage~ns(Time,df=14),data=en)
pred3=predict(fit3,newdata=list(Time=Time.grid),se=T)
lines(Time.grid,pred3$fit,col="blue",lwd=2)
legend("topright",legend=c("df=14"), col=c("blue"),lty=1,lwd=2,cex=.8)


#smooth splines
plot(en$Time,en$Voltage,xlab="Time",ylab="Voltage")
abline(lm(en$Voltage ~ en$Time),col="grey")
sp.spline <- smooth.spline(en$Time,en$Voltage,cv=TRUE)
lines(sp.spline)
lines(smooth.spline(en$Time,en$Voltage,spar=0.5),col="blue")
lines(smooth.spline(en$Time,en$Voltage,spar=1),col="green")
lines(smooth.spline(en$Time,en$Voltage,spar=1.1),col="yellow",lty=2)
legend("topright",legend=c("spar=0.5","spar=1","spar=1.1"), col=c("blue","green","yellow"),lty=1,lwd=2,cex=.8)
smooth.spline(en$Time,en$Voltage,spar=1.1)$lambda
#thus we control lambda to 0.001888992

#local regression
plot(en$Time,en$Voltage,xlim=timelims ,cex=.5,col="darkgrey")
title (" Local Regression ")
fit=loess(Voltage~Time,span=.2,data= en)
fit2=loess(Voltage~Time,span=.5,data=en)
lines(Time.grid,predict(fit,data.frame(Time=Time.grid)), col="red",lwd=2)
lines(Time.grid,predict(fit2,data.frame(Time=Time.grid)), col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"), col=c("red","blue"),lty=1,lwd=2,cex=.8)


#unltral model
timelims=range(ult$Time) 
Time.grid=seq(from=timelims[1],to=timelims[2])

#plot
plot(ult$Time,ult$Voltage,col="gray")

#splines
all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = glm(Voltage~bs(Time, df =i),data=ult)
  all.cv[i] = cv.glm(ult, lm.fit, K = 10)$delta[2]
}
which.min(all.cv[-c(1, 2)])

plot(ult$Time,ult$Voltage,col="gray")
fit2=lm(Voltage~bs(Time,df=13),data=ult)
pred2=predict(fit2,newdata=list(Time=Time.grid),se=T)
lines(Time.grid,pred2$fit,col="green",lwd=2)
legend("topright",legend=c("df=13"), col=c("green"),lty=1,lwd=2,cex=.8)

#Natural splines
plot(ult$Time,ult$Voltage,col="gray")
all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = glm(Voltage~ns(Time, df =i),data=ult)
  all.cv[i] = cv.glm(ult, lm.fit, K = 10)$delta[2]
}
which.min(all.cv[-c(1, 2)])

#degrees of freedom 13
fit3=lm(Voltage~ns(Time,df=13),data=ult)
pred3=predict(fit3,newdata=list(Time=Time.grid),se=T)
lines(Time.grid,pred3$fit,col="blue",lwd=2)
legend("topright",legend=c("df=13"), col=c("blue"),lty=1,lwd=2,cex=.8)


#smooth splines
plot(ult$Time,ult$Voltage,xlab="Time",ylab="Voltage")
abline(lm(ult$Voltage ~ ult$Time),col="grey")
sp.spline <- smooth.spline(ult$Time,ult$Voltage,cv=TRUE)
lines(sp.spline)
lines(smooth.spline(ult$Time,ult$Voltage,spar=0.5),col="blue")
lines(smooth.spline(ult$Time,ult$Voltage,spar=0.8),col="red")
lines(smooth.spline(ult$Time,ult$Voltage,spar=1),col="green")
lines(smooth.spline(ult$Time,ult$Voltage,spar=1.1),col="yellow",lty=2)
lines(smooth.spline(ult$Time,ult$Voltage,spar=1.2),col="pink",lty=2)
legend("topright",legend=c("spar=0.5","spar=0.8","spar=1","spar=1.1","spar=1.2"), col=c("blue","red","green","yellow","pink"),lty=1,lwd=2,cex=.8)
smooth.spline(ult$Time,ult$Voltage,spar=1.2)$lambda

#local regression
plot(ult$Time,ult$Voltage,xlim=timelims ,cex=.5,col="darkgrey")
title (" Local Regression ")
fit=loess(Voltage~Time,span=.2,data= ult)
fit2=loess(Voltage~Time,span=.5,data=ult)
lines(Time.grid,predict(fit,data.frame(Time=Time.grid)), col="red",lwd=2)
lines(Time.grid,predict(fit2,data.frame(Time=Time.grid)), col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"), col=c("red","blue"),lty=1,lwd=2,cex=.8)

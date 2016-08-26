install.packages("e1071", dependencies = TRUE)
library(e1071)
#pre-process data split it into x and y
x = as.matrix(mmr_levee[,2:ncol(mmr_levee)])
dat = data.frame(x, y = as.factor(mmr_levee[,1]))
train = sample(1:nrow(mmr_levee), 35)

attach(dat)

#build svm model
svmfit=svm(y~., data=dat[train,], kernel="linear", cost=10,scale=FALSE)
plot(svmfit, dat[train,], V2~V3)

#list all the support vectors
svmfit$index
summary(svmfit)

#perform svm model when cost = 0.1
svmfit=svm(y~., data=dat, kernel="linear", cost=0.1,scale=FALSE)
plot(svmfit, dat[train,], V2~V3)
svmfit$index
summary(svmfit)

#corss validation
set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)

#build test data
test = dat[-train,]
ypred=predict(bestmod,test)
table(predict=ypred, truth=test$y)

#non-linear kernel
svmfit=svm(y∼., data=dat[train ,], kernel ="radial", gamma=1,cost=1)
plot(svmfit , dat[train ,], V2~V3)
summary(svmfit)

#cross-validation
set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
bestmod=tune.out$best.model
summary(bestmod)
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newdata=dat[-train,]))

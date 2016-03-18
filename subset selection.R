#dataset2
lpga.full=regsubsets(V7~.,lpga2009)
summary(lpga.full)
lpga.full=regsubsets(V7~.,data = lpga2009, nvmax = 13)
summary(lpga.full)
lpga.summary = summary(lpga.full)
par(mfrow=c(2,2))
plot(lpga.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(lpga.summary$adjr2 ,xlab="Number of Variables ",ylab="Adjusted RSq",type="l")
which.max(lpga.summary$adjr2)
points(8,lpga.summary$adjr2[8], col="red",cex=2,pch=20)
plot(lpga.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="l")
which.min(lpga.summary$cp )
points(3,lpga.summary$cp [3],col="red",cex=2,pch=20)
which.min(lpga.summary$bic )
plot(lpga.summary$bic ,xlab="Number of Variables ",ylab="BIC",type="l")
points(3,lpga.summary$bic [3],col="red",cex=2,pch=20)
coef(lpga.full, 3)
coef(lpga.full, 8)

#validation set
set.seed (1)
train=sample(c(TRUE,FALSE), nrow(lpga2009),rep=TRUE)
test =(!train )
lpga.full=regsubsets(V7~.,data=lpga2009[train,], nvmax =13)
test.mat=model.matrix(V7~.,data=lpga2009[test,])
val.errors=rep(NA,13)
for(i in 1:13){
  coefi=coef(lpga.full,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((lpga2009$V7[test]-pred)^2) 
}

which.min(val.errors)
coef(lpga.full,3)

#cross validation
predict.regsubsets =function (object ,newdata ,id ,...){
 form=as.formula(object$call [[2]])
mat=model.matrix(form,newdata)
 coefi=coef(object ,id=id)
  xvars=names(coefi)
 mat[,xvars]%*%coefi }
#cross validation
k=10
set.seed (1)
folds=sample(1:k,nrow(lpga2009),replace=TRUE)
cv.errors=matrix(NA,k,13, dimnames=list(NULL, paste(1:13)))

for(j in 1:k){
  lpga.ft=regsubsets (V7~., data = lpga2009[folds!=j,], nvmax = 13)
  for(i in 1:13){
    pred = predict(lpga.ft,lpga2009[folds==j,],id=i)
    cv.errors[j,i] = mean( (lpga2009$V7[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors ,2,mean) 
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors ,type='b')
lpga.best=regsubsets (V7~.,data=lpga2009 , nvmax=13)
which.min(mean.cv.errors)
coef(lpga.best,3)

#forward
lpga.fwd=regsubsets(V7~.,data = lpga2009, nvmax = 13, method="forward")
#summary(lpga.full)
lpga.summary = summary(lpga.fwd)
par(mfrow=c(2,2))
plot(lpga.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(lpga.summary$adjr2 ,xlab="Number of Variables ",ylab="Adjusted RSq",type="l")
which.max(lpga.summary$adjr2)
points(9,lpga.summary$adjr2[9], col="red",cex=2,pch=20)
plot(lpga.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="l")
which.min(lpga.summary$cp )
points(3,lpga.summary$cp [3],col="red",cex=2,pch=20)
which.min(lpga.summary$bic )
plot(lpga.summary$bic ,xlab="Number of Variables ",ylab="BIC",type="l")
points(3,lpga.summary$bic [3],col="red",cex=2,pch=20)
coef(lpga.full, 3)
coef(lpga.full, 9)

#validation set
set.seed (1)
train=sample(c(TRUE,FALSE), nrow(lpga2009),rep=TRUE)
test =(!train )
lpga.fwd=regsubsets(V7~.,data=lpga2009[train,], nvmax =13, method="forward")
test.mat=model.matrix(V7~.,data=lpga2009[test,])
val.errors=rep(NA,13)
for(i in 1:13){
  coefi=coef(lpga.fwd,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((lpga2009$V7[test]-pred)^2) 
}

which.min(val.errors)
coef(lpga.fwd,3)

#cross validation
predict.regsubsets =function (object ,newdata ,id ,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi }
#cross validation
k=10
set.seed (1)
folds=sample(1:k,nrow(lpga2009),replace=TRUE)
cv.errors=matrix(NA,k,13, dimnames=list(NULL, paste(1:13)))

for(j in 1:k){
  lpga.ft=regsubsets (V7~., data = lpga2009[folds!=j,], nvmax = 13,method = "forward")
  for(i in 1:13){
    pred = predict(lpga.ft,lpga2009[folds==j,],id=i)
    cv.errors[j,i] = mean( (lpga2009$V7[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors ,2,mean) 
mean.cv.errors
which.min(mean.cv.errors)
par(mfrow=c(1,1))
plot(mean.cv.errors ,type='b')
lpga.fwd=regsubsets (V7~.,data=lpga2009 , nvmax=13,method = "forward")
coef(lpga.best,11)

#backward
lpga.bwd=regsubsets(V7~.,data = lpga2009, nvmax = 13, method="backward")
#summary(lpga.full)
lpga.summary = summary(lpga.bwd)
par(mfrow=c(2,2))
plot(lpga.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(lpga.summary$adjr2 ,xlab="Number of Variables ",ylab="Adjusted RSq",type="l")
which.max(lpga.summary$adjr2)
points(9,lpga.summary$adjr2[9], col="red",cex=2,pch=20)
plot(lpga.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="l")
which.min(lpga.summary$cp )
points(3,lpga.summary$cp [3],col="red",cex=2,pch=20)
which.min(lpga.summary$bic )
plot(lpga.summary$bic ,xlab="Number of Variables ",ylab="BIC",type="l")
points(3,lpga.summary$bic [3],col="red",cex=2,pch=20)
coef(lpga.full, 3)
coef(lpga.full, 9)

#validation
set.seed (1)
train=sample(c(TRUE,FALSE), nrow(lpga2009),rep=TRUE)
test =(!train )
lpga.bwd=regsubsets(V7~.,data=lpga2009[train,], nvmax =13, method="backward")
test.mat=model.matrix(V7~.,data=lpga2009[test,])
val.errors=rep(NA,13)
for(i in 1:13){
  coefi=coef(lpga.bwd,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((lpga2009$V7[test]-pred)^2) 
}

which.min(val.errors)
coef(lpga.bwd,3)

#cross
#cross validation
k=10
set.seed (1)
folds=sample(1:k,nrow(lpga2009),replace=TRUE)
cv.errors=matrix(NA,k,13, dimnames=list(NULL, paste(1:13)))

for(j in 1:k){
  lpga.ft=regsubsets (V7~., data = lpga2009[folds!=j,], nvmax = 13,method = "backward")
  for(i in 1:13){
    pred = predict(lpga.ft,lpga2009[folds==j,],id=i)
    cv.errors[j,i] = mean( (lpga2009$V7[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors ,2,mean) 
mean.cv.errors
which.min(mean.cv.errors)
par(mfrow=c(1,1))
plot(mean.cv.errors ,type='b')
lpga.bwd=regsubsets (V7~.,data=lpga2009 , nvmax=13,method = "backward")
coef(lpga.best,9)

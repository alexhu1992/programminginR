set.seed(1)
train = sample(392, 196) #392 long from 1 to 192
lm.fit = lm(mpg~horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2) #just use rows in test set, get MSE
lm.fit2 = lm(mpg~poly(horsepower,2), data = Auto, subset = train) #second fit
mean((mpg-predict(lm.fit2,Auto))[-train]^2)  #calculate MSE again
set.seed(2)  #get different training set
train = sample(392,196)
lm.fit = lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)
#we can see from this method got really big variance, the MSE varies from different training set
#we can use glm to do linear regression as well, just leave the family out

#LOOCV method
glm.fit = glm(mpg~horsepower, data = Auto)
coef(glm.fit)
cv.error = cv.glm(Auto,glm.fit)
cv.error$delta   #MSE take all 396 MSE and average it, first one of the delta is the true MSE
cv.error = rep(0,5) #create vector with value 0 and rep 5 times
for(i in 1:5) {
  glm.fit = glm(mpg~poly(horsepower,i), data = Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}
cv.error


#k-fold cross validation, more general approach of k-fold(which LOOCV is k=1)
#reduce the computation a little bit 
set.seed(17)
cv.error=rep(0,10)
for(i in 1:10){
  glm.fit = glm(mpg~poly(horsepower,i), data = Auto)
  cv.error[i] = cv.glm(Auto,glm.fit, K=10)$delta[1]
}

#lda fit
attach(lowbwt)
train = sample(189,100)  #go through all the data 1~189 generate 100 
lda.fit = lda(LOW~. -ID -BWT, data = lowbwt, subset = train)
names(lda.fit)
lda.pred = predict(lda.fit, lowbwt[-train,]) #comma is important
#MSE  LOW[-train]
mean(lda.pred$class != LOW[-train])

#qda
qda.fit = qda(LOW~. -ID -BWT, data = lowbwt, subset = train)
qda.pred = predict(qda.fit, lowbwt[-train,])
mean(qda.pred$class != LOW[-train])

#lda.fit2 cv
lda.fit2 = lda(LOW~. -ID -BWT, data = lowbwt, CV = TRUE)
mean(lda.fit2$class != LOW)

#qda.fi2 loocv
qda.fit2 = qda(LOW~. -ID -BWT, data = lowbwt, CV = TRUE)
mean(qda.fit2$class != LOW)

#knn
train.x = lowbwt[train, -1] #choose from training data eliminate first column
train.x = train.x[,-1]  #eliminate low column
train.x = train.x[,-9]  #eliminate bwt column
#do the same on test
test.x = lowbwt[-train,-1] 
test.x = test.x[,-1]  #eliminate low column
test.x = test.x[,-9]  #eliminate bwt column
head(test.x)
train.y = LOW[train] #response
knn.pred = knn(train.x, test.x, train.y, k =5)
mean(knn.pred != LOW[-train])

#loocv knn
train.x = lowbwt[, -1] #eliminate first column
train.x = train.x[,-1]  #eliminate low column
train.x = train.x[,-9]  #eliminate bwt column
train.y = LOW #response variable
knn.fit = knn.cv(train.x, train.y, k=5)
mean(knn.fit != LOW)

#bootstrap
alpha.fn = function(data, index) {
  X= data$X[index]
  Y = data$Y[index]
  return ((var(Y) - cor(X,Y))/(var(X)+var(Y)-2*cor(X,Y)))
}
alpha.fn(Portfolio, 1:100)
set.seed(1)
alpha.fn(Portfolio, sample(100,100,replace = T))
boot(Portfolio, alpha.fn,R=1000)

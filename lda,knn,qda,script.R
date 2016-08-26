library("ISLR", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("MASS", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
qda.fit = qda(Direction~ Lag1 + Lag2, data = Smarket)
library("class", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
qda.fit = qda(Direction~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class == Direction.2005)
attach(Smarket)
train.X = cbind(Lag1,Lag2)[train,]
test.x = cbind(Lag1,Lag2)[!train,]
dim(test.x)
train.Y = Direction[train]
set.seed(1)   #randomly picks a value
knn.pred = knn(train.X, test.x, train.Y, k = 1)
knn.pred
table(knn.pred, Direction.2005)
dim(Caravan)
attach(Caravan)
test = 1:1000
train.X = Caravan
train.X = Caravan[-test, ]
test.X = Caravan[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
knn.pred = knn(train.X, test.X, train.Y, k=1)
train.X = Caravan[-test,-86 ]
test.X = Caravan[test,-86]
mean(test.Y != knn.pred)
mean(test.Y !="No")
standardized.data = scale(Caravan[,-86])
knn.pred = knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)


#lowbwt dataset
dim(lowbwt)
train = sample(189,100)
#logistic regression
glm.fit = glm(LOW~. -ID -BWT, data = lowbwt, family = binomial, subset = train)
summary(glm.fit)
glm.fit2 = glm(LOW~AGE+LWT+RACE, data = lowbwt, family = binomial, subset = train)
summary(glm.fit2)
glm.probs = predict(glm.fit, lowbwt[-train,], type = "response")
glm.pred = rep(0, 89)
glm.pred[glm.pred>.5] =1
table(glm.pred, LOW[-train])
glm.probs = predict(glm.fit2, lowbwt[-train,], type = "response")
glm.pred = rep(0, 89)
glm.pred[glm.probs>.5] =1
table(glm.pred, LOW[-train])

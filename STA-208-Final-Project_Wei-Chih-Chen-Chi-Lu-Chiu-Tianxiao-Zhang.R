##################################
# STA 208 Final Project          #
# Wei-Chih Chen                  #
# Chi-Lu Chiu                    #
# Tianxiao Zhang                 #
##################################

#### Unsupervised Learning
trainneighbor = neighbor.dat
ind.na = which(is.na(neighbor.dat))
trainneighbor = trainneighbor[ind.na,]
train = data.frame(x=trainneighbor[,-1],y=as.factor(trainneighbor[,1]))
testneighbor = neighbor.dat[-ind.na,]
test = data.frame(x=testneighbor[,-1])

###kMeans### MR = 0.473
kms = kmeans(train, 3, nstart = 10)
table(predict=kms$cluster, truth=trainneighbor[,1])
Mis.kMeans <- 1-mean(kms$cluster==trainneighbor[,1]) ##misclassification rate of kMeans

library(clue)
vector = cl_predict(kms, newdata = test) #kMeans results

##misclassification rate of kMeans = 0.5

###cluster### single clustering best MR = 0.4583
###(a)###
hc.single=hclust(dist(pair.dist),method="single")
cut=cutree(hc.single, 3)
#plot(hc.single,main="Single Linkage",xlab="",sub="",cex=.9)
plot(x, col=cut, pch = (cut + 48))

##misclassification rate of single clustering = 0.45833

###(b)###
hc.complete=hclust(dist(pair.dist),method="complete")
#plot(hc.complete,main="Complete Linkage",xlab="",sub="",cex=.9)
cutb=cutree(hc.complete, 3)
plot(x, col=cutb)

##misclassification rate of complete clustering = 0.54167

###(e)###
hc.average=hclust(dist(pair.dist),method="average")
plot(hc.average,main="Average Linkage",xlab="",sub="",cex=.9)
cut.e1=cutree(hc.average, 3)
plot(x, col=cut.e1)

##misclassification rate of average clustering = 0.54167

hc.average2=hclust(dist(pair.dist)^2,method="average")
plot(hc.average2,main="Average Linkage with square distance",xlab="",sub="",cex=.9)
cut.e2=cutree(hc.average2, 3)
plot(x, col=cut.e2)

##misclassification rate of average clustering with square distance = 0.54167
## reusult is the same as e1

###(f)###
hc.centroid=hclust(dist(pair.dist),method="centroid")
plot(hc.centroid,main="Centroid Linkage",xlab="",sub="",cex=.9)
cut.f1=cutree(hc.centroid, 3)
plot(x, col=cut.f1)

##misclassification rate of centroid clustering = 0.5

hc.centroid2=hclust(dist(pair.dist)^2,method="centroid")
plot(hc.centroid2,main="Centroid Linkage with square distance",xlab="",sub="",cex=.9)
cut.f2=cutree(hc.centroid2, 3)
plot(x, col=cut.f2)

##misclassification rate of centroid clustering with square distance = 0.54167

###EM algorithm ###  MR = 0.418
library(mclust)
mc <- Mclust(train, 3)
table(predict=mc$classification, truth=train$y)
Mis.em <- 1-mean(mc$classification==train$y)
##misclassification rate of EM algorithm = 0.5

###MDS###
loc <- cmdscale(pair.dist)
x <- loc[, 1]
y <- -loc[, 2]
plot(x, y, type = "n", asp=1)
text(x, y, neighbor.dat[,1], cex = 1, col = as.integer(neighbor.dat[,1]+1) )

library(class)
knnpred=knn(pair.dist,x.014.te,train$y,k=1)
table(knnpred,y.014.te)
1-mean(knnpred==y.014.te)


#### Supervised Learning
neighbor.dat = data.frame(neighbor.dat)
na <- which(is.na(neighbor.dat[, 1]))
trainData = neighbor.dat[-na, ]
testData = neighbor.dat[na, ]

trainData.factor = cbind(as.factor(trainData[, 1]), trainData[, -1])
testData.factor = cbind(as.factor(testData[, 1]), testData[, -1])
colnames(trainData.factor)[1] <- "Crime"
colnames(testData.factor)[1] <- "Crime"

### Multinomial Logistic regression
library(nnet)
library(caret)
set.seed(25)
multinom.fit = train(Crime ~., data = trainData.factor, method = "multinom", trControl = trainControl(method = "cv", number = 10))
multinom.fit
multinom.filter = train(Crime ~., data = filteredtrainData, method = "multinom", trControl = trainControl(method = "cv", number = 10))
multinom.filter

### Tree
library(tree)
tree.fit = tree(Crime ~., data = trainData.factor)
set.seed(10)
cv.tree.fit = cv.tree(tree.fit, method = "misclass")
plot(cv.tree.fit$size, cv.tree.fit$dev, type = "b") # Best size is 3
prune.fit = prune.misclass(tree.fit, best = 3)
prune.pred = predict(prune.fit, type = "class")
mean(trainData.factor$Crime != prune.pred)

tree.fit = train(Crime ~., data = trainData.factor, method = "rpart", trControl = trainControl(method = "cv", number = 10))
tree.fit

### Bagging
library(adabag)
library(randomForest)
set.seed(25)
bag.fit = bagging.cv(Crime ~., data = trainData.factor)
bag.fit
bag.fit2 = randomForest(Crime ~., data = trainData.factor, mtry = 92, importance = TRUE)
varImpPlot(bag.fit2, n.var = 10, cex = .7, main = "Bagging")

### Random Forest
set.seed(5)
rf.fit = randomForest(Crime ~., data = trainData.factor, mtry = sqrt(92), do.trace=100, na.action=na.omit, imp = T)
rf.fit
varImpPlot(rf.fit, n.var = 10, cex = .7, main = "Random Forest")

### SVM
library(e1071)
set.seed(25)
tune.out = tune(svm, Crime ~., data = trainData.factor, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)), tunecontrol = tune.control(sampling = "cross", cross = 10, best.model = TRUE))
tune.out
bestmod = tune.out$best.model
bestmod

tune.out2 = tune(svm, Crime ~., data = trainData.factor, kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)), tunecontrol = tune.control(sampling = "cross", cross = 10, best.model = TRUE))
tune.out2
bestmod2 = tune.out2$best.model
bestmod2

####
data.clean = neighbor.dat
View(data.clean)
ind.na = which(is.na(neighbor.dat))
data.na = neighbor.dat[ind.na,]
data.full = neighbor.dat[-ind.na,]

### LDA
lda.fit.scale = lda(scale(data.full[, -1]), data.full[, 1], cv = TRUE)
class.pred.scale = predict(lda.fit.scale, data.full[, -1])$class
class.pred.scale
mis.rate.scale = table(class.pred.scale == data.full[, 1])[1]/length(data.full[, 1])
mis.rate.scale
lda.fit = lda(data.full[, -1], data.full[, 1], cv = TRUE)
class.pred = predict(lda.fit, data.full[, -1])$class
class.pred
mis.rate = table(class.pred == data.full[, 1])[1]/length(data.full[, 1])
mis.rate
class.pred.na = predict(lda.fit, data.na[, -1])$class
class.pred.na

### K-NN
k.nn = function(data, class){
  knn.pred = matrix(, nrow = 54, ncol = 55)
  knn.mis = matrix(, nrow = 54, ncol = 1)
  for(i in 1:54){
    knn.pred[i, ] = knn.cv(data, class, i)
    knn.mis[i] = table(knn.pred[i, ] == class)[1]/length(class)
  }
  min.mis = which(knn.mis == min(knn.mis))
  knn.mis[min.mis[1], ]
}

knn.miss = rep(NA, nrow = 100)
for(i in 1:100){
  knn.miss[i] = k.nn(data.full[, -1], data.full[, 1])
}
mean(knn.miss)

### Boost
boosting = function(tree){
  boost = gbm(factor(Crime)~., distribution = "multinomial", data = data.frame(data.full),
              verbose = F, n.trees = tree)
  # View(summary(boost))
  pred.boost.full = predict(boost, newdata = data.frame(data.full), n.trees = 2500)
  crime.boost.full = rep(NA, nrow(data.full))
  for (i in 1:nrow(data.full)) {
    crime.boost.full[i] = which.max(pred.boost.full[(3*i-2):(3*i)])
  }
  crime.boost.full
  length(which(crime.boost.full != data.full[,1]))/length(crime.boost.full)
}
t = c(100, 200, 500, 1000, 1500, 2000, 2500)

boost.mis = matrix(, nrow = 100, ncol = 7)
for(i in 1:100){
  boost.mis[i, ] = sapply(t, function(t)boosting(t))
}
boost.miss = rep(NA, 7)
for(i in 1:7){
  boost.miss[i] = mean(boost.mis[, i])
}
boost.miss
boost = gbm(factor(Crime)~., distribution = "multinomial", data = data.frame(data.full),
            verbose = F, n.trees = 2000, cv.folds = 10)
summary(boost)

pred.boost.na = predict(boost, newdata = data.frame(data.na), n.trees = 1000)
crime.boost.na = rep(NA, nrow(data.na))
for (i in 1:nrow(data.na)) {
  crime.boost.na[i] = which.max(pred.boost.na[(3*i-2):(3*i)])
}
crime.boost.na


### Shinkage Method
## Ridge
ridge.mis = rep(NA, 100)
for(j in 1:100){
  cv.glm.dat.r = cv.glmnet(data.full[,-1], data.full[,1], alpha = 0)
  lambda.min.r = cv.glm.dat.r$lambda.min
  glm.dat.r = glmnet(data.full[,-1], data.full[,1], family = "multinomial", 
                     lambda = lambda.min.r, alpha = 0)
  pred.glm.full.r = predict(glm.dat.r, newx = data.full[,-1], s = lambda.min.r, 
                            type = "response")
  crime.glm.full.r = rep(NA, nrow(data.full))
  for ( i in 1:nrow(data.full)){
    crime.glm.full.r[i] = which.max(pred.glm.full.r[i,,1])
  }
  ridge.mis[j] = length(which(crime.glm.full.r != data.full[,1]))/length(data.full[, 1])
}
summary(ridge.mis)
sd(ridge.mis)
# coef
coef.r = predict(glm.dat.r, newx = data.full[,-1], 
                 s = lambda.min.r, type = "nonzero")
coef.r

## Lasso
ela.mis = rep(NA, 100)
for(j in 1:100){
  cv.glm.dat.l = cv.glmnet(data.full[,-1], data.full[,1])
  lambda.min.l = cv.glm.dat.l$lambda.min
  glm.dat.l = glmnet(data.full[,-1], data.full[,1], family = "multinomial", 
                     lambda = lambda.min.l)
  pred.glm.full.l = predict(glm.dat.l, newx = data.full[,-1], s = lambda.min.l, 
                            type = "response")
  crime.glm.full.l = rep(NA, nrow(data.full))
  for ( i in 1:nrow(data.full)){
    crime.glm.full.l[i] = which.max(pred.glm.full.l[i,,1])
  }
  ela.mis[j] = length(which(crime.glm.full.l != data.full[,1]))/length(data.full[, 1])
}
summary(ela.mis)
sd(ela.mis)

ela.mis = rep(NA, 100)
crime.full.pred = matrix(, nrow = 100, ncol = nrow(data.full))
crime.na.pred = matrix(, nrow = 100, ncol = nrow(data.na))
lambda = rep(NA, 100)
coef.all = vector("list", 100)
for(j in 1:100){
  cv.glm.dat.l = cv.glmnet(data.full[,-1], data.full[,1])
  lambda.min.l = cv.glm.dat.l$lambda.min
  lambda[j] = lambda.min.l
  glm.dat.l = glmnet(data.full[,-1], data.full[,1], family = "multinomial", 
                     lambda = lambda.min.l)
  coef.all[[j]] = coef(glm.dat.l, s = lambda.min.l)
  pred.glm.full.l = predict(glm.dat.l, newx = data.full[,-1], s = lambda.min.l, 
                            type = "response")
  crime.glm.full.l = rep(NA, nrow(data.full))
  for ( i in 1:nrow(data.full)){
    crime.glm.full.l[i] = which.max(pred.glm.full.l[i,,1])
  }
  pred.glm.na.l = predict(glm.dat.l, newx = data.na[,-1], 
                          s = lambda.min.l, type = "response")
  crime.full.pred[j, ] = crime.glm.full.l
  ela.mis[j] = length(which(crime.glm.full.l != data.full[,1]))/length(data.full[, 1])
  crime.glm.na.l = rep(NA, nrow(data.na))
  for (i in 1:nrow(data.na)) {
    crime.glm.na.l[i] = which.max(pred.glm.na.l[i,,1])
  }
  crime.na.pred[j, ] = crime.glm.na.l
}
View(crime.na.pred)
crime.full.pred[no.mis, ]
table(ela.mis)
no.mis = which(ela.mis == 0)
sam = sample(no.mis, 1)
table(lambda[no.mis])
lambda[sam]
crime.na.pred[sam, ]

coef.all[[sam]][1]
coef.all[[sam]][2]
coef.all[[99]][3]
coef.all[[no.mis[1]]][1]

# coef
coef.l = predict(glm.dat.l, newx = data.full[,-1], 
                 s = lambda.min.l, type = "nonzero")
unlist(coef.l[1])
coef(glm.dat.l)



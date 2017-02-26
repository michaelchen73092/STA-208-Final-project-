##################################
# STA 208 HW5                    #
# Wei-Chih Chen ID:912448776     #
#                                #
##################################
###data set###
# correct
# train
# test
#############
load("/Users/cmichael/Desktop/208HW/Final Project/neighbor.rdata")
correct <- c(1, 2, 2, 3, 1, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 1, 3, 1, 2, 3, 1, 1, 3)
trainneighbor =neighbor.dat
trainneighbor = trainneighbor[-c(1, 8, 13, 17, 18, 20, 23, 30, 31, 42, 46, 47, 49, 52, 53, 54, 55, 57, 60, 69, 71, 72, 76, 79),]
train = data.frame(x=trainneighbor[,-1],y=as.factor(trainneighbor[,1]))
testneighbor = neighbor.dat[-c(2,3,4,5,6,7,9,10,11,12,14,15,16,19,21,22,24,25,26,27,28,29,32,33,34,35,36,37,38,39,40,41,43,44,45,48,50,51,56,58,59,61,62,63,64,65,66,67,68,70,73,74,75,77,78),]
test = data.frame(x=testneighbor[,-1])
completedata = neighbor.dat
completedata[1,1] = 1
completedata[8,1] = 2
completedata[13,1] = 2
completedata[17,1] = 3
completedata[18,1] = 1
completedata[20,1] = 2
completedata[23,1] = 1
completedata[30,1] = 1
completedata[31,1] = 1
completedata[42,1] = 2
completedata[46,1] = 2
completedata[47,1] = 1
completedata[49,1] = 1
completedata[52,1] = 1
completedata[53,1] = 1
completedata[54,1] = 2
completedata[55,1] = 1
completedata[57,1] = 3
completedata[60,1] = 1
completedata[69,1] = 2
completedata[71,1] = 3
completedata[72,1] = 1
completedata[76,1] = 1
completedata[79,1] = 3
complete = data.frame(x=completedata[,-1],y=as.factor(completedata[,1]))
##################################


###kMeans### MR = 0.473
kms = kmeans(train, 3, nstart = 10)
table(predict=kms$cluster, truth=trainneighbor[,1])
Mis.kMeans <- 1-mean(kms$cluster==trainneighbor[,1]) ##misclassification rate of kMeans

library(clue)
vector = cl_predict(kms, newdata = test) #kMeans results
table(predict=vector, truth=correct)
Mis.kMeans <- 1-mean(vector==correct)
##misclassification rate of kMeans = 0.5

###cluster### single clustering best MR = 0.4583
###(a)###
hc.single=hclust(dist(pair.dist),method="single")
cut=cutree(hc.single, 3)
#plot(hc.single,main="Single Linkage",xlab="",sub="",cex=.9)
plot(x, col=cut, pch = (cut + 48))
vector = cut[-c(2,3,4,5,6,7,9,10,11,12,14,15,16,19,21,22,24,25,26,27,28,29,32,33,34,35,36,37,38,39,40,41,43,44,45,48,50,51,56,58,59,61,62,63,64,65,66,67,68,70,73,74,75,77,78)]
table(predict=vector, truth=correct)
Mis.hcsingle <- 1-mean(vector==correct)
##misclassification rate of single clustering = 0.45833

###(b)###
hc.complete=hclust(dist(pair.dist),method="complete")
#plot(hc.complete,main="Complete Linkage",xlab="",sub="",cex=.9)
cutb=cutree(hc.complete, 3)
plot(x, col=cutb)
vector = cutb[-c(2,3,4,5,6,7,9,10,11,12,14,15,16,19,21,22,24,25,26,27,28,29,32,33,34,35,36,37,38,39,40,41,43,44,45,48,50,51,56,58,59,61,62,63,64,65,66,67,68,70,73,74,75,77,78)]
table(predict=vector, truth=correct)
Mis.hccomplete <- 1-mean(vector==correct)
##misclassification rate of complete clustering = 0.54167

###(e)###
hc.average=hclust(dist(pair.dist),method="average")
plot(hc.average,main="Average Linkage",xlab="",sub="",cex=.9)
cut.e1=cutree(hc.average, 3)
plot(x, col=cut.e1)
vector = cut.e1[-c(2,3,4,5,6,7,9,10,11,12,14,15,16,19,21,22,24,25,26,27,28,29,32,33,34,35,36,37,38,39,40,41,43,44,45,48,50,51,56,58,59,61,62,63,64,65,66,67,68,70,73,74,75,77,78)]
table(predict=vector, truth=correct)
Mis.hcaveragee1 <- 1-mean(vector==correct)
##misclassification rate of average clustering = 0.54167

hc.average2=hclust(dist(pair.dist)^2,method="average")
plot(hc.average2,main="Average Linkage with square distance",xlab="",sub="",cex=.9)
cut.e2=cutree(hc.average2, 3)
plot(x, col=cut.e2)
vector = cut.e2[-c(2,3,4,5,6,7,9,10,11,12,14,15,16,19,21,22,24,25,26,27,28,29,32,33,34,35,36,37,38,39,40,41,43,44,45,48,50,51,56,58,59,61,62,63,64,65,66,67,68,70,73,74,75,77,78)]
table(predict=vector, truth=correct)
Mis.hcaveragee2 <- 1-mean(vector==correct)
##misclassification rate of average clustering with square distance = 0.54167
## reusult is the same as e1

###(f)###
hc.centroid=hclust(dist(pair.dist),method="centroid")
plot(hc.centroid,main="Centroid Linkage",xlab="",sub="",cex=.9)
cut.f1=cutree(hc.centroid, 3)
plot(x, col=cut.f1)
vector = cut.f1[-c(2,3,4,5,6,7,9,10,11,12,14,15,16,19,21,22,24,25,26,27,28,29,32,33,34,35,36,37,38,39,40,41,43,44,45,48,50,51,56,58,59,61,62,63,64,65,66,67,68,70,73,74,75,77,78)]
table(predict=vector, truth=correct)
Mis.hccentroidf1 <- 1-mean(vector==correct)
##misclassification rate of centroid clustering = 0.5

hc.centroid2=hclust(dist(pair.dist)^2,method="centroid")
plot(hc.centroid2,main="Centroid Linkage with square distance",xlab="",sub="",cex=.9)
cut.f2=cutree(hc.centroid2, 3)
plot(x, col=cut.f2)
vector = cut.f2[-c(2,3,4,5,6,7,9,10,11,12,14,15,16,19,21,22,24,25,26,27,28,29,32,33,34,35,36,37,38,39,40,41,43,44,45,48,50,51,56,58,59,61,62,63,64,65,66,67,68,70,73,74,75,77,78)]
table(predict=vector, truth=correct)
Mis.hccentroidf2 <- 1-mean(vector==correct)
##misclassification rate of centroid clustering with square distance = 0.54167

###EM algorithm ###  MR = 0.418
library(mclust)
mc <- Mclust(train, 3)
#vector = mc$classification[-c(2,3,4,5,6,7,9,10,11,12,14,15,16,19,21,22,24,25,26,27,28,29,32,33,34,35,36,37,38,39,40,41,43,44,45,48,50,51,56,58,59,61,62,63,64,65,66,67,68,70,73,74,75,77,78)]
table(predict=mc$classification, truth=train$y)
Mis.em <- 1-mean(mc$classification==train$y)
##misclassification rate of EM algorithm = 0.5

###MDS###
loc <- cmdscale(pair.dist)
x <- loc[, 1]
y <- -loc[, 2]
plot(x, y, type = "n", asp=1)
text(x, y, neighbor.dat[,1], cex = 0.6, col = as.integer(neighbor.dat[,1]+1) )

library(class)
knnpred=knn(pair.dist,x.014.te,train$y,k=1)
table(knnpred,y.014.te)
1-mean(knnpred==y.014.te) 


###Logistic regression###
library(glmnet)
set.seed(25)
grid = 10 ^ seq(10, -2, length=100)
ridge.fit = cv.glmnet(as.matrix(train[, -1]), train[, 1], alpha = 0, lambda=grid) 
ridge.bestlam = ridge.fit$lambda.min
ridge.predict = predict(ridge.fit, as.matrix(test[, -1]), s = ridge.bestlam) 
mean_err_ridge = mean((test$Apps-ridge.predict)^2)


# Clustering

?iris
str(iris)


myiris <- iris[,-5]
str(myiris)
set.seed(1234)
km1 <- kmeans(myiris, 3)
attributes(km1)
km1$size
km1$cluster
km1$centers
table(iris$Species, km1$cluster)

plot(myiris[, c(3,4)], col = km1$cluster)
points(km1$centers[, c(3,4)], col = 4:6,  pch = 8, cex = 2)

install.packages('fpc')
library(fpc)
plotcluster(myiris, km1$cluster)
attributes(km1)
?kmeans
head(myiris)
km1$withinss
myiris
myiris$classification <- km1$cluster
myiris[myiris$classification==1,]$classification <- 'setosa' 

no.setosa <- myiris[myiris$classification!='setosa',]

no.setosa.new <- no.setosa[,-5] 

no.setosa.new 

wss <- vector()
set.seed(1234)
for (i in 2:10)
   wss[i] <- sum(kmeans(no.setosa.new, centers=i)$withinss)

wss[-1]

plot(2:10, wss[-1], type= "b", xlab="# clusters", ylab="Sum squares")

?kmeans

km.model <- kmeans(no.setosa.new,5)

plot(no.setosa.new[, c(3,4)], col = km.model$cluster)
points( km.model$centers[, c(3,4)], col = 4:6,  pch = 8, cex = 2)


# B

set.seed(1234)
mySample <- sample(1:nrow(iris), 40)
mySample
sampleData <- iris[mySample,]
sampleData
sampleData$Species <- NULL

distances <- dist(sampleDat)


# C Detectando Outliers

myiris <- iris
myiris$Species <- NULL
set.seed(1234)
km1 <- kmeans(myiris, 3)
attributes(km1)
km1$cluster
centers <- km1$centers[km1$cluster,]
centers
mtxcenters <- matrix(centers, nrow=150, ncol=4)
mtxcenters
DistCentroids <- sqrt(rowSums(myiris - mtxcenters)^2)
DistCentroids


outliers <- myiris[tail(order(DistCentroids), n=10),]
iris
names(myiris)
plot(myiris[,c(1,2)], col=km1$cluster)
points(km1$centers[, c(1,2)], col=1:3, pch=19, cex=2)
points(outliers[, c(1,2)], col=6, pch=13, cex=2)





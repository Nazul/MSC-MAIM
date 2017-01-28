
iris
names(iris)
str(iris)
summary(iris)
??iris


set.seed(1234)
particion  <- sample(2,nrow(iris),replace=T, prob=c(.8,.2))
particion
particion == 1
iris[particion==1,]
trainingset  <- iris[particion==1,]
testset  <- iris[particion==2,]
nrow(trainingset)

nrow(trainingset) + nrow(testset)

install.packages('party')
library(party)
summary(iris)
names(iris)
colnames(iris)
formula  <- Species ~ .
cart1  <- ctree(formula, data=trainingset)

plot(cart1)

setwd(//)



png("cart2.png", width = 1200, height = 800)

summary(cart1)
names(cart1)
cart1
names(iris)

table(predict(cart1, newdata=testset), testset$Species)


nuevoespecimen  <- data.frame(Sepal.Length=100, Sepal.Width=1000, Petal.Length=1.9, Petal.Width=10.3)
nuevoespecimen

predict(cart1, newdata=nuevoespecimen)

cart1
rm(cart1)
rm(x,y)

save(cart1, file = "cartiris.R")
save(cart1, x, y, file = "myvar.R")

x = 1
y = 2

x
y

mvar??

load("cartiris.R")
load("myvar.R")

## bodyfat

??bodyfat
data("bodyfat", package='TH.data')

str(bodyfat)
summary(bodyfat)

theirformula  <- DEXfat ~ hipcirc + anthro3a + kneebreadth
ourformula  <- DEXfat ~ . 

set.seed(1234)
particion  <- sample(2,nrow(bodyfat),replace=T, prob=c(.5,.5))

particion == 1

trainingset  <- bodyfat[particion==1,]
testset  <- bodyfat[particion==2,]

nrow(trainingset) + nrow(testset)

install.packages('rpart')
library(rpart)


str(trainingset)
cart2  <- rpart(formula=theirformula, data=trainingset, method='anova')
plot(cart2)
text(cart2, user=T, cox=.2)

cart2$cptable

attributes(cart2)
cart2$cptable[which.min(cart2$cptable[,"xerror"]),"CP"]

prunecp  <- cart2$cptable[5,"CP"]


cart3  <- prune(cart2,cp=prunecp) 

plot(cart3)
text(cart3, user=T, cox=.2)

names(bodyfat)

mypred  <- predict(cart2, newdata = testset)
mypred
plot(mypred~DEXfat, data=testset, xlab="observed", ylab="predicted")
abline(0,1)

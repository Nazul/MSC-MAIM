# healthcare

setwd("/home/cursos/Descargas")
getwd()

# Data Integration
Oxymetry <- read.csv("Oximetry.csv", stringsAsFactors=F)
str(Oxymetry)
# Cast to date
Oxymetry$Date
#Oxymetry$Date <- as.POSIXct(Oxymetry$Date, "%m/%d/%Y %H:%M")
#Oxymetry$Date <- as.Date(Oxymetry$Date, "%m/%d/%Y %H:%M")
#Oxymetry$Date
?strptime
Oxymetry$Date <- strptime(Oxymetry$Date, "%m/%d/%Y %H:%M")
head(Oxymetry)
Oxymetry <- Oxymetry[,c(1,2,4)]
head(Oxymetry)
Oxymetry$Date

# Data Integration
Oxymetry <- read.csv("Oximetry.csv", stringsAsFactors=F)

datos <- c("carlos", "pedro", "juan")
typeof(datos)
datos <- as.factor(datos)
datos
as.numeric(datos)

Glucose <- read.csv("Glucose.csv", stringsAsFactors=F)
str(Glucose)
Glucose$Date <- strptime(Glucose$Date, "%m/%d/%Y %H:%M")

BloodPressure <- read.csv("bloodPressure.csv", stringsAsFactors=F)
str(BloodPressure)
BloodPressure$Date <- strptime(BloodPressure$Date, "%m/%d/%Y %H:%M")

WeightHeight <- read.csv("Weight_Height.csv", stringsAsFactors=F)
str(WeightHeight)
WeightHeight$Date <- strptime(WeightHeight$Date, "%m/%d/%Y %H:%M")


# Most recent record
Oxymetry
Oxymetry <- Oxymetry[order(Oxymetry$Date, decreasing=F),]
nrow(Oxymetry)
head(Oxymetry)
Oxymetry <- Oxymetry[!duplicated(Oxymetry$Patient, fromLast=T),]

Oxymetry
Glucose <- Glucose[order(Glucose$Date, decreasing=F),]
nrow(Glucose)
head(Glucose)
Glucose <- Glucose[!duplicated(Glucose$Patient, fromLast=T),]

BloodPressure <- BloodPressure[order(BloodPressure$Date, decreasing=F),]
nrow(BloodPressure)
head(Glucose)
BloodPressure <- BloodPressure[!duplicated(BloodPressure$Patient, fromLast=T),]

WeightHeight <- WeightHeight[order(WeightHeight$Date, decreasing=F),]
nrow(WeightHeight)
head(Glucose)
WeightHeight <- WeightHeight[!duplicated(WeightHeight$Patient, fromLast=T),]


# Merge
oxymetry.glucose <- merge(Oxymetry, Glucose, by="Patient")
nrow(oxymetry.glucose)
str(oxymetry.glucose)
oxymetry.glucose.bpre <- merge(oxymetry.glucose, BloodPressure, by="Patient")
nrow(oxymetry.glucose.bpre)
str(oxymetry.glucose.bpre)
all.data <- merge(oxymetry.glucose.bpre, WeightHeight, by="Patient")
str(all.data)

all.data.clean <- all.data[,-c(4,6,11,15)]
str(all.data.clean)
all.data.clean <- all.data.clean[,-3]

all.data.ready <- all.data.clean[,-1]
str(all.data.ready)
# redudant variables?
cor(all.data.ready)

all.data.ready$dummy <- 1
all.data.ready <- all.data.ready[,"dummy"]
all.data.ready 

all.data.ready <- all.data.ready[,-c(7,8)]
colnames(all.data.ready)

nrow(all.data.ready)

boxplot(SpO2~Glucose,data=all.data.ready, main="Looking for Outliers",
        xlab="Glucose", ylab="Sp02") 

summary(all.data.ready)

# normalization? 
all.data.ready.norm <- all.data.ready
all.data.ready.norm 
all.data.ready.norm$SpO2 <-  
  (all.data.ready$SpO2-min(all.data.ready$SpO2))/
    (max(all.data.ready$SpO2)-min(all.data.ready$SpO2))
summary(all.data.ready.norm)
all.data.clean$Patient
head(all.data.ready)
rownames(all.data.ready) <- all.data.clean$Patient

all.data.ready[1,]
colnames(all.data.ready) <- c("spo2", "glucose", "systolic", 
                              "diastolic", "abp", "hr", "imc")

wss <- vector()
set.seed(1234)
for (i in 2:10)
  wss[i] <- sum(kmeans(all.data.ready, centers=i)$withinss)


plot(2:10, wss[-1], type= "b", xlab="# clusters", ylab="Sum squares")

# we select 4!

set.seed(1234)
head(all.data.ready)
all.data.ready$group <- NULL
clusters <- kmeans(all.data.ready, centers=4)
library(fpc)
plotcluster(all.data.ready, clusters$cluster)

attributes(clusters)

clusters$centers

# glucose levels: 75 - 120
# systolic: 120
# diastolic: 80
# abp: 120

# Name the centers
# 1 sick   
# 2 healthy_enough
# 3 healthy
# 4 slightly_sick


all.data.ready$group <- clusters$cluster

head(all.data.ready)

all.data.ready[all.data.ready$group==1,]$group <- "sick"
all.data.ready[all.data.ready$group==2,]$group <- "healthy_enough"
all.data.ready[all.data.ready$group==3,]$group <- "healthy"
all.data.ready[all.data.ready$group==4,]$group <- "slightly_sick"


# CART
library(rpart)

formula1 <- group ~ .
?rpart
cart  <- rpart(formula=formula1, data=all.data.ready, method='class')
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(cart)


## Neural Net
install.packages('neuralnet')
library(neuralnet)
nrow(all.data.ready)
head(all.data.ready)
str(trainingSet)
index <- sample(2, nrow(all.data.ready), replace=T, prob=c(.7, .3))
index
trainingSet <- all.data.ready[index==1,]
testSet <- all.data.ready[index==2,]
head(trainingSet)
formula2 <- group ~ spo2 + glucose + systolic + diastolic + abp + hr + imc
set.seed(1234)
nn.health <- neuralnet(formula2, trainingSet, 20, threshold=.01, linear.output=T)
plot(nn.health)

# evaluate

head(testSet)
my.prediction <- compute(nn.health, testSet[,-1])
my.prediction
attributes(my.prediction)
results <- data.frame(actual=testSet$group,
                      prediction=my.prediction$net.result)

head(results, 50)






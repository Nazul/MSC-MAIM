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

# we select 5!

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
# 1 low spo2, low systolic, low diastolic: sligthly_sick   
# 2 high glocose, high systolic, high diastolic, high hr: sick
# 3 healthy
# 4 slightly low systolic, low abp: healthy_enough


all.data.ready$group <- clusters$cluster

head(all.data.ready)

all.data.ready[all.data.ready$group==1,]$group <- "sligthly_sick"
all.data.ready[all.data.ready$group==2,]$group <- "sick"
all.data.ready[all.data.ready$group==3,]$group <- "healthy"
all.data.ready[all.data.ready$group==4,]$group <- "healthy_enough"

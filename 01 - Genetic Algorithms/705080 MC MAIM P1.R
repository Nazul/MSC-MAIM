##**********************************************************
## ITESO - Maestría en Sistemas Computacionales
## Manejo y Análisis de Información Masiva
## Práctica 1. Genetic algorithms
##
## Mario Contreras (705080)
##
##*********************************************************
# Change working directory to this file's folder (running file)
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)
# Change working directory to this file's folder (IDE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install.packages("genalg")
library("genalg")


# 1)
# Fitness function - inversamente proporsional de un arreglo
fitness.one <- function(param=c()) {
  retVal <- 1 / sum(param)
  retVal
}

ga.one <- rbga.bin(size = 10, popSize = 100, iters = 200, mutationChance = 0.01, elitism = 20, evalFunc = fitness.one)
summary(ga.one, echo = TRUE)


# 2)
fitness.two <- function(param=c()) {
  if (length(param == 2)) {
    retVal <- abs(param[1] - pi) + abs(param[2] - sqrt(50))
  }
  else {
    stop("Invalid arguments")
  }
  retVal
}

ga.two <- rbga(c(1, 1), c(5,10), evalFunc = fitness.two, mutationChance = 0.01, elitism = 20, iters = 150)
summary(ga.two, echo = TRUE)


# 3)
items <- data.frame(item = c("pocketknife", "beans", "potatoes", "unions", "sleeping bag", "rope", "compass"),
                    survivalpoints = c(10, 20, 15, 2, 30, 10, 30),
                    weight = c(1, 5, 10, 1, 7, 5, 1))
items
weight.limit <- 20

fitness.three <- function(x) {
  items.weight <- x %*% items$weight
  items.survival.points <- x %*% items$survivalpoints
  if (items.weight > weight.limit) {
    return (0)
  }
  else {
    return (-items.survival.points)
  }
}

ga.three <- rbga.bin(size = 7, popSize = 200, iters = 200, mutationChance = 0.01, elitism = 20, evalFunc = fitness.three)
summary(ga.three, echo = TRUE)

bestSolution <- c(1, 1, 0, 1, 1, 1, 1)
bestSolution %*% items$survivalpoints
items[bestSolution==1,]


# EOF

# Neural network

# 1.1
install.packages('neuralnet')
library('neuralnet')

# 1.2
set.seed(1234)
# runif - random numbers (uniform distribution)
training.Set <- as.data.frame(runif(50, min = 0, max = 100))

# 1.3
training.Output <- sqrt(training.Set)
training <- cbind(training.Set, training.Output)
colnames(training) <- c("Input", "Output")
head(training)

# 1.4
formula1 <- Output~Input
nn1 <- neuralnet(formula = formula1, data = training, hidden = 10, threshold = 0.01)

# 1.5
print(nn1)
plot(nn1)

# 1.6
test.Data <- as.data.frame((1:10)^2)
test.Data

nn.Results <- compute(nn1, test.Data)
nn.Results

compare.Output <- cbind(test.Data, sqrt(test.Data), nn.Results$net.result)
colnames(compare.Output) <- c("Input", "Expected", "NN")
compare.Output$Delta <- compare.Output$Expected - compare.Output$NN
compare.Output

evaluation.nn1 <- sqrt(sum(compare.Output$Delta^2))
evaluation.nn1


#####

# 2.1
credit.data <- read.csv("C:\\Users\\Mario_Contreras\\Documents\\Downloads\\creditset.csv")
str(credit.data)

# 2.2
training.Set <- credit.data[1:800,]

# 2.3
test.Set <- credit.data[801:2000,]

# 2.4
formula2 <- default10yr~LTI+age
nn.Loan <- neuralnet(formula = formula2, data = training.Set, hidden = 4, threshold = 0.01)
print(nn.Loan)

# 2.5
plot(nn.Loan)

# 2.6
test.Data <- test.Set[, c("LTI", "age")]
head(test.Data)
credit.Results <- compute(nn.Loan, test.Data)

results <- data.frame(actual=test.Set$default10yr, prediction=round(credit.Results$net.result, digits = 0))
results[1:100,]


# 2.7
#compare <- sum(results$actual - results$prediction)
#compare
compare <- xor(results$actual, results$prediction)
utils::View(compare)
sum(compare, na.rm=TRUE)

# EOF

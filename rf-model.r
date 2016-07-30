library(caret)
library(pROC)
library(randomForest)

setwd('~/Desktop/honestbee/')

clean <- function(data) {
  data$X <- NULL

  # Convert to factor
  data$SeriousDlqin2yrs <- as.factor(data$SeriousDlqin2yrs)
  levels(data$SeriousDlqin2yrs) <- c("no", "yes")

  # Make names legal
  names(data) <- make.names(names(data))

  return(data)
}

transform <- function(data) {
  # New MonthlyDebtPayment column
  monthly.income <- data$MonthlyIncome

  na.income <- is.na(monthly.income)
  monthly.income[na.income] <- 1

  zero.income <- monthly.income == 0
  monthly.income[zero.income] <- 1

  data$MonthlyDebtPayment <- data$DebtRatio * monthly.income

  # Replace NA income with -1.
  monthly.income[na.income] <- -1
  monthly.income[zero.income] <- 0
  data$MonthlyIncome <- monthly.income

  # Set NA #Dependents to -1
  data$NumberOfDependents[is.na(data$NumberOfDependents)] <- -1

  # Total days of late
  data$TotalDaysLate <- data$NumberOfTime30.59DaysPastDueNotWorse +
                        data$NumberOfTime60.89DaysPastDueNotWorse +
                        data$NumberOfTimes90DaysLate

  # Debt - Income
  data$DebtIncomeDiff <- data$MonthlyDebtPayment - data$MonthlyIncome

  return(data)
}

# Preprocess data
preprocess <- function(data) {
  data <- clean(data)
  data <- transform(data)
  return(data)
}


balance.yes <- function(training, sample.size) {
  population <- training[training$SeriousDlqin2yrs == 'yes',]
  samples <- population[sample(nrow(population), sample.size, replace = TRUE), ]
  return(rbind(training, samples))
}


# Read training set
set.seed(10)
raw.training <- read.csv("data/cs-training.csv")
training <- preprocess(raw.training)
tr.decision <- training$SeriousDlqin2yrs
nmin <- sum(tr.decision == 'yes')

# RF settings
ntree <- 8000
mtry <- 4
nodesize <- 1
do.trace <- 100

cat('ntree:', ntree, fill = TRUE)
cat('mtry:', mtry, fill = TRUE)


# Fit RF to the whole training data

y.train <- training$SeriousDlqin2yrs
x.train <- subset(training, select = -SeriousDlqin2yrs)

sampsize <- rep(2500, 2)

## mtry parameter tuning
### CV settings
# fold <- 10
# repeats <- 5
# ctrl <- trainControl(method = "repeatedcv", number = fold, repeats = repeats, selectionFunction = "oneSE",
#                      classProbs = TRUE, summaryFunction = twoClassSummary, verboseIter = TRUE)
# rf.tune <- train(x = x.train, y = y.train, method = "rf",
#                   trControl = ctrl, maximize = TRUE, metric = "ROC",
#                   ntree = ntree, nodesize = nodesize, sampsize = sampsize, do.trace = do.trace)
# mtry <- rf.tune$bestTune$mtry
# cat('Tuning Results:\n')
# print(rf.tune$results)


print('Fitting RF to training data.')
rf.best <- randomForest(x = x.train,
                        y = y.train,
                        ntree = ntree,
                        mtry = mtry,
                        nodesize = nodesize,
                        sampsize = sampsize,
                        replace = TRUE,
                        do.trace = do.trace)

# Get AUC score
class.prob <- predict(rf.best, type = 'prob')
rf.auc <- auc(y.train, class.prob[,2])
print(rf.auc)

# Apply model to new test set
print("Predict test set")
test <- read.csv("data/cs-test.csv")
Id <- test$X
test <- preprocess(test)
prediction <- predict(rf.best, subset(test, select = -SeriousDlqin2yrs), type = "prob")
result <- data.frame(Id = Id, Probability = prediction[, 2])

# write to file
write.csv(result, "rf-prediction.csv", row.names = FALSE)

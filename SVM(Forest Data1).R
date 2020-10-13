# divide into training and test data
View(forest)

# Data exploration
summary(forest)
str(forest)
attach(forest)

str(size_category)
levels(size_category)

# Converting two categories into 0s and 1s
Size <- ifelse(size_category == "small", 0, 1)

# Normalize function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}


forest_n <- as.data.frame(lapply(forest[-29], normalize))
View(forest_n)

forest_n <- cbind(forest_n,Size)
attach(forest_n)

str(forest_n)
forest_n$Size <- as.factor(forest_n$Size)

library(caTools)

# Splitting data into training and testing
sample = sample.split(forest_n,SplitRatio = 0.75)
train1 =subset(forest_n,sample ==TRUE) 
test1=subset(forest_n, sample==FALSE)

install.packages("kernlab")
library(kernlab)
library(caret)

##Training a model on the data ----
forest_classifier <- ksvm(Size~ ., data = train1, scaled = FALSE, kernel = "vanilladot")

# basic information about the model
forest_classifier

## Evaluating model performance ----
forest_predictions <- predict(forest_classifier, test1)
head(forest_predictions)

confusionMatrix(table(forest_predictions, test1$Size))


## Improving model performance with other kernels
forest_classifier_rbf <- ksvm(Size ~ ., data = train1, scaled = FALSE, kernel = "rbfdot")

forest_predictions_rbf <- predict(forest_classifier_rbf, test1)

confusionMatrix(table(forest_predictions_rbf, test1$Size))


#Improving model performance with other kernels
forest_classifier_poly <- ksvm(Size ~ ., data = train1, scaled = FALSE, kernel = "polydot")

forest_predictions_poly <- predict(forest_classifier_poly, test1)

confusionMatrix(table(forest_predictions_poly, test1$Size))


#Improving model performance with other kernels
forest_classifier_ano <- ksvm(Size ~ ., data = train1, scaled = FALSE, kernel = "anovadot")

forest_predictions_ano <- predict(forest_classifier_ano, test1)

confusionMatrix(table(forest_predictions_ano, test1$Size))


#Improving model performance with other kernels
forest_classifier_lap <- ksvm(Size ~ ., data = train1, scaled = FALSE, kernel = "laplacedot")

forest_predictions_lap <- predict(forest_classifier_lap, test1)

confusionMatrix(table(forest_predictions_lap, test1$Size))


#Improving model performance with other kernels
forest_classifier_tan <- ksvm(Size ~ ., data = train1, scaled = FALSE, kernel = "tanhdot")

forest_predictions_tan <- predict(forest_classifier_tan, test1)

confusionMatrix(table(forest_predictions_tan, test1$Size))



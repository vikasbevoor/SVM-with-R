# divide into training and test data
View(forest)
forest <- forest[-c(1:2)]
str(forest$size_category)

Size <- ifelse(forest$size_category == "small" , 1, 2)
forest <- cbind(forest[-29],Size)

normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

forest_n <- as.data.frame(lapply(forest, normalize))
View(forest_n)
str(forest_n)
forest_n$Size <- as.factor(forest_n$Size)

forest_train <- forest_n[1:400,]
forest_test  <- forest_n[401:517,]
View(forest_train)

##Training a model on the data ----
# begin by training a simple linear SVM
install.packages("kernlab")
library(kernlab)
forest_classifier <- ksvm(Size ~ ., data = forest_train, scaled = FALSE, kernel = "vanilladot")
library(caret)

# basic information about the model
forest_classifier

## Evaluating model performance ----
# predictions on testing dataset
forest_predictions <- predict(forest_classifier, forest_test)
head(forest_predictions)

confusionMatrix(table(forest_predictions, forest_test$Size))

#OR,
agreement <- forest_predictions == forest_test$Size
table(agreement)
prop.table(table(agreement))


## Improving model performance ----
forest_classifier_rbf <- ksvm(Size ~ ., data = forest_train, scaled = FALSE, kernel = "rbfdot")
forest_predictions_rbf <- predict(forest_classifier_rbf, forest_test)

agreement_rbf <- forest_predictions_rbf == forest_test$Size
table(agreement_rbf)
prop.table(table(agreement_rbf))

?ksvm

#Improving model performance ----
forest_classifier_poly <- ksvm(Size ~ ., data = forest_train, scaled = FALSE, kernel = "polydot")
forest_predictions_poly <- predict(forest_classifier_poly, forest_test)

agreement_poly <- forest_predictions_poly == forest_test$Size
table(agreement_poly)
prop.table(table(agreement_poly))

# divide into training and test data
View(Sal_Test)
View(Sal_Train)

library(moments)

# Data exploration
summary(Sal_Train)
str(Sal_Train)
attach(Sal_Train)

summary(age)
str(age)
boxplot(age, col = "red")
hist(age)
skewness(age)
kurtosis(age)

table(Salary)

# Splitting data into training and testing
Sal_train <- Sal_Train
Sal_test  <- Sal_Test


install.packages("kernlab")
library(kernlab)
library(caret)


#Training a model on the data 
Salary_classifier <- ksvm(Salary ~., data = Sal_train,kernel = "vanilladot")

# basic information about the model
Salary_classifier

# Evaluating model performance
Salary_predictions <- predict(Salary_classifier, Sal_test)
head(Salary_predictions)

confusionMatrix( table(Salary_predictions, Sal_test$Salary))

# Improving model performance 
Salary_classifier_rbf <- ksvm(Salary ~ ., data = Sal_train, kernel = "rbfdot")

Salary_predictions_rbf <- predict(Salary_classifier_rbf, Sal_test)

confusionMatrix( table(Salary_predictions_rbf, Sal_test$Salary))


#Improving model performance 
Salary_classifier_poly <- ksvm(Salary ~ ., data = Sal_train, kernel = "polydot")

Salary_predictions_poly <- predict(Salary_classifier_poly, Sal_test)

confusionMatrix( table(Salary_predictions_poly, Sal_test$Salary))


#Improving model performance 
Salary_classifier_ano <- ksvm(Salary ~ ., data = Sal_train, kernel = "anovadot")

Salary_predictions_ano <- predict(Salary_classifier_ano, Sal_test)

confusionMatrix( table(Salary_predictions_ano, Sal_test$Salary))

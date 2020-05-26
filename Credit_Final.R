library(C50)
str(credit)
View(credit)
credit[, 'default'] <- as.factor(credit[, 'default'])
str(credit)
credit[, 'existing_credits'] <- as.integer(credit[, 'existing_credits'])

set.seed(123458)
in_train <- sample(1:nrow(credit), size =650)
train_data <- credit[in_train, ]
test_data <- credit[-in_train, ]

tree_m <- C5.0( x= train_data [ , -17], y=train_data$default, control = C5.0Control(minCases = 20, earlyStopping = TRUE ))

tree_m

plot(tree_m)

summary(tree_m)


#Boosting

tree_n <- C5.0( x= train_data [ , -17], y=train_data$default,trials = 3, control = C5.0Control(minCases = 20, earlyStopping = TRUE ))


tree_n

summary(tree_n)
plot(tree_n)

#Predict

predict( tree_n, newdata = test_data[1:3, -17], type = "prob")


predict( tree_n, newdata = test_data[1:3, -17])

predict.c5 <- predict(tree_n, test_data[,-17])
predict.c52 <- predict(tree_m, test_data[,-17])
print(predict.c5)
print(predict.c52)
# Confusion Matrix
confusionMatrix(predict.c5, test_data$default)
confusionMatrix(predict.c52, test_data$default)

# Using Rpart Algorithm fror the Prediction

library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(ggplot2)
library(bitops)
library(rattle)


tree_r <- rpart(default ~., data = train_data, control = rpart.control(minsplit = 5))
View(tree_r)
rpart.plot(tree_r, extra = 106)
rpart.plot.version1(tree_r)

#Creatinh optimal Decision Plot using Rpart
plot(tree_r)
text(tree_r, pretty=0)
rpart.plot(tree_r)
library(rattle)

fancyRpartPlot(tree_r)
printcp(tree_r)
plotcp(tree_r)

ptree_r <- prune(tree_r, cp =  tree_r$cptable[which.min(tree_r$cptable[,"xerror"]), "CP"])
fancyRpartPlot(ptree_r, uniform=TRUE, main="Pruned Tree")

#predictions
predict.rpart <- predict(tree_r, test_data, type = 'class')
predict.rpartprune <- predict(ptree_r, test_data, type = 'class')

#Confusion matrix for evaluating Model
confusionMatrix(predict.rpart, test_data$default)
confusionMatrix(predict.rpartprune, test_data$default)


#Drawing tree
prp(tree_r)
fancyRpartPlot(tree_r)


#Using Decision Tree Party Package
library(party)
library(sandwich)

tr <- ctree(default~. , data = train_data, controls = ctree_control(mincriterion = 0.99, minsplit = 10))

plot(tr)
print(tr)

#Prediction
Predict(tr, test_data)

tab1 <- table(predict(tr), train_data$default)
print(tab1)

#Confusion Matric
confusionMatrix(predict(tr, test_data), test_data$default)



# Using Random Forrest
library(randomForest)
forest_tr <- randomForest(default~., data = train_data)
plot(forest_tr)
predict_forest_tr <- predict(forest_tr, test_data)
print(predict_forest_tr)

confusionMatrix(predict_forest_tr, test_data$default)

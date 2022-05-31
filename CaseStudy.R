library(ISLR)
library(boot)
library(caTools)
library(caret)
library(pROC)
library(tidyverse)
library(dplyr)
library(Hmisc)

#Clean data
data_set <- read.csv("flight.csv", header=TRUE, stringsAsFactors=FALSE)
data_set$Month <- NULL
data_set1 <- na.omit(data_set)
summary(data_set1)

set.seed(123)
table(train_data$Canceled)

#upsampling the data
up_dataset <- upSample(x= data_set1[,colnames(data_set1) %nin% "Canceled"],
         y= data_set1$Canceled)

table(up_dataset$Class)

#setting variables as factors
data_set1$Canceled <- as.factor(data_set1$Canceled)

data_set1$UniqueCarrier <- as.factor(data_set1$UniqueCarrier)


train <- createDataPartition(data_set1$Canceled, list = F, p= .8)

train_data <- data_set1[train, ]
test_data <- data_set1[-train, ]

glm.bi  <- glm(Canceled ~., data = train_data, family=binomial)

summary(glm.bi)


res_train <- predict(glm.bi, train_data, type = "response")
res_test <- predict(glm.bi, test_data, type = "response")

(confmat_test <- table(Actual_value=test_data$Canceled, Predicted_Value = res_test >.33))

confusionMatrix(as.factor(as.integer(res_test >0.33)), as.factor(test_data$Canceled), mode = "everything", positive="1")
(matrix_test <- sum(diag(confmat_test)) / sum(confmat_test))

# let's try with upsampled dataset
train <- createDataPartition(up_dataset$Class, list = F, p= .8)

train_data <- up_dataset[train, ]
test_data <- up_dataset[-train, ]

glm.bi.up  <- glm(Class ~., data = train_data, family=binomial)

summary(glm.bi)
summary(glm.bi.up)


res_train_up <- predict(glm.bi.up, train_data, type = "response")
res_test_up <- predict(glm.bi.up, test_data, type = "response")

(confmat_test <- table(Actual_value=test_data$Class, Predicted_Value = res_test_up >0.4))

confusionMatrix(as.factor(as.integer(res_test_up >0.3)), as.factor(test_data$Clas), mode = "everything", positive="1")

(matrix_test <- sum(diag(confmat_test)) / sum(confmat_test))
#F1_Score(y_true=test_data$Class, y_pred = as.numeric(res_test >0.5))

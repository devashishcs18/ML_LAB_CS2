setwd("C:/Users/Acer/Desktop/Computer Science/ML")
data<-read.csv("stroke.csv", na.strings = "N/A")

library(e1071)
library(caTools)
library(caret)
library(tidyverse)
library(ROCR)

data<-as.data.frame(data)
data<-data[complete.cases(data),]
data$gender<-as.numeric(as.factor(data$gender))
data$ever_married<-as.numeric(as.factor(data$ever_married))
data$work_type<-as.numeric(as.factor(data$work_type))
data$Residence_type<-as.numeric(as.factor(data$Residence_type))
data$smoking_status<-as.numeric(as.factor(data$smoking_status))
data$stroke<-as.factor(data$stroke)
str(data)

# Splitting data into train
# and test data
split <- sample.split(data, SplitRatio = 0.7)
train <- subset(data, split == "TRUE")
test <- subset(data, split == "FALSE")


logistic_model <- glm( stroke ~gender + age + hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + bmi + smoking_status , data = train, family = "binomial")
logistic_model

summary(logistic_model) 


# Predict test data based on model 
predict_reg <- predict(logistic_model,  
                       test, type = "response") 
predict_reg   

# Changing probabilities 
predict_reg <- ifelse(predict_reg >0.5, 1, 0) 

# Evaluating model accuracy 
# using confusion matrix 
cm<-table(test$stroke , predict_reg)
print(cm)
missing_classerr <- mean(predict_reg != test$stroke) 
print(paste('Accuracy =', 1 - missing_classerr)) 




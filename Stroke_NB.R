setwd("C:/Users/Acer/Desktop/Computer Science/ML")
data<-read.csv("stroke.csv", na.strings = "N/A")

library(e1071)
library(caTools)
library(caret)

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
train_cl <- subset(data, split == "TRUE")
test_cl <- subset(data, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 2:11])
test_scale <- scale(test_cl[, 2:11])

# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes( stroke ~gender + age + hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + bmi + smoking_status , data = train_cl)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$stroke , y_pred)
cm

# Model Evauation
d<-confusionMatrix(cm)
print(d)

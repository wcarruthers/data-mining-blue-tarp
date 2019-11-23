setwd("~/uva_dsi/SYS6018/project1")
train_data = read.csv("HaitiPixels.csv")
setwd("~/uva_dsi/SYS6018/project1")
setwd("~/uva_dsi/SYS6018/project_final/Hold Out Data")
test_data = read.csv("HoldOut_Pixels.csv")
train_data$is_blue_tarp <- factor(ifelse(train_data$Class == "Blue Tarp", "True", "False"))
summary(test_data)
summary(train_data)

#libraries
library(caret)

length(unique(train_data$Class))
#Vegetation, Soil, Rooftop, Varioous Non-Tarp, Blue Tarp

#I'm first going to create a factor that is true if blue tarp and false otherwise

train_data$is_blue_tarp <- factor(ifelse(train_data$Class == "Blue Tarp", "True", "False"))
set.seed(17)

#################
#LOGISTIC REGRESSION
#################
glm.fit.train <- glm(is_blue_tarp ~ Red + Green + Blue, 
                     data = train_data,
                     family = binomial)

glm.pred <- rep("False", nrow(test_data))
glm.probs <- predict(glm.fit.train, test_data, type = 'response')
glm.pred[glm.probs>0.5]<- "True"
confusionMatrix(as.factor(glm.pred), test_data$is_blue_tarp)
#detection rate
d_rate <- 14310/(14310+170)
d_rate
#false alarm rate
f_rate <- 20314/(20314+1969383)
f_rate

#AUC
library(ROCR)
pred.log.glm <- prediction(glm.probs, test_data$is_blue_tarp)
perf.log.glm <- performance(pred.log.glm, "tpr", "fpr")
auc.log.glm <- performance(pred.log.glm, 'auc')
auc.log.glm@y.values[[1]]

#################
#LDA
#################

#model with training data
lda.fit=lda(is_blue_tarp ~ Red + Green + Blue, data=train_data)

#predicted test set 
lda.pred=predict(lda.fit, test_data)$class

confusionMatrix(lda.pred, test_data$is_blue_tarp)
#detection rate
d_rate <- 12148/(12148+2332)
d_rate
#false alarm rate
f_rate <- 34245/(34245+1955452)
f_rate

#auc curve
library(ROCR)
pred.lda = prediction(predict(lda.fit, test_data)$posterior[,2], test_data$is_blue_tarp)
auc.lda = performance(pred.lda, "auc")
auc.lda@y.values[[1]]

#################
#QDA
#################

#model with training data
qda.fit=qda(is_blue_tarp ~ Red + Green + Blue, data=train_data)

#predicted test set 
qda.pred=predict(qda.fit, test_data)$class

confusionMatrix(qda.pred, test_data$is_blue_tarp)
#detection rate
d_rate <- 10058/(10058+4422)
d_rate
#false alarm rate
f_rate <- 3651/(3651+1986046)
f_rate

#auc curve
library(ROCR)
pred.qda = prediction(predict(qda.fit, test_data)$posterior[,2], test_data$is_blue_tarp)
auc.qda = performance(pred.qda, "auc")
auc.qda@y.values[[1]]


#################
#KNN
#################
library(class)
library(caret)
train.X <- data.frame(train_data$Red, train_data$Green, train_data$Blue)
test.X <- data.frame(test_data$Red, test_data$Green, test_data$Blue)
#test_sample <- test_data[sample(1:nrow(test_data), 500000,
#                          replace=FALSE),] 
#test.X <- data.frame(test.X$Red, test.X$Green, test.X$Blue)

#changed to test_sample
train_data.is_blue_tarp<- train_data$is_blue_tarp
knn.pred = knn(train.X, test.X, train_data.is_blue_tarp, k=6)


confusionMatrix(knn.pred, test_data$is_blue_tarp)


#detection rate
d_rate <- 11890/(11890+2590)
d_rate
#false alarm rate
f_rate <- 12843/(1976854+12843)
f_rate

#################
#Random Forest need to recheck
#################

rf.fit <-randomForest(is_blue_tarp ~ Red + Green + Blue, data=train_data, mtry=1, ntree = 250)
rf.predy = predict(rf.fit, test_data)
confusionMatrix(rf.predy, test_data$is_blue_tarp)
summary(test_data$is_blue_tarp)
library(pROC)
library(ROCR)
##confusion matrix must be flipped here..
#detection rate
d_rate <- 11185/(11185+3295)
d_rate
#false alarm rate
f_rate <- 6907/(1989697)
f_rate


#################
#SVM (Kernel = Radial)
#################
library(e1071)
svm.fit <- svm(is_blue_tarp ~ Red + Green + Blue, kernel = 'radial', data = train_data, cost = 5, gamma = 50)
svm.pred = predict(svm.fit, test_data)
a <-ifelse(test_data$is_blue_tarp == 'False', 1,0)
auc_means[i]<-auc(svm.pred, a)
p <- ifelse(svm.pred == "False", 1,0)
#might have to put p in place of svm.pred
confusionMatrix(svm.pred, test_data$is_blue_tarp)

library(pROC)
library(ROCR)

#detection rate
d_rate <- 10355/(10355+4125)
d_rate
#false alarm rate
f_rate <- 9601/(9601+1980096)
f_rate
auc(svm.pred, test_data$is_blue_tarp)



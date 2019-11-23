setwd("~/uva_dsi/SYS6018/project1")
haiti_data = read.csv("HaitiPixels.csv")

length(unique(haiti_data$Class))
#Vegetation, Soil, Rooftop, Varioous Non-Tarp, Blue Tarp

#I'm first going to create a factor that is true if blue tarp and false otherwise

haiti_data$is_blue_tarp <- factor(ifelse(haiti_data$Class == "Blue Tarp", "True", "False"))

# this is my custom function to calculate False Alarm Rate and Detection Rate
# detection Rate = Number of correct Blue Tarps/ number of total blue tarps
# false alarm rate = number of incorrect blue tarps/(number of non-blue tarps)
custom_err <- function(df){
  actual <- df['a']
  pred <- df['p']
  
  #newvec <-  ifelse(actual==0 & pred==0, "TN",ifelse(actual==0 & pred==1, "FP",
  #                                                   ifelse(actual==1 & pred==0, "FN", "TP")))
  
  #0 for Blue Tarp
  #1 for Not Blue Tarp
  correct_tarps <- ifelse(actual==0 & pred == 0, TRUE, FALSE)
  blue_tarps <- ifelse(actual==0, TRUE, FALSE)
  correct_tarps_cnt <- sum(correct_tarps, na.rm = TRUE)
  blue_tarps_cnt <- sum(blue_tarps, na.rm = TRUE)
  incorrect_blue_tarps <- ifelse(actual==1 & pred == 0, TRUE, FALSE)
  non_blue_tarps <- blue_tarps <- ifelse(actual==1, TRUE, FALSE)
  detection_rate <- correct_tarps_cnt/blue_tarps_cnt
  #f_alarm_rate <- (length(newvec[newvec=='FP']))/(length(newvec[newvec=='FP'])+length(newvec[newvec=='TN']))
  f_alarm_rate <- (sum(incorrect_blue_tarps, na.rm = TRUE)/sum(non_blue_tarps, na.rm = TRUE))
  list(d_rate=as.numeric(detection_rate), f_rate = as.numeric(f_alarm_rate))  
}
# k-Fold Cross-Validation

set.seed(17)
#setting up K folds
haiti_data$k_fold <- sample(10, size = nrow(haiti_data), replace = TRUE)

error_means <- rep(0,10)
auc_means <- rep(0,10)
detection_rate <- rep(0,10)
false_alarm_rate <- rep(0,10)
i = 1
for (i in 1:10){
  glm.fit.train <- glm(is_blue_tarp ~ Red + Green + Blue, 
                       data = haiti_data[haiti_data$k_fold != i,], 
                       family = binomial)
  glm.pred <- rep("False", nrow(haiti_data[haiti_data$k_fold == i,]))
  glm.probs <- predict(glm.fit.train, haiti_data[haiti_data$k_fold == i,], type = 'response')
  glm.pred[glm.probs>0.5]<- "True"
  error_means[i] <- mean(glm.pred != haiti_data[haiti_data$k_fold == i,]$is_blue_tarp)
  library(ROCR)
  pred.k <- prediction(glm.probs, haiti_data[haiti_data$k_fold == i,]$is_blue_tarp)
  perf.k <- performance(pred.k, "tpr", "fpr")
  #need to figure out how to get auc valuess.
  auc.k <- performance(pred.k, 'auc')
  auc_means[i] <- auc.k@y.values[[1]]
  #0 for Blue Tarp
  #1 for Not Blue Tarp
  a <-ifelse(haiti_data[haiti_data$k_fold == i,]$is_blue_tarp == 'False', 1,0)
  p <- ifelse(glm.pred == "False", 1,0)
  df = data.frame(a,p)
  h <- custom_err(df)
  detection_rate[i] <- h$d_rate
  false_alarm_rate[i] <- h$f_rate
  #confusionMatrix(as.factor(glm.pred), haiti_data[haiti_data$k_fold == i,]$is_blue_tarp)
  #190 correct blue tarps out of 190+22
  #7 incorrect out of (6119 + 7)
  #190/(190+22)
  #7/(7+6119)
}

1-mean(error_means)
mean(auc_means)
mean(detection_rate)
mean(false_alarm_rate)


i = 1
#LDA
cv.lda <-
  function (data, model=is_blue_tarp ~ Red + Green + Blue, yname="is_blue_tarp", K=10, seed=123) {
    n <- nrow(data)
    set.seed(seed)
    datay=data[,yname] #response variable
    library(MASS)
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    CV=NULL
    auc_means <- rep(0,10)
    detection_rate <- rep(0,10)
    false_alarm_rate <- rep(0,10)
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      lda.fit=lda(model, data=data[train.index,])
      #observed test set y
      lda.y <- data[test.index, yname]
      #predicted test set y
      lda.predy=predict(lda.fit, data[test.index,])$class
      
      #observed - predicted on test data
      error= mean(lda.y!=lda.predy)
      #error rates 
      CV=c(CV,error)
      #auc curve
      library(ROCR)
      pred.k = prediction(predict(lda.fit, data[test.index,])$posterior[,2], data[test.index,]$is_blue_tarp)
      auc.k = performance(pred.k, "auc")
      auc_means[i] <- auc.k@y.values[[1]]
  
      a <-ifelse(data[test.index,]$is_blue_tarp == 'False', 1,0)
      p <- ifelse(lda.predy == "False", 1,0)
      df = data.frame(a,p)
      h <- custom_err(df)
      detection_rate[i] <- h$d_rate
      false_alarm_rate[i] <- h$f_rate
      
    }
    #Output
    list(call = model, K = K, 
         lda_error_rate = mean(CV), 
         seed = seed, auc_mean = mean(auc_means), 
         d_rate = mean(detection_rate), f_rate = mean(false_alarm_rate))  
  }
er2<-cv.lda(data = haiti_data, model=is_blue_tarp ~ Red + Green + Blue, yname="is_blue_tarp", K=10, seed=17)
1-er2$lda_error_rate
er2$auc_mean
er2$d_rate
er2$f_rate

#QDA
cv.qda <-
  function (data, model=is_blue_tarp ~ Red + Green + Blue, yname="is_blue_tarp", K=10, seed=123) {
    n <- nrow(data)
    set.seed(seed)
    datay=data[,yname] #response variable
    library(MASS)
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    
    CV=NULL
    auc_means <- rep(0,10)
    detection_rate <- rep(0,10)
    false_alarm_rate <- rep(0,10)
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      qda.fit=qda(model, data=data[train.index,])
      #observed test set y
      qda.y <- data[test.index, yname]
      #predicted test set y
      qda.predy=predict(qda.fit, data[test.index,])$class
      
      #observed - predicted on test data
      error= mean(qda.y!=qda.predy)
      #error rates 
      CV=c(CV,error)
      #auc curve
      library(ROCR)
      pred.k = prediction(predict(qda.fit, data[test.index,])$posterior[,2], data[test.index,]$is_blue_tarp)
      auc.k = performance(pred.k, "auc")
      auc_means[i] <- auc.k@y.values[[1]]
      
      a <-ifelse(data[test.index,]$is_blue_tarp == 'False', 1,0)
      p <- ifelse(qda.predy == "False", 1,0)
      df = data.frame(a,p)
      h <- custom_err(df)
      detection_rate[i] <- h$d_rate
      false_alarm_rate[i] <- h$f_rate
    }
    #Output
    list(call = model, K = K, 
         qda_error_rate = mean(CV), seed = seed, 
         seed = seed, auc_mean = mean(auc_means), 
         d_rate = mean(detection_rate), f_rate = mean(false_alarm_rate))  
  }
er1<-cv.qda(data = haiti_data, model=is_blue_tarp ~ Red + Green + Blue, yname="is_blue_tarp", K=10, seed=17)
1-er1$qda_error_rate
er1$auc_mean
er1$d_rate
er1$f_rate


#K NNN
cv.knn <-
  function (data, k_fold=10, k_value=1, seed=123) {
    n <- nrow(data)
    set.seed(seed)
    
    library(class)
    #partition the data into K subsets
    f <- ceiling(n/k_fold)
    s <- sample(rep(1:k_fold, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    
    CV=NULL
    detection_rate <- rep(0,10)
    false_alarm_rate <- rep(0,10)
    
    for (i in 1:k_fold) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      train.X <- data.frame(data$Red[train.index], data$Green[train.index], data$Blue[train.index])
      test.X <- data.frame(data$Red[test.index], data$Green[test.index], data$Blue[test.index])
      train.is_blue_tarp <- data$is_blue_tarp[train.index]
      test.is_blue_tarp <- data$is_blue_tarp[test.index]
      
      knn.pred = knn(train.X, test.X, train.is_blue_tarp, k=k_value)
      
      #observed - predicted on test data
      error= mean(knn.pred!=test.is_blue_tarp)
      #error rates 
      CV=c(CV,error)
      
      a <-ifelse(test.is_blue_tarp == 'False', 1,0)
      p <- ifelse(knn.pred == "False", 1,0)
      df = data.frame(a,p)
      h <- custom_err(df)
      detection_rate[i] <- h$d_rate
      false_alarm_rate[i] <- h$f_rate
    }
    #Output
    list(K = k_value, 
         knn_error_rate = mean(CV), seed = seed,
         d_rate = mean(detection_rate), f_rate = mean(false_alarm_rate))  
  }

er1<-cv.knn(data = haiti_data, k_fold=10, k_value=6, seed=17)
1-er1$knn_error_rate
er1$d_rate
er1$f_rate

#SVM
library("pROC")
library(ROCR)
cv.svm <-
  function (data, model=is_blue_tarp ~ Red + Green + Blue, yname="is_blue_tarp", K=10, seed=123, kernel = 'linear') {
    n <- nrow(data)
    set.seed(seed)
    datay=data[,yname] #response variable
    library(e1071)
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    CV=NULL
    auc_means <- rep(0,10)
    detection_rate <- rep(0,10)
    false_alarm_rate <- rep(0,10)
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      svm.fit <- svm(model, kernel = 'radial', data = data[train.index,], cost = 5, gamma = 50) #update cost
      #observed test set y
      svm.y <- data[test.index, yname]
      #predicted test set y
      svm.predy = predict(svm.fit, data[test.index,])
      
      #observed - predicted on test data
      error= mean(svm.y!=svm.predy)
      #error rates 
      CV=c(CV,error)
      #auc curve
      
      #HAVEN"T FIGURED THIS PART OUT
      #pred.k = prediction(predict(lda.fit, data[test.index,])$posterior[,2], data[test.index,]$is_blue_tarp)
      #auc.k = performance(pred.k, "auc")
      #auc_means[i] <- auc.k@y.values[[1]]
      
      
      a <-ifelse(data[test.index,]$is_blue_tarp == 'False', 1,0)
      p <- ifelse(svm.predy == "False", 1,0)
      df = data.frame(a,p)
      h <- custom_err(df)
      
      auc_means[i]<-auc(svm.predy, df$a)
      detection_rate[i] <- h$d_rate
      false_alarm_rate[i] <- h$f_rate
      
    }
    #Output
    list(call = model, K = K, 
         svm_error_rate = mean(CV), 
         seed = seed, auc_mean = mean(auc_means), 
         d_rate = mean(detection_rate), f_rate = mean(false_alarm_rate))  
  }
er2<-cv.svm(data = haiti_data, model=is_blue_tarp ~ Red + Green + Blue, yname="is_blue_tarp", K=10, seed=17, kernel = "radial")

er2$svm_error_rate
er2$auc_mean
er2$d_rate
er2$f_rate



#random forests...
?randomForest
library('randomForest')
indexes = createDataPartition(haiti_data$is_blue_tarp, p = .66, list = F)
train = haiti_data[indexes,]
test = haiti_data[-indexes,]

rf.fit <-randomForest(is_blue_tarp ~ Red + Green + Blue, data=train, mtry=1, ntree = 250)
rf.predy = predict(rf.fit, test, type = 'response')
as.factor(rf.predy)
confusionMatrix(test$is_blue_tarp, as.factor(rf.predy))



cv.rf <-
  function (data, model=is_blue_tarp ~ Red + Green + Blue, yname="is_blue_tarp", K=10, seed=123) {
    n <- nrow(data)
    set.seed(seed)
    datay=data[,yname] #response variable

    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    CV=NULL
    auc_means <- rep(0,10)
    detection_rate <- rep(0,10)
    false_alarm_rate <- rep(0,10)
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      rf.fit <-randomForest(model,data=data[train.index,],mtry=1, ntree = 250)
      #predict
      nrow(data[test.index,])
      rf.predy = predict(rf.fit,data[test.index,])
      
    
      error= mean(data[test.index, yname]!=as.factor(rf.predy))
      #error rates 
      CV=c(CV,error)
      #auc curve
      
      #HAVEN"T FIGURED THIS PART OUT
      #pred.k = prediction(predict(lda.fit, data[test.index,])$posterior[,2], data[test.index,]$is_blue_tarp)
      #auc.k = performance(pred.k, "auc")
      #auc_means[i] <- auc.k@y.values[[1]]
      
      a <-ifelse(data[test.index,]$is_blue_tarp == 'False', 1,0)
      p <- ifelse(as.factor(rf.predy) == "False", 1,0)
      df = data.frame(a,p)
      h <- custom_err(df)
      
      #auc_means[i]<-auc(rf.predy, df$a)
      detection_rate[i] <- h$d_rate
      false_alarm_rate[i] <- h$f_rate
      
    }
    #Output
    list(call = model, K = K, 
         rf_error_rate = mean(CV), 
         seed = seed, auc_mean = mean(auc_means), 
         d_rate = mean(detection_rate), f_rate = mean(false_alarm_rate))  
  }
er2<-cv.rf(data = haiti_data, model=is_blue_tarp ~ Red + Green + Blue, yname="is_blue_tarp", K=10, seed=17)

er2$rf_error_rate
er2$auc_mean
er2$d_rate
er2$f_rate

#tuning portion
tune.out = tune(svm, is_blue_tarp ~ Red + Green + Blue, data = train_data, kernel = "linear", 
                ranges = list(cost = c(0.01,0.1, 1, 5, 10, 100)))
summary(tune.out)

tune.out = tune(svm, is_blue_tarp ~ Red + Green + Blue, data = train_data, kernel = "polynomial", 
                ranges = list(cost = c(0.15, 1, 5, 15), 
                              degree = c(2, 3, 4, 5)))
summary(tune.out)


tune.out = tune(svm, is_blue_tarp ~ Red + Green + Blue, data = train_data, kernel = "radial", 
                ranges = list(cost = c(0.15, 1, 5, 15), 
                              gamma = c(0.01, 0.1, 1, 10, 50, 100)))
summary(tune.out)

############random forest tuning######################
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


test_sample <- haiti_data[sample(1:nrow(haiti_data), 45000,
                                 replace=FALSE),] 
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1,2,3), .ntree=c(100, 250, 500, 750))
metric <- "Accuracy"
custom <- train(is_blue_tarp ~ Red + Green + Blue, data = test_sample, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)
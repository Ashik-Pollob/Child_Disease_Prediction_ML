# Logistic Regression
Lr <- train(ch_ari ~ ., 
            data = ari.rose,
            method = "glm",
            family = "binomial",
            trControl = train_control
)
lm_pred<-predict(Lr, test, type = "raw")
confusionMatrix(lm_pred,as.factor(test$ch_ari),mode='everything')
roc1 <- roc(test$ch_ari, as.numeric(lm_pred))
auc(roc1)

##Decision Tree
data.rose <- ROSE(ch_ari ~ ., data = train, seed = 00000)$data
Dt <- train(ch_ari ~ .,
            data = data.rose,
            method = "rpart",
            trControl = train_control
            
)
tree_pred <- predict(Dt, test)
confusionMatrix(tree_pred,test$ch_ari,mode='everything')
roc2 <- roc(test$ch_ari, as.numeric(tree_pred))
auc(roc2)

##SVM
data.rose <- ROSE(ch_ari ~ ., data = train, seed = 123)$data
Svm <- train(ch_ari ~., 
             data = data.rose, 
             method = "svmLinear", 
             trControl = train_control,  
             preProcess = c("center","scale")
             )
svm_pred <- predict(Svm, test,  type = "raw")
confusionMatrix(svm_pred,test$ch_ari,mode='everything')
roc3 <- roc(test$ch_ari, as.numeric(svm_pred))
auc(roc3)

##KNN
data.rose <- ROSE(ch_ari ~ ., data = train, seed = 123)$data
Knn <- train(ch_ari ~ .,
             data = data.rose,
             method = 'knn',
             tuneLength = 44,
             trControl = train_control,
             preProc = c("center", "scale"),
             metric = "ROC"
)
knn_pred <- predict(Knn, test)
confusionMatrix(knn_pred,test$ch_ari,mode='everything')
roc4 <- roc(test$ch_ari, as.numeric(knn_pred))
auc(roc4)

##ANN
data.rose <- ROSE(ch_ari ~ ., data = train, seed = 1)$data
Ann <- train(ch_ari~.,
             data=data.rose,
             method = "nnet",
             trControl= train_control,
             preProcess=c("scale","center")
)
ann_pred <- predict(Ann, test)
confusionMatrix(ann_pred,test$ch_ari,mode='everything')
roc5 <- roc(test$ch_ari, as.numeric(ann_pred))
auc(roc5)

##RF
data.rose <- ROSE(ch_ari ~ ., data = train, seed = 123)$data
tuningGrid <- expand.grid(
  mtry = c(1, 2, 3, 4, 5))

Rf <- train(
  ch_ari ~ .,
  data = data.rose,
  method = "rf",
  trControl = train_control,
  ntree = 500
)
rf_pred <- predict(Rf, test,  type = "raw")
confusionMatrix(rf_pred,test$ch_ari,mode='everything')
roc6 <- roc(test$ch_ari, as.numeric(rf_pred))
auc(roc6)

---
Title: "Child Diseases Classification Diarrhea"
Author: "S.M. Ashikul Islam Pollob"
Date: "16-09-2023"
---


# Logistic Regression
Lr <- train(ch_diar ~ ., 
            data = dia.rose,
            method = "glm",
            family = "binomial",
            trControl = train_control
           )

lm_pred<-predict(Lr, test, type = "raw")
confusionMatrix(lm_pred,as.factor(test$ch_diar),mode='everything')
roc1 <- roc(test$ch_diar, as.numeric(lm_pred))
auc(roc1)



#Decision Tree
Dt <- train(ch_diar ~ .,
            data = dia.rose,
            method = "rpart",
            trControl = train_control
            )

tree_pred <- predict(Dt, test)
confusionMatrix(tree_pred,test$ch_diar,mode='everything')
roc2 <- roc(test$ch_diar, as.numeric(tree_pred))
auc(roc2)



##SVM
Svm <- train(ch_diar ~., 
             data = dia.rose, 
             method = "svmLinear", 
             trControl = train_control,  
             preProcess = c("center","scale")
             )

svm_pred <- predict(Svm, test,  type = "raw")
confusionMatrix(svm_pred,test$ch_diar,mode='everything')
roc3 <- roc(test$ch_diar, as.numeric(svm_pred))
auc(roc3)



##KNN
Knn <- train(ch_diar ~ .,
             data = dia.rose,
             method = 'knn',
             tuneLength = 44,
             trControl = train_control,
             preProc = c("center", "scale"),
             metric = "ROC"
            )

knn_pred <- predict(Knn, test)
confusionMatrix(knn_pred,test$ch_diar,mode='everything')
roc4 <- roc(test$ch_diar, as.numeric(knn_pred))
auc(roc4)



##ANN
Ann <- train(ch_diar~.,
             data=dia.rose,
             method = "nnet",
             trControl= train_control,
             preProcess=c("scale","center")
)
ann_pred <- predict(Ann, test)
confusionMatrix(ann_pred,test$ch_diar,mode='everything')
roc5 <- roc(test$ch_diar, as.numeric(ann_pred))
auc(roc5)



#RF
Rf <- train(
  ch_diar ~ .,
  data = dia.rose,
  method = "rf",
  trControl = train_control,
  ntree = 500
          )

rf_pred <- predict(Rf, test,  type = "raw")
confusionMatrix(rf_pred,test$ch_diar,mode='everything')
roc6 <- roc(test$ch_diar, as.numeric(rf_pred))
auc(roc6)

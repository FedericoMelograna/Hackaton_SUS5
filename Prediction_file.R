rm(list=ls())
dataset<-read.csv("datifinali.csv",sep=",")
dataset<-dataset[,-1]
dati<-dataset[dataset$Target_cost_euro>0,]
test<-dataset[dataset$Target_cost_euro==0,]
n<-nrow(dati)



###Log-trasformation of the output variable. 
set.seed(123)
dati$Target_cost_euro<-log(dati$Target_cost_euro)

###division into train and validation set 90% and 10%
sel<-sample(1:n,size=round(n*0.1,0),replace=F)
train <-dati[-sel,]
validation<-dati[sel,]

set.seed(123)
index<-sample(1:dim(train)[1],0.7*dim(train)[1],replace=F)
new_train<-train[index,]
new_test<-train[-index,]

###First model: Random Forest

library(e1071)
model <- randomForest(Target_cost_euro ~ . ,data=new_train )
predictedY <- predict(model, new_test[,-1])
points(new_test$Target_cost_euro, predictedY, col = "red", pch=4)
error <- mean(abs(exp(new_test$Target_cost_euro) - exp(predictedY)))
error
summary(model)

##prediction from the first model.
predictedY2<- predict(model, test)
prevfin=exp(predictedY2)


model1 <- randomForest(Condition ~ ., data = TrainSet, importance = TRUE)
model1



#####################
##second model: training set all dataset!
set.seed(123)
model <- randomForest(Target_cost_euro ~ . ,data=dati
) #default 500 tree

predictedY <- predict(model, new_test[,-1])
points(new_test$Target_cost_euro, predictedY, col = "red", pch=4)
#our error metric is MAE
error <- mean(abs(exp(new_test$Target_cost_euro) - exp(predictedY)))
error
summary(model)
predictedY2<- predict(model, test)
prevfin=exp(predictedY2)


previsioniensemble=(pred+prevfin)/2
write.csv(prevfin,"previsioniforest2.csv",row.names = F)




# boosting ----------------------------------------------------------------

### PREprocessing for XGB -----------------

all=rbind(dati[,-1],test[,-1])
numericVars <- which(sapply(all, is.numeric))
numericVarNames <- names(numericVars) 
DFnumeric <- all[, names(all) %in% numericVarNames]
DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) ]
cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')
for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}

PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)
DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)
DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)
ZerocolTest <- which(colSums(DFdummies[(nrow(all[,])+1):nrow(all),])==0)
colnames(DFdummies[ZerocolTest])
DFdummies <- DFdummies[,-ZerocolTest]
ZerocolTrain <- which(colSums(DFdummies[1:nrow(all[,]),])==0)
colnames(DFdummies[ZerocolTrain])
DFdummies <- DFdummies[,-ZerocolTrain]
fewOnes <- which(colSums(DFdummies[1:nrow(all[,]),])<10)
colnames(DFdummies[fewOnes])
DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)

combined <- cbind(DFnorm, DFdummies) 
train1 <- combined[-index,]
test1 <- combined[index,]


### Grid search of the hyperparameters

### that grid is the result of multiple tuning iteration. Searching for the optimal 
### combination of hyperparameters.
set.seed(123)
xgb_grid = expand.grid(
  nrounds = 500,
  eta = c(0.1,  0.01),
  max_depth = c(2, 3, 4),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(3 ,5),
  subsample=1
)

xgb_caret <- train(x=combined, y=dati$Target_cost_euro, method='xgbTree', trControl= my_control, tuneGrid=xgb_grid) 
xgb_caret$bestTune
model <- randomForest(Target_cost_euro ~ . ,data=dati)
label_train <-dati$Target_cost_euro

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = as.matrix(combined[c(1:6773),]), label= label_train)
dtest <- xgb.DMatrix(data = as.matrix(combined[c(6774:9677),]))
default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.01, #default = 0.3
  gamma=0,
  max_depth=3, #default=6
  min_child_weight=1, #default=1
  subsample=1,
  colsample_bytree=1,
  ntree=500)

##tuning ot the hyperaprameters
xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 2500, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)
##best iteration found at 897


#train the model using the best iteration found by cross validation
xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 897)
XGBpred <- predict(xgb_mod, dtest)
predictions_XGB <- exp(XGBpred) #need to reverse the log to the real values

head(predictions_XGB)
## [1] 116386.8 162307.3 186494.0 187440.4 187258.3 166241.4


##Writing Prevision
write.table(predictions_XGB,file = "previsionixgb2.csv" ,row.names = FALSE, col.names = FALSE)

model1 <- randomForest(Condition ~ .,ntree=1000 data = TrainSet, importance = TRUE)
model1


#### Lasso prevision

set.seed(27042018)
my_control <-trainControl(method="cv", number=3)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lasso_mod <- train(x=train1, y=dati$Target_cost_euro[index], method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune
min(lasso_mod$results$MAE)

lassoVarImp <- varImp(lasso_mod,scale=F)
lassoImportance <- lassoVarImp$importance

varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))

cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')

LassoPred <- predict(lasso_mod, test1)
predictions_lasso <- exp(LassoPred) #need to reverse the log to the real values
head(predictions_lasso)


pred_lasso=as.integer(predictions_lasso)

mean(abs(predictions_lasso-dati$Target_cost_euro[index]))
write.table(pred_lasso,file = "previsionilasso.csv" ,row.names = FALSE, col.names = FALSE)





### SVM prediction
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(3233)
svm_Linear_Grid <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)], method = "svmLinear",
                         trControl=my_control,
                         tuneGrid = grid,
                         tuneLength = 10)

svm_Linear_Grid$bestTune
#C=0.01

#   C     RMSE       Rsquared   MAE       
# 0.00        NaN        NaN         NaN
# 0.01  0.1299583  0.8971963  0.08474574
# 0.05  0.1304852  0.8958769  0.08447978
# 0.10  0.1306073  0.8956993  0.08477223
# 0.25  0.1308595  0.8953571  0.08495476
# 0.50  0.1308635  0.8953413  0.08493712
# 0.75  0.1307871  0.8954637  0.08494924
# 1.00  0.1307713  0.8954820  0.08488513
# 1.25  0.1308028  0.8954352  0.08491083
# 1.50  0.1307469  0.8955460  0.08491518
# 1.75  0.1308088  0.8954248  0.08490545
# 2.00  0.1308118  0.8954155  0.08494978
# 5.00  0.1307613  0.8955063  0.08492209
plot(svm_Linear_Grid)


SVMpred <- predict(svm_Linear_Grid, test1)
predictions_svm <- exp(SVMpred)


write.table(predictions_svm,file = "previsionisvm.csv" ,row.names = FALSE, col.names = FALSE)


##prev medie 

prevmedie=0.65*predictions_XGB+0.30*predictions_svm+0.05*predictions_lasso
write.table(prevmedie,file = "previsionimedie.csv" ,row.names = FALSE, col.names = FALSE)

#### Final Prediction --------------------------

previsionixgb2 <- read.table("C:/Users/federico/Desktop/Competizione/previsionixgb2.csv", quote="\"", comment.char="")
previsioniforest1200 <- read.csv("C:/Users/federico/Desktop/Competizione/previsioniforest1200.csv", sep="")

prevensemble1200<-previsioniforest1200*0.4+previsionixgb2*0.6
write.csv(prevensemble1200,"previsioniensemble1200.csv",row.names = F)

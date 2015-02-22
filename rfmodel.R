# Initialize
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)

# Read in training data
training <- tbl_df(read.csv("pml-training.csv"))
removeCols <- 1:7
removeCols <- c(removeCols, nearZeroVar(training))
for (i in 1:ncol(training)){
  propNA <- sum(is.na(training[,i]))/nrow(training)
  if (propNA > 0.5) removeCols <- c(removeCols, i)
}
removeCols <- unique(removeCols)
training <- training[-removeCols]


modfit <- randomForest(classe~ .,data=training, importance=TRUE)
testing <- tbl_df(read.csv("pml-testing.csv"))
predict(modfit, testing)
modfit$err.rate[modfit$ntree,1]

imp1 <- importance(modfit, type=1)
impdf1 <- data.frame("feature"=rownames(imp1), 
                    "AccDec"=imp1[,1], row.names=NULL)
impdf1 <- arrange(impdf1, desc(AccDec))
impdf1


imp2 <- importance(modfit, type=2, scale)
impdf2 <- data.frame("feature"=rownames(imp2), 
                     "ImpurDec"=imp2[,1], row.names=NULL)
impdf2 <- arrange(impdf2, desc(ImpurDec))
impdf2

npool <- 20
feats <- intersect(impdf1[1:npool,1], impdf2[1:npool,1])


                 

#modFitTree <- train(classe ~ .,method="rpart",data=training)
#predict(modFitTree, testing)

trainslim <- select(training, 
                    c(which(names(training) %in% feats), ncol(training))
modfit2 <- randomForest(classe~ .,data=trainslim, ntree=100)
predict(modfit2, testing)
modfit2$err.rate[modfit2$ntree,1]

folds <- createFolds(y=trainslim$classe,k=10,
                     list=TRUE,returnTrain=TRUE)
acc <- numeric(0)
for (i in 1:10){
  fld=folds[[i]]
  for (j in 1:3){
    mfit <- randomForest(classe~ .,data=trainslim[fld,], ntree=100)
    pred <- predict(mfit, trainslim[-fld,])
    cm <- confusionMatrix(pred, trainslim[-fld,]$classe )
    acc <- c(acc,1-cm$overall[1])
  }
}

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)

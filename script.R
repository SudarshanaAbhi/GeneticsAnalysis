#pca
library(rgl)
library("FactoMineR")
library("factoextra")
library(ggplot2)
pca_str_8 <- prcomp(top8_str[,-9],scale=TRUE)
comp_str_8 <- data.frame(pca_str_8$x[,1:5])
plot(comp_str_8, pch=16, col=rgb(0,0,0,0.5))
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp_str_8, col=km_str_8$cluster, pch=16)
plot(comp_str_8$PC1,comp_str_8$PC2, col=km_str_8$cluster, pch=16)

library(caTools)
#random forest
library(randomForest)
split_pr <- sample.split(prim_rec_expr_top, SplitRatio = 0.7)
trainData_pr <- subset(prim_rec_expr_top, split == "TRUE")
testData_pr <- subset(prim_rec_expr_top, split == "FALSE")
rf_pr <- randomForest(x = trainData_pr[,-70],y=as.factor(trainData_pr$...72),ntree = 500)
summary(rf_pr)
y_pred_pr = predict(rf_pr, newdata = testData_pr[-70])
confusionMatrix(y_pred_pr,as.factor(testData_pr$...72))

#knn
library(tidyverse)
library(caret)
set.seed(2995)
training.samples_pr <- as.factor(prim_rec_expr_top$...72) %>%
createDataPartition(p = 0.7, list = FALSE)
#trainData_pr_pr  <- prim_rec_expr_top[training.samples_pr, ]
#testData_pr <- prim_rec_expr_top[-training.samples_pr, ]
model_knn_pr <- train(as.factor(...72) ~., data = as.data.frame(trainData_pr), method = "knn",
trControl = trainControl("cv", number = 10),
preProcess = c("center","scale"),
tuneLength = 20
)
plot(model_knn_pr)
model_knn_pr$bestTune
pred_knn_pr <- model_knn_pr %>% predict(testData_pr)
confusionMatrix(pred_knn_pr,as.factor(testData_pr$...72))
model_knn_pr

#svm
library(kernlab)
model_svm_pr <- train(as.factor(...72) ~., data = trainData_pr, method = "svmLinear",
trControl = trainControl("cv", number = 10),
preProcess = c("center","scale")
)
pred_svm_pr <- model_svm_pr %>% predict(testData_pr)
confusionMatrix(pred_svm_pr,as.factor(testData_pr$...72))
#optimize
model_svm_pr_opt <- train((prim_rec_expr_top$...72) ~., data = trainData_pr, method = "svmLinear",
trControl = trainControl("cv", number = 10),tuneGrid = expand.grid(C=seq(0,2,length=20)),
preProcess = c("center","scale")
)
plot(model_svm_pr_opt)
model_svm_pr_opt$bestTune
pred_svm_pr_opt <- model_svm_pr_opt %>% predict(testData_pr)
confusionMatrix(pred_svm_pr_opt,as.factor(testData_pr$...72))

#kmeans
library(cluster)
df_km_pr <- scale(prim_rec_expr_top[,-70])
fviz_nbclust(df_km_pr,kmeans,method="wss")
gap_stat <- clusGap(df_km_pr,
FUN = kmeans,
nstart = 25,
K.max = 10,
B = 50)
km_pr <- kmeans(df_km_pr,centers = 2,nstart = 25)
pred_km_pr <- km_pr %>% predict(testData_pr)
BSS <- km_pr$betweenss
tss <- km_pr$totss
(BSS/tss)*100

#logistic regression 
sample_pr <- sample(c(TRUE, FALSE), nrow(prim_rec_expr_top), 
                 replace=TRUE, prob=c(0.7,0.3))
train <- top_feat_2class_log_reg_filtered[sample, ]
test <- top_feat_2class_log_reg_filtered[!sample, ]
mod_log_pr <- glm(...72~.,data=trainData_pr, family="binomial")
summary(mod_log_pr)
pred_logreg_pr <- predict(mod_log_pr,testData_pr,type = "response")
confusionMatrix(pred_logreg_pr,testData_pr$...72,threshold=optCutOff)

#multinomial regression
library(dplyr)
library(nnet)
multi_mod_8 <- multinom(type ~.,data=train_8)
pred_mn_8 <- predict(multi_mod_8,test_8)
View(test_8)
confusionMatrix(pred_mn_8,test_8$type)

#multinomial regression with pca
multi_mod_8_pca <- multinom(top8_str$type ~ pca_str_8$x[,1] + pca_str_8$x[,2])
pred_mn_8_pca <- predict(multi_mod_8_pca,top8_str$type)
confusionMatrix(pred_mn_8_pca,top8_str$type)
#increase pcs
multi_mod_8_pca <- multinom(top8_str$type ~ pca_str_8$x[,1] + pca_str_8$x[,2] +
                              pca_str_8$x[,3]+pca_str_8$x[,4]+pca_str_8$x[,5])
pred_mn_8_pca <- predict(multi_mod_8_pca,top8_str$type)
confusionMatrix(pred_mn_8_pca,top8_str$type)
#decrease  pcs
multi_mod_8_pca <- multinom(top8_str$type ~ pca_str_8$x[,1] + pca_str_8$x[,2] +
                              pca_str_8$x[,3])
pred_mn_8_pca <- predict(multi_mod_8_pca,top8_str$type)
confusionMatrix(pred_mn_8_pca,top8_str$type)

















###########################################################################################################
##### Juan Pablo Urrutia & Patrick Kaltenbacher #####
#####       Jordan Brand Prediction Project     #####
###########################################################################################################

###########################################################################################################
##### set working directory #####
###########################################################################################################

setwd("/Users/jpurrutia/Desktop/Jordan Brand")

###########################################################################################################
##### Import Libraries #####
###########################################################################################################

library(mosaic)
library(randomForest)
library(dplyr)
library(ISLR)
library(cluster) # for gower similarity and pam
library(pscl)
library(ROCR)
library(tree)
library(gbm)
library(Rtsne) # for t-SNE plot
library(ggplot2) # visualization
library(caret)
library(pROC)
library(e1071)

###########################################################################################################
##### Load Data #####
###########################################################################################################

Jordan <- read.csv("/Users/jpurrutia/Desktop/Jordan Brand/JordanFootballData.csv")
Jordan <- data.frame(Jordan[,-1])

###########################################################################################################
##### Clean data #####
###########################################################################################################

Jordan <- Jordan %>%
  mutate(Q1 = as.factor(Q1),
         Q2 = as.factor(Q2),
         Q9B = as.factor(Q9B))

##### Removing Na's #####

Jordan$Q3A <- ifelse(is.na(Jordan$Q3A),
                     ave(Jordan$Q3A, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q3A)
Jordan$Q3B <- ifelse(is.na(Jordan$Q3B),
                     ave(Jordan$Q3B,FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q3A)
Jordan$Q3C <- ifelse(is.na(Jordan$Q3C),
                     ave(Jordan$Q3C, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q3C)
Jordan$Q3D <- ifelse(is.na(Jordan$Q3D),
                     ave(Jordan$Q3D, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q3D)
Jordan$Q3E <- ifelse(is.na(Jordan$Q3E),
                     ave(Jordan$Q3E, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q3E)
Jordan$Q3F <- ifelse(is.na(Jordan$Q3F),
                     ave(Jordan$Q3F, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q3F)
Jordan$Q3G <- ifelse(is.na(Jordan$Q3G),
                     ave(Jordan$Q3G, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q3G)
Jordan$Q3H <- ifelse(is.na(Jordan$Q3H),
                     ave(Jordan$Q3H, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q3H)
Jordan$Q3I <- ifelse(is.na(Jordan$Q3I),
                     ave(Jordan$Q3I, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q3I)
Jordan$Q4B <- ifelse(is.na(Jordan$Q4B),
                     ave(Jordan$Q4B, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q4B)
Jordan$Q5B <- ifelse(is.na(Jordan$Q5B),
                     ave(Jordan$Q5B, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q5B)
Jordan$Q6 <- ifelse(is.na(Jordan$Q6),
                    ave(Jordan$Q6, FUN = function(x) mean(x,na.rm = TRUE)),
                    Jordan$Q6)
Jordan$Q7 <- ifelse(is.na(Jordan$Q7),
                    ave(Jordan$Q7, FUN = function(x) mean(x,na.rm = TRUE)),
                    Jordan$Q7)
Jordan$Q8 <- ifelse(is.na(Jordan$Q8),
                    ave(Jordan$Q8, FUN = function(x) mean(x,na.rm = TRUE)),
                    Jordan$Q8)
Jordan$Q10B <- ifelse(is.na(Jordan$Q10B),
                      ave(Jordan$Q10B, FUN = function(x) mean(x,na.rm = TRUE)),
                      Jordan$Q10B)
Jordan$Kin1 <- ifelse(is.na(Jordan$Kin1),
                      ave(Jordan$Kin1, FUN = function(x) mean(x,na.rm = TRUE)),
                      Jordan$Kin1)
Jordan$Q12 <- ifelse(is.na(Jordan$Q12),
                     ave(Jordan$Q12, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q12)
Jordan$Q13 <- ifelse(is.na(Jordan$Q13),
                     ave(Jordan$Q13, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q13)
Jordan$Q14 <- ifelse(is.na(Jordan$Q14),
                     ave(Jordan$Q14, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q14)
Jordan$Q15 <- ifelse(is.na(Jordan$Q15),
                     ave(Jordan$Q15, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q15)
Jordan$Q16 <- ifelse(is.na(Jordan$Q16),
                     ave(Jordan$Q16, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q16)
Jordan$Q17 <- ifelse(is.na(Jordan$Q17),
                     ave(Jordan$Q17, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q17)
Jordan$Q18 <- ifelse(is.na(Jordan$Q18),
                     ave(Jordan$Q18, FUN = function(x) mean(x,na.rm = TRUE)),
                     Jordan$Q18)

##########################################################################################
##### Histogram / Boxplot Analysis #####
##########################################################################################

plot(Jordan$Q1, xlab = "Categories of People", ylab = "Frequency", col = "blue")
plot(Jordan$Q2, xlab = "Game Attendance", ylab = "Frequency", col = "blue")
plot(Jordan$Q4B)
plot(Jordan$Q5B)
plot(Jordan$Q6)
plot(Jordan$Q7)
plot(Jordan$Q8)
plot(Jordan$Q9B, main = "Do you intend to purchase (more) Jordan Brand gear in the future?",
     xlab = " No / Yes", ylab = "Frequency", col = "blue")
plot(Jordan$Q10B)
plot(Jordan$Kin1)
plot(Jordan$Q12)
plot(Jordan$Q13)
plot(Jordan$Q14)
plot(Jordan$Q15)
plot(Jordan$Q16)
plot(Jordan$Q17)
plot(Jordan$Q18)

##### Created Fields #####

## plot(Jordan$Amount)
## plot(Jordan$Income)

###########################################################################################################
##### Summary data #####
###########################################################################################################

sapply(Jordan,sd)
sapply(Jordan,mean)
xtabs(~Q9B + Q1, data = Jordan)
xtabs(~Q9B + Q6, data = Jordan)
xtabs(~Q9B + Q7, data = Jordan)
xtabs(~Q9B + Q10B, data = Jordan)



###########################################################################################################
##### Sampling Dataset  #####
###########################################################################################################

View(Jordan)

set.seed(123)

index <- createDataPartition(Jordan$Q9B, p = 0.8,
                             list = F)

set.seed(123)
train <- Jordan[index,]
test <- Jordan[-index,]

###########################################################################################################
##### Logistic Regression  ##### 
###########################################################################################################

set.seed(123)

jord.logit <- glm(Q9B ~ .,
                  family = binomial(link = 'logit'),
                  data = train)

summary(jord.logit)

## analyze the table of deviance
anova(jord.logit, test = "Chisq")

##### Most Significant Variables - Q6, Q3D, Q12 #####

########## CONDENSED MODEL ##########

jord.logit11 <- glm(Q9B ~ Q6+Q12+Q3D,
                     data = train,
                     family = binomial(link = 'logit'))
summary(jord.logit11)

## analyze the table of deviance

anova(jord.logit11, test = "Chisq")

## McFadden R2 index can be used to asses the model fit

pR2(jord.logit11)

########## Assessing the predictive ability of the model ##########

fitted.results <- predict(jord.logit11,
                          newdata = test, type = "response")

fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <-  mean(fitted.results != test$Q9B)

print(paste('Accuracy',1-misClasificError))
## "Accuracy 0.732876712328767"

########## ROC CURVE ##########

p <- predict(jord.logit11,
             newdata = test,
             type = "response")

pr <- prediction(p, test$Q9B)
prf <- performance(pr,measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr,measure = "auc")
auc <- auc@y.values[[1]]
auc
## auc of 0.7789474
## A model with good predictive ability should have an AUC closer to 1 than to 0.5

###########################################################################################################
##### Decision Tree  ##### Classification Tree - Qualitative #####
###########################################################################################################

##### Fitting a tree to the data. Summarize and plot it #####

set.seed(123)
jord.tree <- tree(Q9B ~ ., data = Jordan)
summary(jord.tree)
plot(jord.tree);text(jord.tree,pretty = 0)  
##### Q7 is most important -- Q3A second most -- 14 terminal nodes #####

##### Detailed summary of the tree #####
jord.tree

##### Create tree using training and test datasets #####
##### grow the tree on the training set, and evaluate its performance on the test set #####

jord.tree <- tree(Q9B ~ .,data = train)
plot(jord.tree);text(jord.tree,pretty=0)

##### Predict #####
tree.pred <- predict(jord.tree,test,type = "class")
##### Evaluate error ##### Test Data #####
with(test,table(tree.pred,Q9B))
(32+69)/146    ## Error Rate of 1 - (0.6917808)<-- Accuracy Rate
##### mention that model is more accurate than previous models shown - look for those


##### Prune Tree #####  Cross Validation    -- Big Tree could have too much variance... so prune it
set.seed(123)
cv.jordan <- cv.tree(jord.tree,FUN = prune.misclass)
cv.jordan
plot(cv.jordan)
prune.jordan <- prune.misclass(jord.tree,best = 6) # full training data
plot(prune.jordan);text(prune.jordan,pretty = 0)

##### Evaluating Prune Tree on test data #####

tree.pred <- predict(prune.jordan,test,type = "class")
with(test,table(tree.pred,Q9B))
(32+70)/146
##### 0.6986301

##### It has done about the same as the original tree. Pruning did not hurt missclassification errors and
##### gave a simpler tree -- Q7 is most important -- Q3D second most

###############################################################################
######################### Random Forest (Categorical) and Boosting  ###########
###############################################################################

####################################################################################################
#####Under Sampling#####
####################################################################################################

##### Cross validation... number of iterations...under sampling #####

jord.ctrl <- trainControl(method = 'cv',
                          number = 10, 
                          verboseIter = FALSE,
                          sampling = "down")
##### Model #####
set.seed(1235)
jord_rf_under <- train(Q9B ~ .,
                       data = train,
                       method = "rf",
                       preProcess = c("scale", "center"),
                       trControl = jord.ctrl,
                       model = F)
##### Creating Data Frame with actual ClosedFlag column. Then creating columns with probability of yes or no #####

jord_final_under <- data.frame(actual = test$Q9B,
                               predict(jord_rf_under, newdata = test, type = "prob"))


jord_final_under$predict <- ifelse(jord_final_under$X0 > 0.5, 0, 1)

jord_cm_under <- confusionMatrix(jord_final_under$predict, test$Q9B)
##### 77% Accuracy #####
jord_cm_under

## Accuracy -- 0.739726027

## Error Rate -- 0.260273973

## TP Rate -- 0.813186813

## FP Rate -- 0.381818182

####################################################################################################
#####Over Sampling#####
####################################################################################################
jord.ctrl <- trainControl(method = 'cv',
                          number = 10, 
                          verboseIter = FALSE,
                          sampling = "up")
set.seed(1235)
jord_rf_over <- train(Q9B ~ .,
                      data = train,
                      method = "rf",
                      preProcess = c("scale", "center"),
                      trControl = jord.ctrl,
                      model = F)

jord_final_over <- data.frame(actual = test$Q9B,
                              predict(jord_rf_over, newdata = test, type = "prob"))

jord_final_over$predict <- ifelse(jord_final_over$X0 > 0.5, 0, 1)

jord_cm_over <- confusionMatrix(jord_final_over$predict, test$Q9B)
jord_cm_over

## Accuracy	0.767123288
## Error Rate	0.232876712
## TP Rate	0.858823529
## FP Rate	0.360655738

## Var Imp: 7,8,Kin1,3D, Q6

####################################################################################################
#####SMOTE RF#####
####################################################################################################
sapply(train, function(x) sum(is.na(x)))

jord.ctrl <- trainControl(method = 'cv',
                          number = 10, 
                          verboseIter = FALSE,
                          sampling = "smote",
                          savePredictions = T)

set.seed(1235)
jord_rf_smote <- train(Q9B ~ .,
                       data = train,
                       method = "rf",
                       preProcess = c("scale", "center"),
                       trControl = jord.ctrl,
                       model = F,
                       ntree = 3000)

jord_final_smote <- data.frame(actual = test$Q9B,
                               predict(jord_rf_smote, newdata = test, type = "prob"))

jord_final_smote$predict <- ifelse(jord_final_smote$X0 > 0.5, 0, 1)

jord_cm_smote <- confusionMatrix(jord_final_smote$predict, test$Q9B)

jord_cm_smote

####################################################################################################
#####Model Assessment#####
####################################################################################################
varImp(jord_rf_over, scale = F)

plot(jord_rf_over)

rpartProbs <- predict(jord_rf_over, test, type = 'prob')
colnames(rpartProbs) <- c("Purchased","NotPurchased")

rpartROC <- roc(test$Q9B, rpartProbs[,'Purchased'])
plot(rpartROC, type = 'S',print.thres= .5)

####################################################################################################
#####Predict #####
####################################################################################################
PurchasePrediction <- data.frame(predict(jord_rf_over,newdata = Jordan, type = "prob"))
colnames(PurchasePrediction) <- c("NoPurchaseProbability","PurchaseProbability")

currentResults <- cbind(Jordan, PurchasePrediction)
currentResultsPrint <- currentResults %>%
  mutate(ClosedPurchaseFlag = ifelse(PurchaseProbability > 0.5, 1, 0))

####################################################################################################
#####Output to CSV#####
####################################################################################################
write.csv(currentResultsPrint, file = paste("purchaseprediction.csv"),row.names = FALSE)

# Original  477
# Predicted 457

###########################################################################################################
##### Clustering  ##### 
###########################################################################################################

## calculating distance

gower_dist <- daisy(Jordan,
                    metric = "gower",
                    type = list(logratio = 3))

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

## Output the most similar pair

Jordan[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1,],]

## Output most disimilar pair #### ANALYZE

Jordan[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1,],]

##### Selecting the number of clusters ##### 

## calculate silhouette width for many k using PAM

sil_width <- c(NA)

for (i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

## Plot silhouette width (HIGHER IS BETTER)

plot(1:10, sil_width,
     xlab = "Number of Clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

pam_fit <- pam(gower_dist,diss = TRUE, k = 3)

##### Creating Summaries for cluster characteristics #####

pam_results <- Jordan %>%
  dplyr::select(-Q3A) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

Jordan[pam_fit$metdoids,]


tsne_obj <- Rtsne(gower_dist,is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X","Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = Jordan$Q9B)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))








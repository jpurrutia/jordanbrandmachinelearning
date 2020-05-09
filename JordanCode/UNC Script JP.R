##### Jordan Brand UNC Project #####


#############################################################################################
##### Set working directory #####
#############################################################################################
# setwd("C:/Users/pjkal/Google Drive/Jordan Project/UNC Jordan Project")
# online # setwd("/Users/jpurrutia/Google Drive/Jordan Project/UNC Jordan Project")
setwd("/Users/jpurrutia/Desktop/Jordan Brand")

#############################################################################################
###### Load Packages ######
#############################################################################################
library(tidyverse)
library(stringr)
library(caret)
library(PerformanceAnalytics)
library(knitr)
library(tm)
library(NLP)
library(RODBC)
library(wordcloud)
library(randomForest)
library(tree)
library(pROC)
library(pscl)
library(ROCR)
library(grid)
library(cluster)
library(Rtsne) # for t-SNE plot


#############################################################################################
##### Load Data #####
#############################################################################################
Full_Data <- read_csv("Full Data UNC.csv")
#Full_Data <- as.data.frame(Full_Data)
View(Full_Data)
# summary(Full_Data)
#######################################################################################
##### DATA MANIPULATION #####
#######################################################################################
Full_Data <- Full_Data %>%
  mutate(Q1 = as.factor(Q1),
         Q1a = as.factor(Q1a),
         Q2 = as.factor(Q2),
         Q2a = as.factor(Q2a),
         Q4 = as.factor(Q4),
         Q5 = as.factor(Q5),
         Q6 = as.factor(Q6),
         Q13 = as.factor(Q13),
         Q16  = as.factor(Q16),
         Q17 = as.factor(Q17),
         Q18 = as.factor(Q18),
         Q6log = as.factor(Q6log),
         Q13_A = as.factor(Q13_A),
         Q13_B = as.factor(Q13_B),
         Q13_C = as.factor(Q13_C),
         Q13_D = as.factor(Q13_D),
         Q13_E = as.factor(Q13_E),
         Q13_F = as.factor(Q13_F),
         Q13_G = as.factor(Q13_G),
         Q5_new = as.factor(Q5_new),
         income = as.factor(income))


# Columns to not use 
# 23, 24, 29,31, 32, 33, 34, 35 ,36 ,37,38, 39
mod_data <- Full_Data[,-c(1,2,16,23, 24,29,31, 32, 33, 34, 35 ,36 ,37,38, 39)]
write.csv(mod_data, file = paste("Model Data UNC.csv"),row.names = FALSE)
View(mod_data)
###########################################################################################################
##### Sampling Dataset  #####
###########################################################################################################
set.seed(123)
index <- createDataPartition(mod_data$Q6log, p = 0.6,
                             list = F)

set.seed(123)
train <- mod_data[index,]
test <- mod_data[-index,]
summary(mod_data)

############################################################################################################
##### Logistic Regression  ##### 
###########################################################################################################
set.seed(123)

jord.logit <- glm(Q6log ~ .,
                  family = binomial(link = 'logit'),
                  data = train)

# Warning means that there is a perfect separation... 
summary(jord.logit)

##########################
########## STEP ##########
##########################
## set the seed to make your partition reproductible
set.seed(123)

# Perform the feature selection
step <- stepAIC(jord.logit, direction="both", na.action=na.remove)
step$anova # display results


## analyze the table of deviance
anova(jord.logit, test = "Chisq")

########## CONDENSED MODEL ##########
jord.logit11 <- glm(Q6log ~ Q1a + Q2 + Q3d + Q5 + Q8 + Q11 + Q17 + Q18, # most significant variables from step
                    data = train,
                    family = binomial(link = 'logit'))
summary(jord.logit11)

## analyze the table of deviance
anova(jord.logit11, test = "Chisq")

## McFadden R2 index can be used to asses the model fit
pR2(jord.logit11)
pR2(jord.logit)

########## Assessing the predictive ability of the model ##########

fitted.results <- predict(jord.logit11,
                          newdata = test, type = "response")

fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <-  mean(fitted.results != test$Q6log)

print(paste('Accuracy',1-misClasificError))
## "Accuracy 0.767295597484277"

########## ROC CURVE ##########
p <- predict(jord.logit11,
             newdata = test,
             type = "response")

pr <- prediction(p, test$Q6log)
prf <- performance(pr,measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr,measure = "auc")
auc <- auc@y.values[[1]]
auc
## auc of 0.7949566
## A model with good predictive ability should have an AUC closer to 1 than to 0.5
confusionMatrix(fitted.results,test$Q6log)

#############################################################################################
##### SUPPORT VECTOR CLASSIFIER #####
#############################################################################################
# train method is going to be Radial (non linear). In radial kernel, it needs to select proper
# value of Cost "C" parameter and "sigma" parameter.

tic()
set.seed(123)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(123)

svm_Radial <- train(Q6log ~., data = train, method = "svmRadial",
                    trControl = trctrl,
                    preProcess = c("center","scale"),
                    tuneLength = 10)

toc()
svm_Radial
# Tuning parameter 'sigma' was held constant at a value of 0.01786272
# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were sigma = 0.01594649 and C = 2. 

plot(svm_Radial)
# showing that the final sigma parameter's value is 0.01594649 and C parameter's value as 2
# let's try to test our model's accuracy on our test set. 

test_pred_Radial <- predict(svm_Radial, newdata = test)

confusionMatrix(test_pred_Radial, test$Q6log)


######################### 

# TPR: when a = yes, howoften is p = y (TP/Actual Y(TP+FN)) *85
# FPR: when a = no, how often is p = y (FP/Actual N(FP+TN)) *52
# Specificity: when a = no, how often p = no (TN/Actual N) or (1-TPR)


# we are getting an accuracy of 81.67%. So in this case with values of C=1 & sigma = 0.01786272
# we are getting good results. Let's try to test & tune our classifier with different values
# of sigma and C.
grid_radial <- expand.grid(sigma = c(0,0.01,0.02,0.025,0.03,0.04,
                                     0),
                           C = c(0,0.01,0.05,0.1,0.25,0.5,0.75,1,1.5,2,4,5))
set.seed(123)
svm_Radial_Grid <- train(Q6log ~., data = train, method = "svmRadial",
                         trControl=trctrl,
                         preProcess = c("center","scale"),
                         tuneGrid = grid_radial,              #tuneGrid
                         tuneLength = 10)

svm_Radial_Grid
# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were sigma = 0.02 and C = 0.75.
plot(svm_Radial_Grid)

test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = test)
confusionMatrix(test_pred_Radial_Grid, test$Q6log)

###########################################################################################################
##### Decision Tree  ##### Classification Tree - Qualitative #####
###########################################################################################################

##### Fitting a tree to the data. Summarize and plot it #####
set.seed(123)
jord.tree <- tree(Q6log ~ ., data = mod_data)
summary(jord.tree)
plot(jord.tree);text(jord.tree,pretty = 0)  
##### Q3d is most important -- Q18 second most -- 14 terminal nodes #####

##### Detailed summary of the tree #####
jord.tree

##### Create tree using training and test datasets #####
##### grow the tree on the training set, and evaluate its performance on the test set #####
jord.tree <- tree(Q6log ~ .,data = train)
plot(jord.tree);text(jord.tree,pretty=0)

##### Predict #####
tree.pred <- predict(jord.tree,test,type = "class")
##### Evaluate error ##### Test Data #####
with(test,table(tree.pred,Q6log))
(17+108)/159    ## Error Rate of 1 - (0.7861635)<-- Accuracy Rate

##### Prune Tree #####  Cross Validation -- Big Tree could have too much variance... so prune it
set.seed(123)
cv.jordan <- cv.tree(jord.tree,FUN = prune.misclass)
cv.jordan
plot(cv.jordan)
prune.jordan <- prune.misclass(jord.tree,best = 12) # full training data
plot(prune.jordan);text(prune.jordan,pretty = 0)

##### Evaluating Prune Tree on test data #####
tree.pred <- predict(prune.jordan,test,type = "class")
with(test,table(tree.pred,Q6log))
117/159 # 0.7358491

##### It has done the same as the original tree. Pruning did not hurt missclassification errors and
##### gave a simpler tree -- Q7 is most important -- Q3D second most

############################################################################################
######################### Random Forest (Categorical) and Boosting  ########################
############################################################################################

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
jord_rf_under <- train(Q6log ~ .,
                       data = train,
                       method = "rf",
                       preProcess = c("scale", "center"),
                       trControl = jord.ctrl,
                       model = F)
##### Creating Data Frame with actual ClosedFlag column. Then creating columns with probability of yes or no #####

jord_final_under <- data.frame(actual = test$Q6log,
                               predict(jord_rf_under, newdata = test, type = "prob"))


jord_final_under$predict <- ifelse(jord_final_under$X0 > 0.5, 0, 1)

jord_cm_under <- confusionMatrix(jord_final_under$predict, test$Q6log)
##### 72.5% Accuracy #####
jord_cm_under

## Accuracy -- 0.7799
## Error Rate -- 22
## TP Rate (Sensitivity) -- 0.7561
## FP Rate -- 21%

####################################################################################################
#####Over Sampling#####
####################################################################################################
jord.ctrl <- trainControl(method = 'cv',
                          number = 10, 
                          verboseIter = FALSE,
                          sampling = "up")
set.seed(1235)
jord_rf_over <- train(Q6log ~ .,
                      data = train,
                      method = "rf",
                      preProcess = c("scale", "center"),
                      trControl = jord.ctrl,
                      model = F)

jord_final_over <- data.frame(actual = test$Q6log,
                              predict(jord_rf_over, newdata = test, type = "prob"))

jord_final_over$predict <- ifelse(jord_final_over$X0 > 0.5, 0, 1)

jord_cm_over <- confusionMatrix(jord_final_over$predict, test$Q6log)
jord_cm_over

## Accuracy	0.8239
## Error Rate:	1 - 0.8083
## TP Rate: 0.7317%	(Sensitivity)
## FP Rate: 14.40.%	



## Var Imp: 

# Q3d - excellence 
# Q11 - aware of the UNC / JB partnership
# Q3g - positive impact on UNC football program
# Q3b
# Q3c,a, - brings more value,good fit, 
# Q8 - more likely to donate to UNC b/c of Jordan Brand
# Q7

####################################################################################################
#####Model Assessment#####
####################################################################################################
varImp(jord_rf_over, scale = F)

plot(jord_rf_over)

rpartProbs <- predict(jord_rf_over, test, type = 'prob')
colnames(rpartProbs) <- c("Purchased","NotPurchased")

rpartROC <- roc(test$Q6log, rpartProbs[,'Purchased'])
plot(rpartROC, type = 'S',print.thres= .5)

####################################################################################################
#####Predict #####
####################################################################################################
PurchasePrediction <- data.frame(predict(jord_rf_over,newdata = mod_data, type = "prob"))
colnames(PurchasePrediction) <- c("NoPurchaseProbability","PurchaseProbability")

currentResults <- cbind(mod_data, PurchasePrediction)
currentResultsPrint <- currentResults %>%
  mutate(ClosedPurchaseFlag = ifelse(PurchaseProbability > 0.5, 1, 0))

####################################################################################################
#####Output to CSV#####
####################################################################################################
write.csv(currentResultsPrint, file = paste("purchaseprediction_final.csv"),row.names = FALSE)

# Original  
# Predicted 

###########################################################################################################
##### Clustering  ##### 
###########################################################################################################

## calculating distance

gower_dist <- daisy(mod_data,
                    metric = "gower",
                    type = list(logratio = 3))

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

## Output the most similar pair

mod_data[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1,],]

## Output most disimilar pair #### ANALYZE

mod_data[
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

pam_fit <- pam(gower_dist,diss = TRUE, k = 2)

##### Creating Summaries for cluster characteristics #####

pam_results <- mod_data %>%
  dplyr::select(-Q3e) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

mod_data[pam_fit$metdoids,]


tsne_obj <- Rtsne(gower_dist,is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X","Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = mod_data$Q6log)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


#############################################################################################
##### Correlation Matrix #####
#############################################################################################
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

cor_prep <- cor.prob(Full_Data[sapply(Full_Data, is.numeric)])

matrix <- flattenSquareMatrix(cor_prep)

matrix_HighCor <- subset(matrix, cor >= 0.3 | cor <= -0.3)

kable(matrix_HighCor)
View(matrix_HighCor)

matrix_HighCor <- as.data.frame(matrix_HighCor)

write.csv(matrix_HighCor, file = paste("Corr Table.csv"),row.names = FALSE)

#############################################################################################
###### Internal Consistency #####
#############################################################################################
library(corrr)
View(mod_data)

#########################################################
##### DATA MANIPULATION #####
#########################################################
mod_data <- mod_data[,-c(1,2,3,4,12,13,20,21,22,23,24)]
mod_data %>% correlate()
str(mod_data)

#########################################################
##### AVG INTER-ITEM CORR #####
#########################################################
inter_item <- mod_data %>% correlate() %>% dplyr::select(-rowname) %>% colMeans(na.rm = TRUE)
inter_item
mean(inter_item)

##### Plot #####
data.frame(inter_item) %>%
  ggplot(aes(x = inter_item)) +
  geom_histogram(bins = 10, alpha = .5) +
  geom_vline(xintercept = mean(inter_item), color = "red") +
  xlab("Mean inter-item correlation") +
  theme_bw()
#########################################################
##### Average item-total correlation #####
#########################################################
# investigate average item-total correlation in a similar way to 
# innter-item correlations. First, the total score is calculated.
mod_data$score <- rowMeans(mod_data)
head(mod_data)
# then we correlate everything again, but this time focus on the 
# correlations of the score with the items:
item_total <- mod_data %>% correlate() %>% focus(score)
item_total
mean(item_total$score)
# plot results #
item_total %>%
  ggplot(aes(x = score)) +
  geom_histogram(bins = 10, alpha = .5) +
  geom_vline(xintercept = mean(item_total$score), color = "red") +
  xlab("Mean item-total correlation") +
  theme_bw()

#########################################################
###### Cronbach's Alpha ######
#########################################################
# Cronbach’s alpha is one of the most widely reported measures of internal consistency. 

mod_data$score <- NULL
psych::alpha(mod_data)
# interested in the standardised alpha based upon the correlations.
# we get the "average interitem correlation"

psych::alpha(mod_data)$total$std.alpha
# 0.9092743 ...91%



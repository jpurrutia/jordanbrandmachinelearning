## Load Data

Full_Data <- read_csv("Full Data UNC.csv")
summary(Full_Data)

Full_Data <- Full_Data %>%
  mutate(Q1 = as.factor(Q1),
         Q4 = as.factor(Q4),
         Q6 = as.factor(Q6),
         Q5_new = as.factor(Q5_new),
         income = as.factor(income))

# Columns to not use
# 1, 22, 23, 29, 30
mod_data <- Full_Data[,-c(1,2, 15, 23, 24, 29, 30, 31)]
write.csv(mod_data, "Model Data UNC.csv")



##### GLM - ELASTIC NET #####


# Requires 50/50 train/test split
set.seed(123)
index <- createDataPartition(mod_data$Q6, p = 0.5,
                             list = F)
train <- mod_data[index,]
test <- mod_data[-index,]

# Create new data as matrices
xnet <- model.matrix(Q6 ~ ., mod_data)
ynet <- as.matrix(select(mod_data, Q6))
newtest <- model.matrix(Q6 ~ ., test)

netmodel <- glmnet(xnet, ynet, alpha = 0.5, lambda.min.ratio=0, family = "binomial")
plot(netmodel, xvar = "dev", label = TRUE)
test$Prob_Jord <- predict(netmodel, newx = newtest, type = "class")
test$Pred_Jord <- ifelse(test$Prob_Jord > 0.5, 1, 0)

accuracy(test$Q6, test$Pred_Jord)
# alpha = 0, 78.81%
# alpha = 0.5, 81.30%
# alpha = 1, 81.55%

# Elastic Net - Cross Validation

cv.out <- cv.glmnet(xnet, ynet, alpha = 1, family = "binomial", type.measure = "auc")
# For the type:
# "deviance" uses actual deviance.
# "mae" uses mean absolute error.
# "class" gives misclassification error.
# "auc" (for two-class logistic regression ONLY) gives area under the ROC curve.
plot(cv.out)

# ENCV with s = "lambda.min"
cv.out$lambda.min # 0.02698034
test$Prob_Jord <- predict(cv.out, newx = newtest, s = "lambda.min", type = "response")
test$Pred_Jord <- ifelse(test$Prob_Jord > 0.48, 1, 0)

accuracy(test$Q6, test$Pred_Jord) # alpha = 1, test$Prob_Jord > 0.48, 0.845
confusion.matrix(test$Q6, test$Pred_Jord)
summary(cv.out)

# ENCV with s = "lambda.1se"
cv.out$lambda.1se # 0.0750744
test$Prob_Jord <- predict(cv.out, newx = newtest, s = "lambda.1se", type = "response")
test$Pred_Jord <- ifelse(test$Prob_Jord > 0.62, 1, 0)

accuracy(test$Q6, test$Pred_Jord) # alpha = 1, test$Prob_Jord > 0.56, 0.795
summary(cv.out)

for(i in 2:99){
  p <- i/100
  prob <- ifelse(test$Prob_Jord > p, 1, 0)
  accuracy <- accuracy(test$Q6, prob)
  x[i] <- i/100
  y[i] <- accuracy
}
plot(x,y)

x[which.max(y)]

### NOTES

# Elastic net cross validation proved to yield the highest accuracy (82.59%) at:
# alpha = 1 (LASSO regression)
# test$Prob_Jord > 0.48, optimized using for loop

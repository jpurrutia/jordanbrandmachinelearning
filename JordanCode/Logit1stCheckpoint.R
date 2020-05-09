library(readr)
install.packages("ISLR")
library(ISLR)
#KIN514andSM342Football <- read.csv("~/Desktop/KIN514andSM342Football.csv")
#Jordan <- KIN514andSM342Football

library(readr)
FootballwithoutNAs <- read_csv("~/Desktop/FootballwithoutNAs.csv")
Jordan <- FootballwithoutNAs
#Jordan <- na.omit(FootballwithoutNAs)
View(Jordan)

# double check duplicate errors

#train_ind <-sample(nrow(Default), size = 8000)
#def_train <- Default[train_ind,]
#def_test <- Default[-train_ind,]

model1 <- glm(Jordan$Q7~., data = Jordan, family = binomial(link = "logit"))
summary(model1)
# Q3A, Q3F, Q8, Q9

cor.test(Jordan$Q3A,Jordan$Q3F)
# cor of 0.6429183

cor.test(Jordan$Q3A,Jordan$Q3F)
# cor of 0.6429183

cor.test(Jordan$Q3A,Jordan$Q9)
# cor of 0.3154262

cor.test(Jordan$Q3F,Jordan$Q3A)
# cor of 0.6429183

cor.test(Jordan$Q3F,Jordan$Q8)
# cor of 0.1725144

cor.test(Jordan$Q3F,Jordan$Q9)
# cor of 0.2792814

cor.test(Jordan$Q8,Jordan$Q3A)
# cor of 0.2233245

cor.test(Jordan$Q8,Jordan$Q3F)
# cor of 0.1725144

cor.test(Jordan$Q8,Jordan$Q9)
# cor of 0.2776452

cor.test(Jordan$Q9,Jordan$Q3A)
# cor of 0.3154262

cor.test(Jordan$Q9,Jordan$Q3F)
# cor of 0.2792814

cor.test(Jordan$Q9,Jordan$Q8)
# cor of 0.2776452

?cor
?cor.test

model2 <- glm(Jordan$Q7~Q3A+Q3F+Q8+Q9, data = Jordan, family = binomial(link = "logit"))
# model2 <-glm(default~student+balance, data = def_train, family = binomial(link = "logit"))
summary(model2)

# actual    y     n
# y
# n


contrasts(Default$default)

def_train$predict <-predict(model2, type = "response")
def_test$predict <- predict(model2, newdata = def_test, type = "response")


cont.table.train <- table(pred = def_train$predict > .5, actual = def_train$default)

cont.table.test <- table(pred = def_test$predict > .5, actual = def_test$default)

mean(def_train$predict == def_train$default)

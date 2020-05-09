####################################################################
##### UNC CLEANING SCRIPT #####
####################################################################

#############################################################################################
##### Load Data #####
#############################################################################################
Full_Data <- read_csv("UNC Original Jordan Survey.csv")
#Full_Data <- as.data.frame(Full_Data)
View(Full_Data)

#############################################################################################
### CLEAN DATA ###
#############################################################################################

##### CLEANING FUNCTIONS ####################################################################

# Replacing NA's with mean
na_mean <- function(col) {
  ifelse(is.na(col),
         ave(col, FUN = function(x) mean(x,na.rm = TRUE)),
         (col))
}

# Assign categorical names to the numbers
Q13_Name <- function(col) {
  col <- ifelse(col==1, "Shirts",
                ifelse(col==2, "Shoes/Socks",
                       ifelse(col==3, "Headwear",
                              ifelse(col==4, "Shorts/Pants",
                                     ifelse(col==5, "Accessories",
                                            ifelse(col==6, "Jackets/Weather Gear",
                                                   ifelse(col==7,"Other","none")))))))
}


# Binary Function
bin_func <- function(col) {
  ifelse(col == 2,0,col)
}
#############################################################################################
# Fix incorrect values

# Q1a
#which(Full_Data$Q1a > 2)
Full_Data$Q1a <- ifelse(Full_Data$Q1a > 2, 2, Full_Data$Q1a) # if the response is greater than 2, put 2, else answer
Full_Data$Q1a <- ifelse(is.na(Full_Data$Q1a),2,Full_Data$Q1a)
Full_Data$Q1a <- ifelse(Full_Data$Q1a==2,0,Full_Data$Q1a)
as.data.frame(table(Full_Data$Q1a))
# Var1 Freq
# 1    0  345
# 2    1   55

# Q2
Full_Data$Q2 <- as.factor(Full_Data$Q2)
#summary(Full_Data$Q2)
# Average of column is 2.78516624040921 - Going to convert Na's to 3
Full_Data$Q2[is.na(Full_Data$Q2)] = 3

# Q2a
Full_Data$Q2a <- ifelse(Full_Data$Q2a > 2, 2, Full_Data$Q2a) # if the response is greater than 2, put 2, else answer
Full_Data$Q2a <- ifelse(Full_Data$Q2a == 2,0,Full_Data$Q2a)
Full_Data$Q2a[is.na(Full_Data$Q2a)] <- 0
#summary(Full_Data$Q2a)

# Q3f
summary(Full_Data$Q3f)
#which(is.na(Full_Data$Q3f))
#which(Full_Data$Q3f > 10)
Full_Data$Q3f[133] <- 10 #Full_Data$Q3f[133] <- 10 - selecting row 133 for variable q3f and giving it the value of 10
Full_Data$Q3f <- na_mean(Full_Data$Q3f)
Full_Data$Q3f <- round(Full_Data$Q3f, digits = 0) 

# Q4
#summary(Full_Data$Q4)
#which(is.na(Full_Data$Q4))
Full_Data$Q4[is.na(Full_Data$Q4)] <- 2 #################
Full_Data$Q4 <- bin_func(Full_Data$Q4)
Full_Data$Q4 <- as.factor(Full_Data$Q4)
#summary(Full_Data$Q4)


#summary(Full_Data$Q6)
#Full_Data$Q6 <- as.numeric(Full_Data$Q6)
#Full_Data$Q6 <- as.factor(Full_Data$Q6)
Full_Data$Q6[is.na(Full_Data$Q6)] <- 2
Full_Data$Q6 <- ifelse(Full_Data$Q6 == 5, 2, Full_Data$Q6)
#View(Full_Data)

Full_Data$Q6 <- ifelse(Full_Data$Q6 == 1,1,0)

# Changing Q6 to binary
Full_Data$Q6log <- ifelse(Full_Data$Q6==1, 1, 0)
table(Full_Data$Q6log)
# Q6 has range of 1 and 2 - if the value is equal to 5(error) OR (|) the column average, give it 1, else what it has
View(Full_Data)
# Q15 
summary(Full_Data$Q15)
Full_Data$Q15 <- ifelse(Full_Data$Q15 == 95, 1995, #
                        ifelse(Full_Data$Q15 == 1 | Full_Data$Q15 == 2 | Full_Data$Q15 == 2016, 1976, Full_Data$Q15))

# Q17
summary(Full_Data$Q17)
which(is.na(Full_Data$Q17))
Full_Data$Q17[c(183,78,87,390)] <- 6 # variable Q17 - vector or row values 183,78,87, etc. and assign them value of 6
Full_Data$Q17 <- as.numeric(Full_Data$Q17)
Full_Data$Q17 <- ifelse(Full_Data$Q17==7,8,Full_Data$Q17)
Full_Data$Q17 <- ifelse(Full_Data$Q17==1,8,Full_Data$Q17)

# Q18
summary(Full_Data$Q18)
Full_Data$Q18 <- ifelse(Full_Data$Q18 == 67, 7, Full_Data$Q18)

# Applying NA Mean Function -- Replace NAs of ordinal & numeric data with column averages
Full_Data$Q3a <- na_mean(Full_Data$Q3a)
Full_Data$Q3b <- na_mean(Full_Data$Q3b)
Full_Data$Q3c <- na_mean(Full_Data$Q3c)
Full_Data$Q3d <- na_mean(Full_Data$Q3d)
Full_Data$Q3e <- na_mean(Full_Data$Q3e)
Full_Data$Q3g <- na_mean(Full_Data$Q3g)
Full_Data$Q7 <- na_mean(Full_Data$Q7)
Full_Data$Q8 <- na_mean(Full_Data$Q8)
Full_Data$Q9 <- na_mean(Full_Data$Q9)
Full_Data$Q10 <- na_mean(Full_Data$Q10)
Full_Data$Q11 <- na_mean(Full_Data$Q11)
Full_Data$Q12 <- na_mean(Full_Data$Q12)
Full_Data$Q15 <- na_mean(Full_Data$Q15)
Full_Data$Q18 <- na_mean(Full_Data$Q18)

# Replace other NAs
Full_Data$Q17[is.na(Full_Data$Q17)] = 8
Full_Data$Q17 <- as.numeric(Full_Data$Q17)

# summary(Full_Data$Q6)
# table(Full_Data$Q6) # 74% of Q6 = 1, 21% = 2, 5% = NA
# which(is.na(Full_Data$Q6)) # fill NAs according to non-NA answer distribution
# Full_Data$Q6[c(103,140,176,181,298)] <- 2
# Full_Data$Q6[c(45,61,98,105,107,145,163,173,180,241,289,305,348,401)] <- 1

Full_Data$Q1[is.na(Full_Data$Q1)] <- 6

# Separate Q13 into multiple columns
# library(stringr)
Q13_df_1 <- str_split_fixed(Full_Data$Q13, ",", 2) # taking column Q13 and splitting it by a comma and returning
Q13_df_1
Full_Data$Q13_A <- Q13_df_1[,1]

Q13_df_2 <- str_split_fixed(Q13_df_1[,2], ",", 2)
Q13_df_2
Full_Data$Q13_B <- Q13_df_2[,1]


Q13_df_3 <- str_split_fixed(Q13_df_2[,2], ",", 2)
Full_Data$Q13_C <- Q13_df_3[,1]

Q13_df_4 <- str_split_fixed(Q13_df_3[,2], ",", 2)
Full_Data$Q13_D <- Q13_df_4[,1]

Q13_df_5 <- str_split_fixed(Q13_df_4[,2], ",", 2)
Full_Data$Q13_E <- Q13_df_5[,1]

Q13_df_6 <- str_split_fixed(Q13_df_5[,2], ",", 2)
Full_Data$Q13_F <- Q13_df_6[,1]

Q13_df_7 <- str_split_fixed(Q13_df_6[,2], ",", 2)
Full_Data$Q13_G <- Q13_df_7[,1]



Full_Data$Q13_A <- Q13_Name(Full_Data$Q13_A)
Full_Data$Q13_B <- Q13_Name(Full_Data$Q13_B)
Full_Data$Q13_C <- Q13_Name(Full_Data$Q13_C)
Full_Data$Q13_D <- Q13_Name(Full_Data$Q13_D)
Full_Data$Q13_E <- Q13_Name(Full_Data$Q13_E)
Full_Data$Q13_F <- Q13_Name(Full_Data$Q13_F)
Full_Data$Q13_G <- Q13_Name(Full_Data$Q13_G)


Full_Data$Q5[is.na(Full_Data$Q5)] <- 0
Full_Data$Q5_new <- ifelse(Full_Data$Q5 == 1, "less than 100",
                           ifelse(Full_Data$Q5 == 2, "100-200",
                                  ifelse(Full_Data$Q5 == 3, "200-300",
                                         ifelse(Full_Data$Q5 == 4,"300+","Other/No answer"))))


Full_Data$Q5_new <- as.factor(Full_Data$Q5_new)


Full_Data$income <- ifelse(Full_Data$Q18 == 1, "Under 30k",
                           ifelse(Full_Data$Q18 == 2, "30-50k",
                                  ifelse(Full_Data$Q18 == 3, "50-75k",
                                         ifelse(Full_Data$Q18 == 4, "75-100k",
                                                ifelse(Full_Data$Q18 == 5, "100-150k",
                                                       ifelse(Full_Data$Q18 == 6,"150k+", "Prefer not to answer"))))))

##### Final Cleaning #####
# removing of NA's


which(is.na(Full_Data$Q16))
Full_Data$Q16[is.na(Full_Data$Q16)] <- 4

str(Full_Data$Q4)
View(Full_Data)
#######################################################################################
##### Final Manipulation #####
#######################################################################################
Full_Data$Q3a <- round(Full_Data$Q3a, digits = 0) 
Full_Data$Q3b <- round(Full_Data$Q3b, digits = 0) 
Full_Data$Q3c <- round(Full_Data$Q3c, digits = 0) 
Full_Data$Q3d <- round(Full_Data$Q3d, digits = 0) 
Full_Data$Q3e <- round(Full_Data$Q3e, digits = 0) 
Full_Data$Q3g <- round(Full_Data$Q3g, digits = 0)
#Full_Data$Q4 <- round(Full_Data$Q4, digits = 0)
Full_Data$Q5 <- round(Full_Data$Q5, digits = 0)
Full_Data$Q7 <- round(Full_Data$Q7, digits = 0) 
Full_Data$Q8 <- round(Full_Data$Q8, digits = 0) 
Full_Data$Q9 <- round(Full_Data$Q9, digits = 0) 
Full_Data$Q10 <- round(Full_Data$Q10, digits = 0) 
Full_Data$Q11 <- round(Full_Data$Q11, digits = 0) 
Full_Data$Q12 <- round(Full_Data$Q12, digits = 0) 
Full_Data$Q15 <- round(Full_Data$Q15, digits = 0) 
Full_Data$Q18 <- round(Full_Data$Q18, digits = 0) 

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

summary(Full_Data)
#############################################################################################
##### Dataset for Model Use #####
#############################################################################################
write.csv(Full_Data, file = paste("Full Data UNC.csv"),row.names = FALSE)





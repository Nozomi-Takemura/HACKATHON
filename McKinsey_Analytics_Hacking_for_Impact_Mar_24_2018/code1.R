library(dplyr)
library(readxl)
library(MCMCpack)
library(ggplot2)
install.packages('LiblineaR')
library(LiblineaR)
install.packages('glm')
library(glm)
library("quantmod")
library("reshape2") 
install.packages("glmnet")
library(glmnet)
install.packages("ade4")
library("ade4")
#library(jsonlite)
#library(tidyr)
#TODO: ONE OF THESE PACKAGES OVERWRITES DPLYR::SELECT DANIEL?

data1 <- read_excel("./Desktop/datathon/Hacking_for_impact_data.xlsx", sheet=1)
#[1] "ID_Patient_Care_Situation" "Diagnosed_Condition"       "ID_Patient"                "Treated_with_drugs"       
#[5] "Survived_1_year" 
data2 <- read_excel("./Desktop/datathon/Hacking_for_impact_data.xlsx", sheet=2)
#[1] "Patient_ID"               "Patient_Age"              "Patient_Body_Mass_Index"  "Patient_Smoker"          
#[5] "Patient_Rural_Urban"      "Patient_mental_condition"
data3 <- read_excel("./Desktop/datathon/Hacking_for_impact_data.xlsx", sheet=3)
#"Patient_ID"         "Previous_Condition"
colnames(data1)[1] <- colnames(data3)[1]
colnames(data2)[1] <- colnames(data3)[1]
data <- merge(x = data1, y = data2, by = "Patient_ID", all = TRUE)
data <- merge(x = data1, y = data2, by = "CustomerId", all = TRUE)
summary(data)
# Patient_ID    Diagnosed_Condition   ID_Patient    Treated_with_drugs Survived_1_year   Patient_Age    
# Min.   :    1   Min.   : 0.00       Min.   :    1   Length:41979       Min.   :0.0000   Min.   :  0.00  
# 1st Qu.: 6134   1st Qu.:13.00       1st Qu.: 3160   Class :character   1st Qu.:0.0000   1st Qu.: 16.00  
# Median :12215   Median :27.00       Median : 6254   Mode  :character   Median :1.0000   Median : 33.00  
# Mean   :14316   Mean   :26.51       Mean   : 6262                      Mean   :0.6292   Mean   : 33.05  
# 3rd Qu.:22502   3rd Qu.:40.00       3rd Qu.: 9399                      3rd Qu.:1.0000   3rd Qu.: 50.00  
# Max.   :33014   Max.   :52.00       Max.   :12515                      Max.   :1.0000   Max.   :149.00  
# NA's   :20482   
#  Patient_Body_Mass_Index Patient_Smoker     Patient_Rural_Urban Patient_mental_condition Previous_Condition
#  Min.   : 1.089          Length:41979       Length:41979        Length:41979             Length:41979      
#  1st Qu.:20.206          Class :character   Class :character    Class :character         Class :character  
#  Median :23.370          Mode  :character   Mode  :character    Mode  :character         Mode  :character  
#  Mean   :23.425                                                                                            
#  3rd Qu.:26.707                                                                                            
#  Max.   :30.000                                                                                            
#  NA's   :20482   
#missing rate
sapply(data, function(x) sum(is.na(x))) # 50% of data are missing
# Patient_ID      Diagnosed_Condition               ID_Patient       Treated_with_drugs 
# 0                        0                        0                        0 
# Survived_1_year              Patient_Age  Patient_Body_Mass_Index           Patient_Smoker 3levels
# 0                    20482                    20482                    20482 
# Patient_Rural_Urban Patient_mental_condition       Previous_Condition 
# 20482 2levels                   20482                    21189 
attach(data)
Previous_Condition <- as.factor(Previous_Condition)
Patient_mental_condition <- as.factor(Patient_mental_condition)
Patient_Rural_Urban <- as.factor(Patient_Rural_Urban)
Patient_Smoker <- as.factor(Patient_Smoker)
#maybe a different way exists for the following var..
Treated_with_drugs <- as.factor(Treated_with_drugs)
detach(data)
##Fit a m
----------------------------------------------------------------
#1--remove whole missing data
data.com <- data[complete.cases(data), ]
# > 12977/dim(data.com)[1]
# [1] 0.6241943
#more or less symmetric...?
dim(data.com)
attach(data.com)
data.com$Treated_with_drugs <- as.factor(data.com$Treated_with_drugs)
data.com$Previous_Condition <- as.factor(data.com$Previous_Condition)
data.com$Patient_mental_condition <- as.factor(data.com$Patient_mental_condition)
data.com$Patient_Rural_Urban <- as.factor(data.com$Patient_Rural_Urban)
data.com$Patient_Smoker <- as.factor(data.com$Patient_Smoker)
str(data.com)
detach(data.com)
dummy <- data.com[c("Patient_ID","Patient_Smoker","Patient_Rural_Urban","Patient_mental_condition","Previous_Condition","Treated_with_drugs")]
dummy <- data.com[-c("Patient_Smoker","Patient_Rural_Urban","Patient_mental_condition","Previous_Condition","Treated_with_drugs")]
dummy_names <- c("Patient_Smoker","Patient_Rural_Urban","Patient_mental_condition","Previous_Condition","Treated_with_drugs")
data.num <- data.com[, !(colnames(data.com) %in% dummy_names), drop=FALSE]
dummy <- acm.disjonctif(dummy)
# x_train <- model.matrix( ~ .-1, train[,features])
# lm = cv.glmnet(x=x_train,y = as.factor(train$y), intercept=FALSE ,family =   "binomial", alpha=1, nfolds=7)
# best_lambda <- lm$lambda[which.min(lm$cvm)]
data.com.drop <- cbind(data.num, dummy)
attach(data.com.drop)
data.com.drop$Patient_ID <- NULL
data.com.drop.pre <- data.com.drop
data.com.drop.pre$Survived_1_year <- NULL
LiblineaR(data.com.drop.pre, as.factor(data.com.drop$Survived_1_year), type = 0, cost = 1, epsilon = 0.01, bias = 1, cross = 10)
#LiblineaR(as.matrix(data.com.drop.pre$Patient_Age), as.matrix(data.com.drop$Survived_1_year), type = 0, cost = 1, epsilon = 0.01, bias = 1, cross = 10)
fit1 <- glmnet(as.matrix(data.com.drop.pre), as.matrix(data.com.drop$Survived_1_year), family = "binomial")
fit1_cv10 <- cv.glmnet(as.matrix(data.com.drop.pre), as.matrix(data.com.drop$Survived_1_year), family = "binomial", foldid = 10)

### only part of data
#dummy.f <- data[c("Patient_ID","Patient_Smoker","Patient_Rural_Urban","Patient_mental_condition","Previous_Condition","Treated_with_drugs")]

myglmnet = cv.glmnet(as.matrix(data.com.drop.pre), as.matrix(data.com.drop$Survived_1_year), family = "binomial", type.measure="mae", grouped=FALSE, alpha=1, nfolds=10)
glmnet_hat <- as.numeric(predict(myglmnet, as.matrix(data.com.drop.pre), type="class", s="lambda.min"))
out_train <- as.matrix(data.com.drop$Survived_1_year)
out_predicted <- as.matrix(glmnet_hat)
detach(data.com.drop)

## full data?
dummy.f <- data[c("Patient_ID","Treated_with_drugs")]
mat <- matrix(0, nrow = dim(data)[1], ncol = 7)
frame <- c("DX1", "DX2", "DX3", "DX4" ,"DX5","DX6", "NA")

for (i in 1:dim(dummy.f)[1]){
  a <- as.vector(strsplit(dummy.f$Treated_with_drugs[i], " ")[[1]])
  for (j in 1:length(a)){
    mat[i,as.numeric(which(a[j] == frame))] <- 1
  }
}
mat.id <- cbind(data$Patient_ID, data$Survived_1_year, mat)
mat.id <- subset(mat.id, c(mat.id[,9] == 0))
#1stcol: id
#2ndcol: binary out
myglmnet_2 = cv.glmnet(as.matrix(mat.id[,3:8]), as.matrix(mat.id[,2]), family = "binomial", type.measure="mae", grouped=FALSE, alpha=1, nfolds=10)
glmnet_hat_2 <- as.numeric(predict(myglmnet, as.matrix(data.com.drop.pre), type="class", s="lambda.min"))
out_train_2 <- as.vector(mat.id[,2])
out_predicted_2 <- as.vector(glmnet_hat_2)
mean(out_train_2 == out_predicted_2)

dummy.f$Treated_with_drugs[1]
dummy.f <- acm.disjonctif(dummy.f)

#####################
test################
test <- read.csv("./Desktop/datathon/Validation.csv", header = TRUE, ',')
test.d <- test[c("Diagnosed_Condition","ID_Patient","Patient_Age","Patient_Body_Mass_Index")]
test.d <- cbind(test.d, dummy.t)

input <- matrix(0, ncol = dim(data.com.drop.pre)[2], nrow = 15)
input[,1] <- test[,2]
input[,2] <- test[,3]

replace_imputed <- function(original, imputed){
  
  namestoChange <- colnames(original)[colnames(imputed) %in% colnames(original)]
  
  for(i in 1:length(namestoChange)){
    original[namestoChange[i]] <- imputed[namestoChange[i]]
  }
  return(original)
  
}
(replace_imputed(input,test.d))


for (i in 1:dim(data.com.drop.pre)[2]){
  for (j in dim(test.d)[2]){
    if (colnames(test.d[j]) == colnames(data.com.drop.pre[i])){
      input[,j] <- test.d[,j]
      }
  }
}


glmnet_hat_test <- as.numeric(predict(myglmnet, as.matrix(input), type="class", s="lambda.min"))
out_train_test <- as.matrix(data.com.drop$Survived_1_year)
out_predicted_test <- as.matrix(glmnet_hat)
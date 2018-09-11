
#Problem Statement: 


#The client, a financial service institution, want to increase revenue streams and intents to target a segment of their customers who are most likely to default on the loans/Credit taken. 

#------------------------------Preparing the environment for Logistic Regression---------------------------------------#

list.of.packages <- c("caret", "ggplot2","MASS","car","mlogit","caTools","sqldf","Hmisc","aod","BaylorEdPsych","ResourceSelection","pROC","ROCR")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(aod)
library(BaylorEdPsych)
library(ResourceSelection)
library(pROC)
library(ROCR)
library(caTools)

#--------------------------------Setting the Working Directory-----------------------------------------#
Path<-"C:/Users/win10/Downloads/shubham data/IVY_Logistic Regression_Case Study"

setwd(Path)
getwd()


data<-read.csv("Data_for_Logistic_Regression.csv",header = TRUE)
data1=data#To create a backup of original data
head(data1)


#------------------------------------Basic Exploration of the data--------------------------------------------# 
str(data1)
summary(data1)
dim(data1)
data1$Default_On_Payment<-as.factor(data1$Default_On_Payment)
str(data1)


#-----------------------------------Missing Value Treatment (if any)-------------------------------------------#
data.frame(colSums(is.na(data1)))  # No missing values


#--------------------------------Information Value Calculation (A variable reduction technique)----------------------------------#

#-----------> Creating two data sets for numeric and categorical values

## Data set with numeric variable
cat <- data1[,c(2,4:5,7:8,10,11,13,15,16,18,20:22)]# Categorical Data Frame
num <- data1[,-c(2,4:5,7:8,10,11,13,15,16,18,20:21)]# Numerical Data Frame
head(cat)
head(num)
str(num)
str(cat)


#---------------------------------------IV for numeric data-------------------------------------------------------#


IVCal <- function(variable, target,data,groups)
{
  data[,"rank"] <- cut2(data[,variable],g=groups)
  tableOutput <-sqldf(sprintf("select rank, 
                              count(%s) n,
                              sum(%s) good
                              from data 
                              group by rank",target,target))
  tableOutput <- sqldf("select *,
                       (n - good) bad
                       from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)*100
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)*100
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))*100
  tableOutput$IV <- (log(tableOutput$good_rate/tableOutput$bad_rate))*(tableOutput$good_rate-tableOutput$bad_rate)/100
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

a1<- IVCal("Customer_ID","Default_On_Payment",num,groups=10)
a2<- IVCal("Duration_in_Months","Default_On_Payment",num,groups=10)
a3<- IVCal("Credit_Amount","Default_On_Payment",num,groups=10)
a4<- IVCal("Inst_Rt_Income","Default_On_Payment",num,groups=10)
a5<- IVCal("Current_Address_Yrs","Default_On_Payment",num,groups=10)
a6<- IVCal("Age","Default_On_Payment",num,groups=10)
a7<- IVCal("Num_CC","Default_On_Payment",num,groups=10)
a8<- IVCal("Dependents","Default_On_Payment",num,groups=10)
a9<- IVCal("Count","Default_On_Payment",num,groups=10)


IV_num<- data.frame(rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9))
IV_num



#-------------------------------------Information Value for categorical data----------------------------------------------------------#

CA <- function(target, variable, data) {
  A1<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")
  
  A1<- fn$sqldf("select *, (n-good) bad from A1")
  A1$bad_rate <- A1$bad/sum(A1$bad)*100
  
  A1$good_rate<- A1$good/sum(A1$good)*100
  A1$WOE<- (log(A1$good_rate/A1$bad_rate))*100
  A1$IV <- (log(A1$good_rate/A1$bad_rate))*(A1$good_rate-A1$bad_rate)/100
  IV <- sum(A1$IV[is.finite(A1$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}
A<- CA("Default_On_Payment","Status_Checking_Acc",cat)
B<- CA("Default_On_Payment","Credit_History",cat)
C<- CA("Default_On_Payment","Purposre_Credit_Taken",cat)
D<- CA("Default_On_Payment","Savings_Acc",cat)
E<- CA("Default_On_Payment","Years_At_Present_Employment",cat)
F<- CA("Default_On_Payment","Marital_Status_Gender",cat)

G<- CA("Default_On_Payment","Other_Debtors_Guarantors",cat)
H<- CA("Default_On_Payment","Property",cat)
I<- CA("Default_On_Payment","Other_Inst_Plans",cat)
J<- CA("Default_On_Payment","Housing",cat)
K<- CA("Default_On_Payment","Job",cat)
L<- CA("Default_On_Payment","Telephone",cat)
M<- CA("Default_On_Payment","Foreign_Worker",cat)




IV_cat<- data.frame(rbind(A,B,C,D,E,F,G,H,I,J,K,L,M))
IV_cat
Final_IV <- data.frame(rbind(IV_num,IV_cat))
Final_IV

write.csv(Final_IV,"Final_IV.csv")


########################################################### IV Ends here ##############################################

#--------------------------Splitting the data into training and test data set------------------------#

set.seed(100)#This is used to produce reproducible results, everytime we run the model

spl = sample.split(data1$Default_On_Payment, 0.7)
data.train = subset(data1, spl == TRUE)
str(data.train)
dim(data.train)


data.test = subset(data1, spl == FALSE)
str(data.test)
dim(data.test)


#-------------------------------------Logistic Regression Model Building------------------------------------------#

#Iteration 1

model1 <- glm(Default_On_Payment~., data=data.train, family=binomial())
View(model)
summary(model1)

#Iteration 2

model2 <- glm(Default_On_Payment~ Status_Checking_Acc+ Duration_in_Months+ Credit_History +Purposre_Credit_Taken +Credit_Amount
             +Savings_Acc +Years_At_Present_Employment +Inst_Rt_Income +Marital_Status_Gender + Other_Debtors_Guarantors 
            +Property +Age +Other_Inst_Plans + Housing + Num_CC +Job + Telephone + Foreign_Worker + Count, data=data.train, family=binomial())

summary(model2)


#Iteration 3

model3 <- glm(Default_On_Payment~ Status_Checking_Acc+ Duration_in_Months+ Credit_History +Purposre_Credit_Taken +Credit_Amount
             +Savings_Acc +Years_At_Present_Employment +Inst_Rt_Income +Marital_Status_Gender + Other_Debtors_Guarantors 
             +Property +Age +Other_Inst_Plans + Housing + Num_CC + Telephone + Foreign_Worker + Count, data=data.train, family=binomial())

summary(model3)

#Iteration 4

model4 <- glm(Default_On_Payment~ Status_Checking_Acc+ Duration_in_Months+ I(Credit_History == "A32") + I(Credit_History == "A33") + I(Credit_History == "A34") +Purposre_Credit_Taken +Credit_Amount
             +Savings_Acc + I(Years_At_Present_Employment == "A74") +Inst_Rt_Income +Marital_Status_Gender + Other_Debtors_Guarantors 
             +Property +Age +Other_Inst_Plans + Housing + Num_CC + Telephone + Foreign_Worker + Count, data=data.train, family=binomial())

summary(model4)

#Iteration 5

model5 <- glm(Default_On_Payment~ Status_Checking_Acc+ Duration_in_Months+ I(Credit_History == "A32") + I(Credit_History == "A33") + I(Credit_History == "A34")
             +I(Purposre_Credit_Taken == "A41") +I(Purposre_Credit_Taken == "A410") + I(Purposre_Credit_Taken == "A42") + I(Purposre_Credit_Taken == "A43") + I(Purposre_Credit_Taken == "A48") +I(Purposre_Credit_Taken == "A49")
             +Credit_Amount
             +Savings_Acc + I(Years_At_Present_Employment == "A74") +Inst_Rt_Income +Marital_Status_Gender + Other_Debtors_Guarantors 
             +Property +Age +Other_Inst_Plans + Housing + Num_CC + Telephone + Foreign_Worker + Count, data=data.train, family=binomial())

summary(model5)

#Iteration 6

model6 <- glm(Default_On_Payment~ Status_Checking_Acc+ Duration_in_Months+ I(Credit_History == "A32") + I(Credit_History == "A33") + I(Credit_History == "A34")
             +I(Purposre_Credit_Taken == "A41") +I(Purposre_Credit_Taken == "A410") + I(Purposre_Credit_Taken == "A42") + I(Purposre_Credit_Taken == "A43") + I(Purposre_Credit_Taken == "A48") +I(Purposre_Credit_Taken == "A49")
             +Credit_Amount + I(Savings_Acc == "A62")+I(Savings_Acc == "A64") + I(Savings_Acc == "A65")
             + I(Years_At_Present_Employment == "A74") +Inst_Rt_Income + I(Marital_Status_Gender == "A92") + I(Marital_Status_Gender == "A93") + Other_Debtors_Guarantors 
             +Property +Age +Other_Inst_Plans + Housing + Num_CC + Telephone + Foreign_Worker + Count, data=data.train, family=binomial())

summary(model6)

#Iteration 7

model7 <- glm(Default_On_Payment~ Status_Checking_Acc+ Duration_in_Months+ I(Credit_History == "A32") + I(Credit_History == "A33") + I(Credit_History == "A34")
             +I(Purposre_Credit_Taken == "A41") +I(Purposre_Credit_Taken == "A410") + I(Purposre_Credit_Taken == "A42") + I(Purposre_Credit_Taken == "A43") + I(Purposre_Credit_Taken == "A48") +I(Purposre_Credit_Taken == "A49")
             +Credit_Amount + I(Savings_Acc == "A62")+I(Savings_Acc == "A64") + I(Savings_Acc == "A65")
             + I(Years_At_Present_Employment == "A74") +Inst_Rt_Income + I(Marital_Status_Gender == "A93") + Other_Debtors_Guarantors 
             +Property +Age + I(Other_Inst_Plans == "A143") + Housing + Num_CC + Telephone + Foreign_Worker + Count, data=data.train, family=binomial())

summary(model7)

#Iteration 8

model8 <- glm(Default_On_Payment~ Status_Checking_Acc+ Duration_in_Months+ I(Credit_History == "A32") + I(Credit_History == "A33") + I(Credit_History == "A34")
              +I(Purposre_Credit_Taken == "A41") +I(Purposre_Credit_Taken == "A410") + I(Purposre_Credit_Taken == "A42") + I(Purposre_Credit_Taken == "A43") + I(Purposre_Credit_Taken == "A48") +I(Purposre_Credit_Taken == "A49")
              +Credit_Amount + I(Savings_Acc == "A62")+I(Savings_Acc == "A64") + I(Savings_Acc == "A65")
              + I(Years_At_Present_Employment == "A74") +Inst_Rt_Income + I(Marital_Status_Gender == "A93") + Other_Debtors_Guarantors 
              +Property +Age + I(Other_Inst_Plans == "A143") + Housing + Num_CC + Telephone + Foreign_Worker, data=data.train, family=binomial())

summary(model8)



#vif test
vif(model8)




#------------------------------Checking the overall fitness of the model----------------------------#


#--------------->using Wald Test
wald.test(b=coef(model8), Sigma= vcov(model8), Terms=1:32)#Here Terms, no. of independent variables in your final train model
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0


#------------------->Lagrange Multiplier or Score Test (Assess wether the current variable 
#significantly improves the model fit or not)


# Difference betweene null deviance and deviance
modelChi <- model8$null.deviance - model8$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- model8$df.null - model8$df.residual
chidf


# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)



#------------------------------------Predicting power of the model using R2----------------------------#
PseudoR2(model8)


#--------------------Lackfit Deviance for assessing wether the model where
#Ho: Observed Frequencies/probabilties =Expected FRequencies/probabilties ----------------------------------------#
residuals(model8) # deviance residuals
residuals(model8, "pearson") # pearson residuals

sum(residuals(model8, type = "pearson")^2)
deviance(model8)

#########Larger p value indicate good model fit
1-pchisq(deviance(model8), df.residual(model8))

# P value is 0.9999417. Thus, we accept the Null Hypthesis Ho that Observed Frequencies = Expected Frequencies





#####################################################################################################################
# Coefficients (Odds)
model8$coefficients
# Coefficients (Odds Ratio)
exp(model8$coefficients)


# Variable Importance of the model
varImp(model8)

# Predicted Probabilities
prediction <- predict(model8,newdata = data.train,type="response")
prediction

write.csv(prediction,"pred.csv")


rocCurve   <- roc(response = data.train$Default_On_Payment, predictor = prediction, 
                  levels = rev(levels(data.train$Default_On_Payment)))
data.train$Default_On_Payment <- as.factor(data.train$Default_On_Payment)

#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.train$Default_On_Payment)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)


#===================================Testing On Test Dataset ==================================

# Iteration 1
test1 <- glm(Default_On_Payment~ Status_Checking_Acc+ Duration_in_Months+ I(Credit_History == "A32") + I(Credit_History == "A33") + I(Credit_History == "A34")
             +I(Purposre_Credit_Taken == "A41") +I(Purposre_Credit_Taken == "A410") + I(Purposre_Credit_Taken == "A42") + I(Purposre_Credit_Taken == "A43") + I(Purposre_Credit_Taken == "A48") +I(Purposre_Credit_Taken == "A49")
             +Credit_Amount + I(Savings_Acc == "A62")+I(Savings_Acc == "A64") + I(Savings_Acc == "A65")
             + I(Years_At_Present_Employment == "A74") +Inst_Rt_Income + I(Marital_Status_Gender == "A93") + Other_Debtors_Guarantors 
             +Property +Age + I(Other_Inst_Plans == "A143") + Housing + Num_CC + Telephone + Foreign_Worker, data=data.test, family=binomial())

summary(test1)

# Iteration 2

test2 <- glm(Default_On_Payment~ Status_Checking_Acc+ Duration_in_Months+ I(Credit_History == "A32") + I(Credit_History == "A33") + I(Credit_History == "A34")
             +I(Purposre_Credit_Taken == "A41") +I(Purposre_Credit_Taken == "A410") + I(Purposre_Credit_Taken == "A42") + I(Purposre_Credit_Taken == "A43") + I(Purposre_Credit_Taken == "A48") +I(Purposre_Credit_Taken == "A49")
             +Credit_Amount +I(Savings_Acc == "A64") + I(Savings_Acc == "A65")
             + I(Years_At_Present_Employment == "A74") +Inst_Rt_Income + I(Marital_Status_Gender == "A93") + Other_Debtors_Guarantors 
             +Property +Age + I(Other_Inst_Plans == "A143") + Housing+ Num_CC + Telephone + Foreign_Worker, data=data.train, family=binomial())

summary(test2)

# Iteration 3

test3 <- glm(Default_On_Payment~ Status_Checking_Acc+ Duration_in_Months+ I(Credit_History == "A32") + I(Credit_History == "A33") + I(Credit_History == "A34")
             +I(Purposre_Credit_Taken == "A41") +I(Purposre_Credit_Taken == "A410") + I(Purposre_Credit_Taken == "A42") + I(Purposre_Credit_Taken == "A43") + I(Purposre_Credit_Taken == "A48") +I(Purposre_Credit_Taken == "A49")
             +Credit_Amount +I(Savings_Acc == "A64") + I(Savings_Acc == "A65")
             + I(Years_At_Present_Employment == "A74") +Inst_Rt_Income + I(Marital_Status_Gender == "A93") + Other_Debtors_Guarantors 
             +I(Property == "A122") +I(Property == "A124") +Age + I(Other_Inst_Plans == "A143") + Housing+ Num_CC + Telephone + Foreign_Worker, data=data.train, family=binomial())

summary(test3)


vif(test3)




#------------------------------Checking the overall fitness of the model----------------------------#


#--------------->using Wald Test
wald.test(b=coef(test3), Sigma= vcov(test3), Terms=1:30)#Here Terms, no. of independent variables in your final train model
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0


#------------------->Lagrange Multiplier or Score Test (Assess wether the current variable 
#significantly improves the model fit or not)


# Difference betweene null deviance and deviance
modelChi <- test3$null.deviance - test3$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- test3$df.null - test3$df.residual
chidf


# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)



#------------------------------------Predicting power of the model using R2----------------------------#
PseudoR2(test3)


#--------------------Lackfit Deviance for assessing wether the model where
#Ho: Observed Frequencies/probabilties =Expected FRequencies/probabilties ----------------------------------------#
residuals(test3) # deviance residuals
residuals(test3, "pearson") # pearson residuals

sum(residuals(test3, type = "pearson")^2)
deviance(test3)

#########Larger p value indicate good model fit
1-pchisq(deviance(test3), df.residual(test3))

# P value is 0.9999235. Thus, we accept the Null Hypthesis Ho that Observed Frequencies = Expected Frequencies





#####################################################################################################################
# Coefficients (Odds)
test3$coefficients
# Coefficients (Odds Ratio)
exp(test3$coefficients)


# Variable Importance of the model
varImp(test3)

# Predicted Probabilities
prediction <- predict(test3,newdata = data.test,type="response")
prediction

write.csv(prediction,"pred.csv")


rocCurve   <- roc(response = data.test$Default_On_Payment, predictor = prediction, 
                  levels = rev(levels(data.test$Default_On_Payment)))


#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.test$Default_On_Payment)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)


#Final model which satisfies all test and can be used in unseen data
FinalModel <- glm(Default_On_Payment~ Status_Checking_Acc+ Duration_in_Months+ I(Credit_History == "A32") + I(Credit_History == "A33") + I(Credit_History == "A34")
             +I(Purposre_Credit_Taken == "A41") +I(Purposre_Credit_Taken == "A410") + I(Purposre_Credit_Taken == "A42") + I(Purposre_Credit_Taken == "A43") + I(Purposre_Credit_Taken == "A48") +I(Purposre_Credit_Taken == "A49")
             +Credit_Amount +I(Savings_Acc == "A64") + I(Savings_Acc == "A65")
             + I(Years_At_Present_Employment == "A74") +Inst_Rt_Income + I(Marital_Status_Gender == "A93") + Other_Debtors_Guarantors 
             +I(Property == "A122") +I(Property == "A124") +Age + I(Other_Inst_Plans == "A143") + Housing+ Num_CC + Telephone + Foreign_Worker, data=data.train, family=binomial())


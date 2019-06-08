#UpGrad - PGDDS - Capstone BFSI Project


#Group members:
#1. Sharath Athrey
#2. Sumit Kaushik
#3. Vandhana Shri


library(Information)
library(ggplot2)
library(dplyr)
library(caret)
library(car)
library(MASS)
library(woeBinning)
library(e1071)
library(caTools)
library(ROCR)



#Data Understanding / Preparation / Cleaning


# Importing the two CSV files (Demographics and Credit Bureau)

DG_Data <- read.csv("Demographic data.csv")
CB_Data <- read.csv("Credit Bureau data.csv")


# Getting Data Dimesnions (Observations and Variables)

dim(DG_Data)     #71295 obs. of  12 variables
dim(CB_Data)     #71295 obs. of  19 variables

# Independent Variables : 18, Dependent Variable : 1, Data Type : integer
str(CB_Data)     

#  Independent Variables : 11, Dependent Variable : 1, Data Types : integer, categorical
str(DG_Data)


# Checking if the key field - Application ID is unique
sapply(list(CB_Data,DG_Data),function(x) sum(!duplicated(x$Application.ID)))

#Observations on the Original data - 71295
#Observations after identifying duplicates - 71292

CB_Data[duplicated(CB_Data$Application.ID),] #765011468 - 653287861 - 671989187
DG_Data[duplicated(DG_Data$Application.ID),] #765011468 - 653287861 - 671989187


#Excluding the duplicate application ID entires - on both data sets
CB_Data <- CB_Data[!CB_Data$Application.ID %in% c(765011468, 653287861, 671989187),]
DG_Data <- DG_Data[!DG_Data$Application.ID %in% c(765011468, 653287861, 671989187),]


# Data Quality Check


# Check for NA's in the data
sum(is.na(CB_Data))
sum(is.na(DG_Data))

#Exluding missing entries
CB_Data<-CB_Data[!(is.na(CB_Data$Performance.Tag)),]
DG_Data<-DG_Data[!(is.na(DG_Data$Performance.Tag)),]

nrow(CB_Data)
nrow(DG_Data)
# Final cleaned up observations - 69864 obs

# Loading ggplot2 library
library(ggplot2)

# Plotting Age histogram
ggplot(DG_Data,aes(Age))+geom_histogram()

# Let's check the outlier in the variables 

quantile(DG_Data$Age,seq(0,1,0.01))

# Box plot 
summary(DG_Data$Age)
boxplot(DG_Data$Age)

# Capping the lower values of age with 18.

DG_Data[(which(DG_Data$Age<18)),]$Age <- 18


# Binning the age variable and store it into "binning.age".

DG_Data$binning.age <- as.factor(cut(DG_Data$Age, breaks = c(18, 20, 30, 40, 50, 60, 70)))
# 
# # Check the numeric value of response rate in each bucket
# 
agg_age <- merge(aggregate(Performance.Tag ~ binning.age, DG_Data, mean),aggregate(Performance.Tag~binning.age, DG_Data, sum),by = "binning.age") 
# 
# # Adding No.of_prospect
count <- data.frame(table(DG_Data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)
# 
# 
# # changing column name of each variables in agg_age dataframe
# 
colnames(agg_age) <- c("age", "performance", "count_prospects","No.of_prospect")
# 
# # Round Off the values
# 
agg_age$performance<- format(round(agg_age$performance, 3))
# 
agg_age
# 
# #-------------------------------------------------------
# 
# # Let's see the response rate of each age bucket in the plot
# 
ggplot(agg_age, aes(age, No.of_prospect,label = performance)) + 
geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
geom_text(size = 3, vjust = -0.5)
# 

##--------------------------------------------------------  

# Checking structure of dataset

str(DG_Data)

#-----Next Variable is "Gender"

# Checking the levels of the job

levels(DG_Data$Gender)

# Let's replace Unknown level to M

levels(DG_Data$Gender)[1] <- "M"
#Remove rows with spaces
#DG_Data[(which(DG_Data$Gender=="")),]$Gender <- "M"

# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable

plot_performance <- function(cat_var, var_name){
  a <- aggregate(Performance.Tag ~ cat_var, DG_Data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_performance <- cbind(a, count)
  
  colnames(agg_performance) <- c(var_name, "Performance","No.of_Prospect")
  agg_performance[, 2] <- format(round(agg_performance[, 2], 3))
  
  ggplot(agg_performance, aes(agg_performance[, 1], count, label = Performance)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}
 
plot_performance(DG_Data$Gender,"Gender")

##--------------------------------------------------------  

# Checking structure of dataset 

str(DG_Data)

# Checking Marital status

summary(DG_Data$Marital.Status..at.the.time.of.application.)

# Let's replace Unknown level to married

levels(DG_Data$Marital.Status..at.the.time.of.application.)[1] <- "Married"

# Plotting marital status

plot_performance(DG_Data$Marital.Status..at.the.time.of.application.,"marital")


# No. of dependents
boxplot(DG_Data$No.of.dependents)
summary(DG_Data$No.of.dependents)
plot_performance(DG_Data$No.of.dependents,"No.of.dependents")


#Income
boxplot(DG_Data$Income)
summary(DG_Data$Income)

# So let's check the percentile distribution of income 

quantile(DG_Data$Income,seq(0,1,0.01))

# Binning the income variable and store it into "binning.income".

DG_Data$binning.income <- as.factor(cut(DG_Data$Income, breaks = c(10, 20, 30, 40, 50, 60)))


plot_performance(DG_Data$binning.income,"Income")

# Let's see the education variables

levels(DG_Data$Education)



# Reducing the levels of education variable, change spaces and others to uneducated

levels(DG_Data$Education)[c(1,4)] <- "Uneducated"

# # Let's again check the education plot
# 
plot_performance(DG_Data$Education,"Education_levels")

#Check Profession
levels(DG_Data$Profession)

levels(DG_Data$Profession)[1] <- "SAL"

# Let's again check the profession plot
# 
plot_performance(DG_Data$Profession,"Profession")

# Check Type of residence

levels(DG_Data$Type.of.residence)

#Change spaces and Others to Rented

levels(DG_Data$Type.of.residence)[c(1,4)] <- "Rented"

# Let's again check the profession plot
# 
plot_performance(DG_Data$Type.of.residence,"Type.of.residence")

# check no of months in current residence


boxplot(DG_Data$No.of.months.in.current.residence)

quantile(DG_Data$No.of.months.in.current.residence,seq(0,1,0.01))

#Fixing outliers
x <- DG_Data$No.of.months.in.current.residence
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
DG_Data$No.of.months.in.current.residence <- x

# Binning the No.of.months.in.current.residence and store it into "binning.residence".

DG_Data$binning.residence <- as.factor(cut(DG_Data$No.of.months.in.current.residence, breaks = c(1, 12, 24, 48, 60, 90, 120)))


plot_performance(DG_Data$binning.residence,"No. of months in residence")


# check no of months in current company


boxplot(DG_Data$No.of.months.in.current.company)

quantile(DG_Data$No.of.months.in.current.company,seq(0,1,0.01))

#Fixing outliers
x <- DG_Data$No.of.months.in.current.company
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
DG_Data$No.of.months.in.current.company <- x

# Binning the No.of.months.in.current.company and store it into "binning.company".

DG_Data$binning.company <- as.factor(cut(DG_Data$No.of.months.in.current.company, breaks = c(0, 20, 40, 60, 80)))


plot_performance(DG_Data$binning.company,"No. of months in company")

#-------------------------------------------------------

# EDA of the credit data. 
#-------------------------------------------------------

# check No.of.times.90.DPD.or.worse.in.last.6.months
sum(is.na(CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months))

# Writing a function "plot_cb_performance" to do the same task for each variable

plot_cb_performance <- function(cat_var, var_name){
  a <- aggregate(Performance.Tag ~ cat_var, CB_Data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_performance <- cbind(a, count)
  
  colnames(agg_performance) <- c(var_name, "Performance","No.of_Prospect")
  agg_performance[, 2] <- format(round(agg_performance[, 2], 3))
  
  ggplot(agg_performance, aes(agg_performance[, 1], count, label = Performance)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}
summary(CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months)

plot_cb_performance(CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months,"No.of.times.90.DPD.or.worse.in.last.6.months")

# check No.of.times.60.DPD.or.worse.in.last.6.months
sum(is.na(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months))
summary(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months)
boxplot(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months)

plot_cb_performance(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months,"No.of.times.60.DPD.or.worse.in.last.6.months")


# check No.of.times.30.DPD.or.worse.in.last.6.months
sum(is.na(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months))
summary(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months)
boxplot(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months)

plot_cb_performance(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months,"No.of.times.30.DPD.or.worse.in.last.6.months")


# check No.of.times.90.DPD.or.worse.in.last.12.months
sum(is.na(CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months))
summary(CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months)

plot_cb_performance(CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months,"No.of.times.90.DPD.or.worse.in.last.12.months")

# check No.of.times.60.DPD.or.worse.in.last.12.months
sum(is.na(CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months))
summary(CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months)

plot_cb_performance(CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months,"No.of.times.60.DPD.or.worse.in.last.12.months")

# check No.of.times.30.DPD.or.worse.in.last.12.months
sum(is.na(CB_Data$No.of.times.30.DPD.or.worse.in.last.12.months))
summary(CB_Data$No.of.times.30.DPD.or.worse.in.last.12.months)

plot_cb_performance(CB_Data$No.of.times.30.DPD.or.worse.in.last.12.months,"No.of.times.30.DPD.or.worse.in.last.12.months")


# check Avgas.CC.Utilization.in.last.12.months
sum(is.na(CB_Data$Avgas.CC.Utilization.in.last.12.months))
summary(CB_Data$Avgas.CC.Utilization.in.last.12.months)
boxplot(CB_Data$Avgas.CC.Utilization.in.last.12.months)

#Fixing outliers
x <- CB_Data$Avgas.CC.Utilization.in.last.12.months
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
CB_Data$Avgas.CC.Utilization.in.last.12.months <- x

# Binning the Avgas.CC.Utilization and store it into "binning.ccutilization".

CB_Data$binning.ccutilization <- as.factor(cut(CB_Data$Avgas.CC.Utilization.in.last.12.months, breaks = c(0, 20, 40, 60, 80, 100)))
plot_cb_performance(CB_Data$binning.ccutilization,"Avgas.CC.Utilization")

# check No.of.trades.opened.in.last.6.months
sum(is.na(CB_Data$No.of.trades.opened.in.last.6.months))
summary(CB_Data$No.of.trades.opened.in.last.6.months)

plot_cb_performance(CB_Data$No.of.trades.opened.in.last.6.months,"No.of.trades.opened.in.last.6.months")

# check No.of.trades.opened.in.last.12.months
sum(is.na(CB_Data$No.of.trades.opened.in.last.12.months))
summary(CB_Data$No.of.trades.opened.in.last.12.months)
# Binning the No.of.trades.opened.in.last.12.months and store it into "binning.trades_12_months".

CB_Data$binning.trades_12_months <- as.factor(cut(CB_Data$No.of.trades.opened.in.last.12.months, breaks = c(0, 5, 10, 15, 20, 25)))
plot_cb_performance(CB_Data$binning.trades_12_months,"No.of.trades.opened.in.last.12.months")

# check No.of.PL.trades.opened.in.last.6.months
sum(is.na(CB_Data$No.of.PL.trades.opened.in.last.6.months))
summary(CB_Data$No.of.PL.trades.opened.in.last.6.months)

plot_cb_performance(CB_Data$No.of.PL.trades.opened.in.last.6.months,"No.of.PL.trades.opened.in.last.6.months")


# No.of.PL.trades.opened.in.last.12.months
sum(is.na(CB_Data$No.of.PL.trades.opened.in.last.12.months))
summary(CB_Data$No.of.PL.trades.opened.in.last.12.months)

plot_cb_performance(CB_Data$No.of.PL.trades.opened.in.last.12.months,"No.of.PL.trades.opened.in.last.12.months")

# No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
sum(is.na(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))
summary(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

plot_cb_performance(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")

# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
sum(is.na(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))
summary(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
boxplot(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
CB_Data$binning.inquiry_12months <- as.factor(cut(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., breaks = c(0, 1, 5, 10, 20)))

plot_cb_performance(CB_Data$binning.inquiry_12months,"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")

# Presence.of.open.home.loan
sum(is.na(CB_Data$Presence.of.open.home.loan))
summary(CB_Data$Presence.of.open.home.loan)

CB_Data$Presence.of.open.home.loan <- as.factor(CB_Data$Presence.of.open.home.loan)
levels(CB_Data$Presence.of.open.home.loan)
plot_cb_performance(CB_Data$Presence.of.open.home.loan,"Presence.of.open.home.loan")

# check Outstanding.Balance
sum(is.na(CB_Data$Outstanding.Balance))
summary(CB_Data$Outstanding.Balance)
boxplot(CB_Data$Outstanding.Balance)
# Binning the Outstanding.Balance and store it into "Outstanding.Balance".

CB_Data$binning.Outstanding.Balance <- as.factor(cut(CB_Data$Outstanding.Balance, breaks = c(0, 10000, 100000, 1000000, 3000000, 5500000)))
plot_cb_performance(CB_Data$binning.Outstanding.Balance,"Outstanding.Balance")

# Total.No.of.Trades
sum(is.na(CB_Data$Total.No.of.Trades))
summary(CB_Data$Total.No.of.Trades)

CB_Data$binning.Total.Trades <- as.factor(cut(CB_Data$Total.No.of.Trades, breaks = c(0, 1, 5, 10, 20, 50)))
plot_cb_performance(CB_Data$binning.Total.Trades,"Total.Trades")

# Presence.of.open.auto.loan
sum(is.na(CB_Data$Presence.of.open.auto.loan))
summary(CB_Data$Presence.of.open.auto.loan)

CB_Data$Presence.of.open.auto.loan <- as.factor(CB_Data$Presence.of.open.auto.loan)
levels(CB_Data$Presence.of.open.auto.loan)
plot_cb_performance(CB_Data$Presence.of.open.auto.loan,"Presence.of.open.auto.loan")

str(DG_Data)
str(CB_Data)


# Delete binning columns
DG_Data <- DG_Data[,-c(13:16)]
CB_Data <- CB_Data[,-c(20:26)]

#Remove Aplication Id
#DG_Data <- DG_Data[,-c(1)]
#CB_Data <- CB_Data[,-c(1)]

# Weight Of Evidence Analaysis (WOE) and Information Value (IV)


# WOE - Replace missing values in the data
# IV - Find the most significant variables in the data

# ************** Demographic data *************


colSums(is.na(DG_Data))   #0
colSums(is.na(CB_Data))   #0




#Merging demographic and credit bureau data

Merged_Data <- merge(DG_Data,CB_Data, by="Application.ID", all = F)
head(Merged_Data)

#Remove one Performance Tag
Merged_Data <- Merged_Data[,-c(12)]
str(Merged_Data)
colnames(Merged_Data)[29]<- "Perf.Tag"
levels(Merged_Data$Perf.Tag)
#--------------------------------------------------------- 

library(scorecard)
iv = iv(Merged_Data, y = 'Perf.Tag') 

knitr::kable(iv)

bins = woebin(Merged_Data, y = 'Perf.Tag')
data_woe = woebin_ply( Merged_Data, bins )

# Information value (IV) Analysis
IV_credit <- create_infotables(data=data_woe, y="Perf.Tag", parallel = TRUE)
IV_credit$Summary

library(tidyr)
library(tibble)
library(tidyverse)
colSums(is.na(data_woe))
data_woe <- data_woe[,-c(2,3,4,5,6,8,9,10)]
str(data_woe)
# Split the data into train and test data
library(caret)
library(caTools)
library(dummies)


data_woe$Perf.Tag <- as.factor(ifelse(data_woe$Perf.Tag == 1, "yes", "no"))

set.seed(1)

split_indices <- sample.split(data_woe$Perf.Tag, SplitRatio = 0.70)

train <- data_woe[split_indices, ]
summary(train)
test <- data_woe[!split_indices, ]

nrow(train)/nrow(Merged_Data)

nrow(test)/nrow(Merged_Data)
### Building the Logistic Regression Model


library(MASS)

library(car)

logistic_1 <- glm(Perf.Tag ~ ., family = "binomial", data = train)

summary(logistic_1)

#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

logistic_2 <- stepAIC(logistic_1, direction = "both")
summary(logistic_2)
vif(logistic_2)

logistic_3 <- glm(formula = Perf.Tag ~ Income_woe + No.of.months.in.current.company_woe + 
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                    No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                    No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    Outstanding.Balance_woe + Total.No.of.Trades_woe, family = "binomial", 
                  data = train)

summary(logistic_3)
vif(logistic_3)

logistic_4 <- glm(formula = Perf.Tag ~ Income_woe + No.of.months.in.current.company_woe + 
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                    No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                    No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    Outstanding.Balance_woe, family = "binomial", 
                  data = train)

summary(logistic_4)
vif(logistic_4)

logistic_5 <- glm(formula = Perf.Tag ~ Income_woe + No.of.months.in.current.company_woe + 
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                    No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                    No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe 
                    , family = "binomial", 
                  data = train)

summary(logistic_5)
vif(logistic_5)

logistic_6 <- glm(formula = Perf.Tag ~ Income_woe + No.of.months.in.current.company_woe +
                    No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                    No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe 
                  , family = "binomial", 
                  data = train)

summary(logistic_6)
vif(logistic_6)

logistic_7 <- glm(formula = Perf.Tag ~ No.of.months.in.current.company_woe +
                    No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                    No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe 
                  , family = "binomial", 
                  data = train)

summary(logistic_7)
vif(logistic_7)

str(test)

logistic_final <- logistic_7
#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(logistic_final, newdata = test, type = "response")
summary(predictions_logit)
#--------------------------------------------------------- 

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 5%.

predicted_response <- factor(ifelse(predictions_logit >= 0.05, 'yes', 'no'))
summary(predicted_response)

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$Perf.Tag, positive = 'yes')

conf

library(ROCR)

pred <- prediction(predictions_logit,test$Perf.Tag)
eva_log<-performance(pred,"sens","spec")
evaA_log<-performance(pred,'acc')
plot(evaA_log)

sensitivity <- eva_log@y.values[[1]]
cutoff <- eva_log@alpha.values[[1]]
specificity<- eva_log@x.values[[1]]
accuracy<-evaA_log@y.values[[1]]
plot(cutoff,sensitivity,col="red")
lines(cutoff,specificity,col="green")
lines(cutoff,accuracy,col="blue")
legend("bottomright", legend=c("sensitivity","accuracy","specificity"),
       col=c("red","blue","green"), lty=1:2, cex=0.8)

abline(v =0.5)
matrix<-data.frame(cbind(sensitivity,specificity,accuracy,cutoff))

final_matrix<-matrix[which(matrix$cutoff>0.01&matrix$cutoff<1),]

#The plot shows optimal cutoff at 0.045

# Let's use the probability cutoff of 4.5%.

predicted_response <- factor(ifelse(predictions_logit >= 0.045, 'yes', 'no'))
summary(predicted_response)
levels(predicted_response)
# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$Perf.Tag, positive = 'yes')

conf

# So final figures are:
#Accuracy : 0.6158
#Sensitivity : 0.62217       
#Specificity : 0.61549

#for cutoff 0.0485
#Sensitivity :0.5723981900
#Specificity :0.6561892902
#Accuracy :0.6526551839

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_churn, test_actual_churn)

performance_measures_test<- performance(pred, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#0.2535332999

#--------------------------------------------------------- 
#Let's use K fold validation with logistic regression

library(doSNOW)
library(parallel)

# detect true cores requires parallel()
nCores <- detectCores(logical = FALSE)
# detect threads
nThreads <- detectCores(logical = TRUE)

cl <- makeCluster(nThreads, type="SOCK")
registerDoSNOW(cl); cl;
getDoParWorkers()
getDoParName()
# Define train control for k fold cross validation
ctrl <- trainControl(method="cv", number=10)
# Fit Logistic Regression Model
model <- train(Perf.Tag ~ No.of.months.in.current.company_woe +
                 No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                 No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe ,  data=train, method="glm", family=binomial(),
                 trControl = ctrl)
# Summarise Results
print(model)
summary(model)

stopCluster(cl)

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit_cv <- predict(model, newdata = test, type = "prob")
summary(predictions_logit_cv)
dim(predictions_logit_cv)
# Cutoff for logistic regression cv to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response_cv <- as.factor(ifelse(predictions_logit_cv[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response_cv, test$Perf.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_cv <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_cv) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_cv)
}

#---------------------------------------------------------    


# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_cv = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_cv[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_cv[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_cv[,2],col="darkgreen",lwd=2)
lines(s,OUT_cv[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_cv <- s[which(abs(OUT_cv[,1]-OUT_cv[,2])<0.12)]


# Let's use the probability cutoff of 4.95%.

predicted_response_cv <- factor(ifelse(predictions_logit_cv[,2] >= 0.0495, 'yes', 'no'))
# Creating confusion matrix for identifying the model evaluation.
conf_cv <- confusionMatrix(predicted_response_cv, test$Perf.Tag, positive = 'yes')

conf_cv
# Results
#Sensitivity : 0.55882         
#Specificity : 0.66869   
#Balanced Accuracy : 0.61376     
#--------------------------------------------------------- 
#Let's use randomforest with logistic regression final model variables
library(randomForest)

# Building the model 
random_rf <- randomForest(Perf.Tag ~No.of.months.in.current.company_woe +
                            No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                            No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe, data = train, proximity = F, do.trace = F, mtry = 5)
rf_pred <- predict(random_rf, test, type = "prob")
summary(rf_pred)
summary(random_rf)

# Model Evaluation ...............#

# Finding Cutoff for randomforest to assign yes or no
#---------------------------------------------------------  
#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$Perf.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    


# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]


#--------------------------------------------------------- 

#--------------------------------------------------------- 
#Let's use K fold validation with random forest

library(doSNOW)
library(parallel)

# detect true cores requires parallel()
nCores <- detectCores(logical = FALSE)
# detect threads
nThreads <- detectCores(logical = TRUE)

cl <- makeCluster(nThreads, type="SOCK")
registerDoSNOW(cl); cl;
getDoParWorkers()
getDoParName()
# Define train control for k fold cross validation
ctrl <- trainControl(method="cv", number=5)
# Fit Logistic Regression Model
rf_model_cv <- train(Perf.Tag ~ . ,  data=train, method="rf", 
               trControl = ctrl)
# Summarise Results
print(rf_model_cv)
summary(rf_model_cv)

stopCluster(cl)

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit_rf_cv <- predict(rf_model_cv, newdata = test, type = "prob")
summary(predictions_logit_rf_cv)
# Cutoff for random forest cv to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response_rf_cv <- as.factor(ifelse(predictions_logit_rf_cv[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response_rf_cv, test$Perf.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf_cv <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf_cv) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf_cv)
}

#---------------------------------------------------------    


# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf_cv = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf_cv[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf_cv[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf_cv[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf_cv[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_cv <- s[which(abs(OUT_rf_cv[,1]-OUT_rf_cv[,2])<0.12)]


#--------------------------------------------------------- 
#Let's use neural network
library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(e1071)
library(keras)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

train$Perf.Tag <- as.factor(train$Perf.Tag)
test$Perf.Tag <- as.factor(test$Perf.Tag)
str(train)

train_num_norm <- as.data.frame(lapply(train[,2:21], normalize ))
test_num_norm <- as.data.frame(lapply(test[,2:21], normalize ))

train_num_norm$Perf.Tag <- as.factor(ifelse(train$Perf.Tag == 'yes', 1, 0))
test_num_norm$Perf.Tag <- as.factor(ifelse(test$Perf.Tag == 'yes', 1, 0))

# build the neural network (NN) formula
a <- colnames(train[,2:21])
mformula <- as.formula(paste('Perf.Tag ~ ' , paste(a,collapse='+')))

set.seed(1234567890)
train_nn <- train_num_norm
test_nn <- test_num_norm
str(train_nn)

# Building the model 
m8 <- nnet(Perf.Tag~., data=train_nn,size=20,maxit=10000,decay=.001, linout=F, trace = F)

table(test_nn$Perf.Tag,predict(m8,newdata=test_nn, type="class"))

m8_pred <- prediction(predict(m8, newdata=test_nn, type="raw"),test_nn$Perf.Tag)
m8_perf <- performance(m8_pred,"tpr","fpr")
nn_pred <- predict(m8, test_nn, type = "raw")
summary(nn_pred)

library(ROCR)
# Model Evaluation ...............#
pred_nn <- ROCR::prediction(nn_pred,test_nn$Perf.Tag)
eva_log<-performance(pred_nn,"sens","spec")
evaA_log<-performance(pred_nn,'acc')
plot(evaA_log)

sensitivity <- eva_log@y.values[[1]]
cutoff <- eva_log@alpha.values[[1]]
specificity<- eva_log@x.values[[1]]
accuracy<-evaA_log@y.values[[1]]
plot(cutoff,sensitivity,col="red")
lines(cutoff,specificity,col="green")
lines(cutoff,accuracy,col="blue")
legend("bottomright", legend=c("sensitivity","accuracy","specificity"),
       col=c("red","blue","green"), lty=1:2, cex=0.8)

abline(v =0.5)
matrix<-data.frame(cbind(sensitivity,specificity,accuracy,cutoff))

final_matrix<-matrix[which(matrix$cutoff>0.01&matrix$cutoff<1),]
# Let's use the probability cutoff of 3.5%.

predicted_response_nn <- factor(ifelse(nn_pred >= 0.035, '1', '0'))
summary(predicted_response_nn)
levels(predicted_response_nn)
# Creating confusion matrix for identifying the model evaluation.
conf_nn <- confusionMatrix(predicted_response_nn, test_nn$Perf.Tag, positive = '1')

conf_nn

#Results of neural network
#Sensitivity : 0.58257919
#Specificity : 0.59133250 
#Accuracy : 0.5909633

# Predicting probabilities of responding for the whole dataset using logistic regression model

predictions_logit <- predict(logistic_final, newdata = data_woe, type = "response")
summary(predictions_logit)

predicted_response <- factor(ifelse(predictions_logit >= 0.045, 'yes', 'no'))
# Creating confusion matrix for identifying the model evaluation.
library(dplyr)

final <- cbind(data_woe,predictions_logit)
final <- cbind(final, predicted_response)



m_step = step(logistic_final, direction="both", trace = FALSE)
m2 = eval(m_step$call)
# Calculate scorecard scores for variables based on the results from woebin and glm: 
my_card <- scorecard(bins, m2, points0 = 400, odds0 = 10, pdo = 20)
sc = scorecard_ply( Merged_Data, my_card )
final <- cbind(final, sc)
summary(final$predicted_response)
final_sorted <-  final[order(-predictions_logit, -score),]
final_sorted_no <- filter(final_sorted, final_sorted$predicted_response == 'no')
final_sorted_yes <- filter(final_sorted, final_sorted$predicted_response == 'yes')
str(final)
summary(final_sorted_yes$score) #529-555
summary(final_sorted_no$score) #554-595

#score cutoff should be around 555
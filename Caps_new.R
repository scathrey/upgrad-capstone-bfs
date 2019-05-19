#UpGrad - PGDDS - Capstone BFSI Project


#Group members:
#1. Sharath Athrey
#2. Sumit Kaushik
#3. Vandhana Shri


#-------------------------------------------------------------------------------#
#Install and Load the required packages
#-------------------------------------------------------------------------------#


install.packages('caTools')
install.packages("foreach")
install.packages('randomForest')
install.packages("lift")
install.packages('plyr')
install.packages('kernlab')
install.packages('readr')
install.packages('xgboost')
install.packages("Information")
install.packages("gridExtra")
install.packages("grid")
install.packages("ggplot2")
install.packages('corrplot')
install.packages("dplyr")
install.packages("caret")
install.packages("car")
install.packages("MASS")
install.packages("ROSE")
install.packages("woeBinning")
install.packages("e1071")
install.packages("cowplot")



library(AUC)
library(ROSE)
library(randomForest)
library(lift)
library(plyr)
library(kernlab)
library(readr)
library(foreach)
library(xgboost)
library(Information)
library(gridExtra)
library(grid)
library(ggplot2)
library(corrplot)
library(dplyr)
library(caret)
library(car)
library(MASS)
library(woeBinning)
library(e1071)
library(cowplot)
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
DG_Data <- DG_Data[,-c(1)]
CB_Data <- CB_Data[,-c(1)]

# Weight Of Evidence Analaysis (WOE) and Information Value (IV)


# WOE - Replace missing values in the data
# IV - Find the most significant variables in the data

# ************** Demographic data *************

#New variable changed.perf.tag (0 to 1, 1 to 0)
DG_Data$Changed.Perf.Tag <- ifelse(DG_Data$Performance.Tag == 0,1,0)

##### creating woe buckets - numerical variables

#New dataset containing character variables and missing values including "Changed.Perf.Tag"
colnames <- c("Changed.Perf.Tag","Marital.Status..at.the.time.of.application.","Gender","Education","Profession","Type.of.residence")
missing_chars <- DG_Data[,colnames(DG_Data)%in%colnames]
binning <- woe.binning(missing_chars, 'Changed.Perf.Tag', missing_chars)
df.with.binned.vars.added <- woe.binning.deploy(missing_chars, binning,add.woe.or.dum.var='woe')
demo_woe <-df.with.binned.vars.added[,c(8,10,12,14,16)]

#replacing 5 char variables having missing values with their respective WOE values.
colnames <- c("Marital.Status..at.the.time.of.application.","Gender","Education","Profession","Type.of.residence")
DG_Data <- DG_Data[,!colnames(DG_Data)%in%colnames]
DG_Data <- cbind(DG_Data,demo_woe)


##### Information value (IV) Analysis
IV_analysis <- Information::create_infotables(data=DG_Data, y="Changed.Perf.Tag", parallel = TRUE)
IV_analysis$Summary

#Replace the missing values of No.of.dependents with WOE values
IV_analysis$Tables[3]


DG_Data$No.of.dependents[which(is.na(DG_Data$No.of.dependents))]<-0
for(i in 1:nrow(DG_Data))
{
  
  if(DG_Data$No.of.dependents[i]>=1 & DG_Data$No.of.dependents[i]< 4)
  {
    if(DG_Data$No.of.dependents[i]<=3)
    {DG_Data$No.of.dependents[i]<- -0.005}
  }
  else if (DG_Data$No.of.dependents[i]>= 4)
  {DG_Data$No.of.dependents[i]<- 0.01}
  else{DG_Data$No.of.dependents[i]<- 0}
}

summary(DG_Data$No.of.dependents)   

#  Information value (IV) Analysis - No bins i.e max bins included
IV_analysis <- Information::create_infotables(data=DG_Data, y="Changed.Perf.Tag", parallel = TRUE)
IV_analysis$Summary


# ************* Credit Bureau data *************

#New variable changed.perf.tag (0 to 1, 1 to 0)
CB_Data$Changed.Perf.Tag <- ifelse(CB_Data$Performance.Tag == 0,1,0)

##### Performing binning on "Avgas.CC.Utilization.in.last.12.months"
colnames<- c("Changed.Perf.Tag","Avgas.CC.Utilization.in.last.12.months")
missing_chars <- CB_Data[,names(CB_Data)%in%colnames]
binning_Avgas.CC.Utilization.in.last.12.months <- woe.binning(missing_chars, 'Changed.Perf.Tag', "Avgas.CC.Utilization.in.last.12.months",stop.limit=0.01)
binning_Avgas.CC.Utilization.in.last.12.months[[2]]
missing_chars <- woe.binning.deploy(missing_chars, binning_Avgas.CC.Utilization.in.last.12.months,add.woe.or.dum.var='woe')
CB_Data$Avgas.CC.Utilization.in.last.12.months<-missing_chars$woe.Avgas.CC.Utilization.in.last.12.months.binned

#binning all other variables with missing values in Credit Bureau data
colnames<- c("Presence.of.open.home.loan","Changed.Perf.Tag","No.of.trades.opened.in.last.6.months","Outstanding.Balance")
missing_chars <- CB_Data[,names(CB_Data)%in%colnames]
binnings <- woe.binning(missing_chars, 'Changed.Perf.Tag', missing_chars)
missing_chars <- woe.binning.deploy(missing_chars, binnings,add.woe.or.dum.var='woe')

#replacing all other missing variables in Credit Bureau data with their WOE values
CB_Data$No.of.trades.opened.in.last.6.months<-missing_chars$woe.No.of.trades.opened.in.last.6.months.binned
CB_Data$Presence.of.open.home.loan<-missing_chars$woe.Presence.of.open.home.loan.binned
CB_Data$Outstanding.Balance<-missing_chars$woe.Outstanding.Balance.binned

colSums(is.na(DG_Data))   #0
colSums(is.na(CB_Data))   #0



# Information value (IV) Analysis
IV_credit <- create_infotables(data=CB_Data, y="Changed.Perf.Tag", parallel = TRUE)
IV_credit$Summary

#All variables except the following 6 variales are monotonically changing across bins:
#"No.of.trades.opened.in.last.12.months"
#"No.of.PL.trades.opened.in.last.6.months"
#"No.of.PL.trades.opened.in.last.12.months"
#"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans."
#"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."
#"Total.No.of.Trades"
# So we will have to make coarse bins for these 6 variables

colnames<-c("No.of.trades.opened.in.last.12.months","No.of.PL.trades.opened.in.last.6.months",
            "No.of.PL.trades.opened.in.last.12.months",
            "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
            "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
            "Total.No.of.Trades")
IV_coarse_bins<-CB_Data[,colnames(CB_Data)%in%colnames]
IV_coarse_bins<-cbind(IV_coarse_bins,CB_Data$Changed.Perf.Tag)
IV_coarse <- create_infotables(data=IV_coarse_bins, y="CB_Data$Changed.Perf.Tag",bins=3, parallel = TRUE)
IV_coarse$Summary

IV_fine_bins<-CB_Data[,!colnames(CB_Data)%in%colnames]
IV_fine_bins<-cbind(IV_fine_bins,CB_Data$Changed.Perf.Tag)
IV_fine <- create_infotables(data=IV_fine_bins, y="CB_Data$Changed.Perf.Tag", parallel = TRUE)
IV_fine$Summary

IV_final<-rbind(IV_coarse$Summary,IV_fine$Summary)
IV_final<-IV_final[-c(19,20),]
IV_final_sorted<-IV_final[order(IV_final$IV,decreasing = T),]
head(IV_final_sorted)

IV_final_sorted<-rbind(IV_final_sorted,IV_analysis$Summary)
IV_final_sorted<-IV_final_sorted[order(IV_final_sorted$IV,decreasing = T),]
head(IV_final_sorted)

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
CB_Data <- CB_Data[,-c(20:24)]

#Remove Aplication Id
#DG_Data <- DG_Data[,-c(1)]
#CB_Data <- CB_Data[,-c(1)]

#-------------------------------------------------------------------------------#
# WOE Analaysis and IV (Information Value)
#-------------------------------------------------------------------------------#

# This method is used to replace the missing values with woe values and find significant variables.

# ************** Demographic data *************

#Since Information package treats 1 as 'good', we are adding a new variable 'Reverse.Performance.Tag'.
DG_Data$Reverse.Performance.Tag <- ifelse(DG_Data$Performance.Tag == 0,1,0)

##### creating woe buckets for numerical variables

#making a dataset containing character variables with missing values along with "Reverse.Performance.Tag"
colnames <- c("Reverse.Performance.Tag","Marital.Status..at.the.time.of.application.","Gender","Education","Profession","Type.of.residence")
char_missing <- DG_Data[,colnames(DG_Data)%in%colnames]
binning <- woe.binning(char_missing, 'Reverse.Performance.Tag', char_missing)
df.with.binned.vars.added <- woe.binning.deploy(char_missing, binning,add.woe.or.dum.var='woe')
demo_woe <-df.with.binned.vars.added[,c(8,10,12,14,16)]

#replacing 5 char variables having missing values with their respective WOE values.
colnames <- c("Marital.Status..at.the.time.of.application.","Gender","Education","Profession","Type.of.residence")
DG_Data <- DG_Data[,!colnames(DG_Data)%in%colnames]
DG_Data <- cbind(DG_Data,demo_woe)

#____________________________________________

##### Information value (IV) Analysis
IV_demo <- Information::create_infotables(data=DG_Data, y="Reverse.Performance.Tag",bins = 2, parallel = TRUE)
IV_demo$Summary

#REPLACING the actual values of No.of.dependents with corresponding WOE VALUES
IV_demo$Tables[3]

#No.of.dependents     N      Percent          WOE           IV
#1               NA     3 4.293873e-05  0.000000000 0.000000e+00
#2            [1,3] 45989 6.582650e-01  -0.005418527 1.937496e-05
#3            [4,5] 23872 3.416921e-01  0.010382663 5.603467e-05

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

#  Information value (IV) Analysis without specifying number of bins
#so that iv is calculated on maximum bins possible for monotonically changing WOE values across bins for continous variables
IV_demo <- Information::create_infotables(data=DG_Data, y="Reverse.Performance.Tag", parallel = TRUE)
IV_demo$Summary

#Based on IV values, below are the top 3 significant variables.

#Variable                               IV
#--------                               ----   
#No.of.months.in.current.residence     7.895394e-02
#Income                                4.241078e-02
#No.of.months.in.current.company       2.176071e-02


# ************* Credit Bureau data *************

#Since Information package treats 1 as 'good', we are adding a new variable 'Reverse.Performance.Tag'.
CB_Data$Reverse.Performance.Tag <- ifelse(CB_Data$Performance.Tag == 0,1,0)

##### binning "Avgas.CC.Utilization.in.last.12.months"
colnames<- c("Reverse.Performance.Tag","Avgas.CC.Utilization.in.last.12.months")
char_missing <- CB_Data[,names(CB_Data)%in%colnames]
binning_Avgas.CC.Utilization.in.last.12.months <- woe.binning(char_missing, 'Reverse.Performance.Tag', "Avgas.CC.Utilization.in.last.12.months",stop.limit=0.01)
binning_Avgas.CC.Utilization.in.last.12.months[[2]]
char_missing <- woe.binning.deploy(char_missing, binning_Avgas.CC.Utilization.in.last.12.months,add.woe.or.dum.var='woe')
CB_Data$Avgas.CC.Utilization.in.last.12.months<-char_missing$woe.Avgas.CC.Utilization.in.last.12.months.binned


#binning all other variables with missing values in Credit Bureau data
colnames<- c("Presence.of.open.home.loan","Reverse.Performance.Tag","No.of.trades.opened.in.last.6.months","Outstanding.Balance")
char_missing <- CB_Data[,names(CB_Data)%in%colnames]
binning_b <- woe.binning(char_missing, 'Reverse.Performance.Tag', char_missing)
char_missing <- woe.binning.deploy(char_missing, binning_b,add.woe.or.dum.var='woe')

#replacing all other missing variables in Credit Bureau data with their WOE values
CB_Data$No.of.trades.opened.in.last.6.months<-char_missing$woe.No.of.trades.opened.in.last.6.months.binned
CB_Data$Presence.of.open.home.loan<-char_missing$woe.Presence.of.open.home.loan.binned
CB_Data$Outstanding.Balance<-char_missing$woe.Outstanding.Balance.binned

colSums(is.na(DG_Data))   #0
colSums(is.na(CB_Data))   #0

#_____________________________________________

##### Information value (IV) Analysis
IV_credit <- create_infotables(data=CB_Data, y="Reverse.Performance.Tag", parallel = TRUE)
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
IV_coarse_bins<-cbind(IV_coarse_bins,CB_Data$Reverse.Performance.Tag)
IV_coarse <- create_infotables(data=IV_coarse_bins, y="CB_Data$Reverse.Performance.Tag",bins=3, parallel = TRUE)
IV_coarse$Summary

IV_fine_bins<-CB_Data[,!colnames(CB_Data)%in%colnames]
IV_fine_bins<-cbind(IV_fine_bins,CB_Data$Reverse.Performance.Tag)
IV_fine <- create_infotables(data=IV_fine_bins, y="CB_Data$Reverse.Performance.Tag", parallel = TRUE)
IV_fine$Summary

IV_final<-rbind(IV_coarse$Summary,IV_fine$Summary)
IV_final<-IV_final[-c(19,20),]
IV_final_sorted<-IV_final[order(IV_final$IV,decreasing = T),]
head(IV_final_sorted)

IV_final_sorted<-rbind(IV_final_sorted,IV_demo$Summary)
IV_final_sorted<-IV_final_sorted[order(IV_final_sorted$IV,decreasing = T),]
head(IV_final_sorted)

#Based on IV values, below are the top significant variables.

# Variable                                                           IV
#No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 0.2715447
#                         Avgas.CC.Utilization.in.last.12.months 0.2607554
#                   No.of.times.30.DPD.or.worse.in.last.6.months 0.2415627
#                  No.of.times.90.DPD.or.worse.in.last.12.months 0.2138748
#                   No.of.times.60.DPD.or.worse.in.last.6.months 0.2058339

#On the combined demographic and credit bureau data the iv values are highest

#-------------------------------------------------------------------------------#
# Outlier treatment
#-------------------------------------------------------------------------------#

summary(demographics$Age) 
levels(factor(demographics$Age))

# checking for any outliers in the continuous data using quantiles and replacing them with the nearest non-outlier values.

quantile(DG_Data$Age,seq(0,1,0.01)) 
DG_Data$Age[which(DG_Data$Age<27)]<-27

quantile(DG_Data$No.of.months.in.current.company,seq(0,1,0.01))
DG_Data$No.of.months.in.current.company[which(DG_Data$No.of.months.in.current.company>74)]<-74

quantile(CB_Data$No.of.trades.opened.in.last.12.months,seq(0,1,0.01))
CB_Data$No.of.trades.opened.in.last.12.months[which(CB_Data$No.of.trades.opened.in.last.12.months>21)]<-21

quantile(CB_Data$Total.No.of.Trades,seq(0,1,0.01))
CB_Data$Total.No.of.Trades[which(CB_Data$Total.No.of.Trades>31)]<-31

quantile(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,seq(0,1,0.01))
CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.[which(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. >15)]<- 15

quantile(CB_Data$No.of.PL.trades.opened.in.last.12.months,seq(0,1,0.01))
CB_Data$No.of.PL.trades.opened.in.last.12.months[which(CB_Data$No.of.PL.trades.opened.in.last.12.months > 9)]<- 9

quantile(CB_Data$No.of.times.30.DPD.or.worse.in.last.12.months,seq(0,1,0.01))
CB_Data$No.of.times.30.DPD.or.worse.in.last.12.months[which(CB_Data$No.of.times.30.DPD.or.worse.in.last.12.months > 5)]<- 5

quantile(CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months,seq(0,1,0.01))
CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months[which(CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months>4)]<-4

quantile(CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months,seq(0,1,0.01))
CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months[which(CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months>3)]<-3

quantile(CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months,seq(0,1,0.01))
CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months[which(CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months > 1)]<- 1

quantile(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months,seq(0,1,0.01))
CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months[which(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months > 2)]<- 2

quantile(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months,seq(0,1,0.01))
CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months[which(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months> 2)]<- 2

quantile(CB_Data$Avgas.CC.Utilization.in.last.12.months,seq(0,1,0.01))
#there are no outliers after WOE imputation

quantile(CB_Data$No.of.trades.opened.in.last.6.months,seq(0,1,0.01))
#there are no outliers after WOE imputation

quantile(CB_Data$No.of.PL.trades.opened.in.last.6.months,seq(0,1,0.01))
CB_Data$No.of.PL.trades.opened.in.last.6.months[which(CB_Data$No.of.PL.trades.opened.in.last.6.months> 4)]<- 4

quantile(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,seq(0,1,0.01))
CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.> 6)]<- 6

# Taking backup of the datasets
credit_bf_model <- CB_Data
demo_bf_model <- DG_Data


#################################################################################
#-------------------------------------------------------------------------------#
# Model Building and Evaluation for Demographic data
#-------------------------------------------------------------------------------#
#################################################################################

prop.table(table(DG_Data$Performance.Tag))
## Only around 4.2% of observations are under default category. 
## The data is highly imbalanced and hence class balancing is to be performed.

#Converting Performance.Tag as factor type.
DG_Data$Performance.Tag <- as.factor(DG_Data$Performance.Tag)

#scaling all variables except target variable to the same scale   
DG_Data[,-c(1,7)]<-data.frame(scale(DG_Data[,-c(1,7)]))

# Split into test and train datasets
set.seed(100)
split_indices <- sample.split(DG_Data$Performance.Tag, SplitRatio = 0.70)
train_demo <- DG_Data[split_indices, ]
test_demo <- DG_Data[!split_indices, ]

table(train_demo$Performance.Tag)
#   0     1 
# 46842   2063

#balancing the two classes using ROSE package
train_demo_SMOTE <- ROSE(Performance.Tag ~ ., train_demo,seed=1)$data
table(train_demo_SMOTE$Performance.Tag)
#    0    1 
#  24450 24455  

prop.table(table(train_demo_SMOTE$Performance.Tag))
# 0         1 
# 0.49 0.50

train_demo_SMOTE_other <- train_demo_SMOTE

# Removing variable Reverse.Performance.Tag from train data
train_demo_SMOTE_other$Application.ID<-NULL
train_demo_SMOTE_other$Reverse.Performance.Tag <- NULL

# Removing variable Application.ID and Reverse.Performance.Tag from test data
test_demo_other <- test_demo
test_demo$Application.ID<-NULL
test_demo$Reverse.Performance.Tag <- NULL

#--------------------------------------------------------------
#************* 1. Logistic Regresssion Model   ****************
#--------------------------------------------------------------

#.............. 1.a) Model Building ...............#

model_lr_1 = glm(Performance.Tag ~ ., data = train_demo_SMOTE_other, family = "binomial")
summary(model_lr_1)

# Variable selection using stepwise AIC algorithm for removing insignificant variables 

model_lr_2<- stepAIC(model_lr_1, direction="both")
summary(model_lr_2)
vif(model_lr_2)
#all vif values are less than 2
#AIC:67245

#predictors are:
#Income                            ***
#No.of.months.in.current.residence ***
#No.of.months.in.current.company   ***
#woe.Profession.binned             ***
#No.of.dependents                  *

# All variables have extremly low p values , hence keeping all varibales on that criteria

final_lr_model <- model_lr_2
test_pred_log = predict(final_lr_model, type = "response", newdata = test_demo[,-6])
summary(test_pred_log)

pred <- prediction(test_pred_log,test_demo$Performance.Tag)
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

final_matrix<-matrix[which(matrix$cutoff>0.1&matrix$cutoff<1),]
matrix<-sapply(final_matrix,function(x) round(x,2))
matrix<-data.frame(matrix)
head(matrix[which(matrix$accuracy==matrix$sensitivity&matrix$sensitivity==matrix$specificity),])

#Cutoff = 0.5
#sensitivity=54%
#specificity=54%
#Accuracy=54%
#Thus a logistic regression model based only on demographic data seems to have low performance.

#--------------------------------------------------------------
#******************* 2. Random Forest   ***********************
#--------------------------------------------------------------

#.............. 2.a) Model Building ...............#

random_rf <- randomForest(Performance.Tag ~., data = train_demo_SMOTE_other, proximity = F, do.trace = F, mtry = 5)
rf_pred <- predict(random_rf, test_demo, type = "prob")

#_____________________________________________________

#.............. 2.b) Model Evaluation ...............#

# Finding Cutoff for randomforest to assign yes or no
perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, test_demo_other$Performance.Tag, positive = "0")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

summary(rf_pred[,2])
s = seq(.00,.91,length=100)
OUT_rf = matrix(0,100,3)

# calculating the sensitivity, specificity and Accuracy for different cutoff values
for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)

box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
min(abs(OUT_rf[,1]-OUT_rf[,2]))
cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.30)]
cutoff_rf
#   0.2206061 0.2297980 0.2389899 0.2481818 0.2573737 0.2665657 0.2757576 0.2849495

# The plot shows that cutoff value of around 0.25 optimises sensitivity and accuracy
predicted_response <- factor(ifelse(rf_pred[, 2] >= 0.25, "1", "0"))
conf_forest <- confusionMatrix(predicted_response, test_demo_other$Performance.Tag, positive = "0")
conf_forest

#Confusion Matrix and Statistics


#Reference
#Prediction     0     1
#0              10421   400
#1              9654   484

#Accuracy : 0.5203          
#Sensitivity : 0.51910         
#Specificity : 0.54751         
#'Positive' Class : 0    

#========== Conclusion ==========
# Random forest model perfoms almost equally for demographic data compared to 
# logistic regression model. But the overall performance is very low for models made from demographic data alone.
# Hence moving forward with building models for demographic and credit data combined.



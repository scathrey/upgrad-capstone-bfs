#UpGrad - PGDDS - Capstone BFSI Project


#Group members:
#1. Sharath Athrey
#2. Sumit Kaushik
#3. Vandhana Shri



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

DG_Data <- read.csv("Demographic data.csv",  na.strings = c("NA",""))
CB_Data <- read.csv("Credit Bureau data.csv",na.strings = c("NA",""))


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
sapply(list(CB_Data,DG_Data), function(x) length(which(is.na(x))))
colSums(is.na(CB_Data))
colSums(is.na(DG_Data))

#Exluding missing entries
CB_Data<-CB_Data[!(is.na(CB_Data$Performance.Tag)),]
DG_Data<-DG_Data[!(is.na(DG_Data$Performance.Tag)),]

#Demo and Credit rejected records : Performance.Tag.
CB_Data_Reject <- CB_Data[which(is.na(CB_Data$Performance.Tag)),]
DG_Data_Reject <- DG_Data[which(is.na(DG_Data$Performance.Tag)),]

nrow(CB_Data)
nrow(DG_Data)
# Final cleaned up observations - 69864 obs

#Data backup before WOE treatment

CB_Data_NoWoe <- CB_Data
DG_Data_NoWoe <- DG_Data


# Weight Of Evidence Analaysis (WOE) and Information Value (IV)


# WOE - Replace missing values in the data
# IV - Find the most significant variables in the data
# This method is used to replace the missing values with woe values and find significant variables.

# ************** Demographic data *************

#Since Information package treats 1 as 'good', we are adding a new variable 'Reverse.Performance.Tag'.
DG_Data$Changed.Perf.Tag <- ifelse(DG_Data$Performance.Tag == 0,1,0)

##### creating woe buckets for numerical variables

#making a dataset containing character variables with missing values along with "Reverse.Performance.Tag"
colnames <- c("Changed.Perf.Tag","Marital.Status..at.the.time.of.application.","Gender","Education","Profession","Type.of.residence")
char_missing <- DG_Data[,colnames(DG_Data)%in%colnames]
binning <- woe.binning(char_missing, 'Changed.Perf.Tag', char_missing)
df.with.binned.vars.added <- woe.binning.deploy(char_missing, binning,add.woe.or.dum.var='woe')
demo_woe <-df.with.binned.vars.added[,c(8,10,12,14,16)]

#replacing 5 char variables having missing values with their respective WOE values.
colnames <- c("Marital.Status..at.the.time.of.application.","Gender","Education","Profession","Type.of.residence")
DG_Data <- DG_Data[,!colnames(DG_Data)%in%colnames]
DG_Data <- cbind(DG_Data,demo_woe)



##### Information value (IV) Analysis
IV_analysis <- Information::create_infotables(data=DG_Data, y="Changed.Perf.Tag",bins = 2, parallel = TRUE)
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
char_missing <- CB_Data[,names(CB_Data)%in%colnames]
binning_Avgas.CC.Utilization.in.last.12.months <- woe.binning(char_missing, 'Changed.Perf.Tag', "Avgas.CC.Utilization.in.last.12.months",stop.limit=0.01)
binning_Avgas.CC.Utilization.in.last.12.months[[2]]
char_missing <- woe.binning.deploy(char_missing, binning_Avgas.CC.Utilization.in.last.12.months,add.woe.or.dum.var='woe')
CB_Data$Avgas.CC.Utilization.in.last.12.months<-char_missing$woe.Avgas.CC.Utilization.in.last.12.months.binned

#binning all other variables with missing values in Credit Bureau data
colnames<- c("Presence.of.open.home.loan","Changed.Perf.Tag","No.of.trades.opened.in.last.6.months","Outstanding.Balance")
char_missing <- CB_Data[,names(CB_Data)%in%colnames]
binning_b <- woe.binning(char_missing, 'Changed.Perf.Tag', char_missing)
char_missing <- woe.binning.deploy(char_missing, binning_b,add.woe.or.dum.var='woe')

#replacing all other missing variables in Credit Bureau data with their WOE values
CB_Data$No.of.trades.opened.in.last.6.months<-char_missing$woe.No.of.trades.opened.in.last.6.months.binned
CB_Data$Presence.of.open.home.loan<-char_missing$woe.Presence.of.open.home.loan.binned
CB_Data$Outstanding.Balance<-char_missing$woe.Outstanding.Balance.binned

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


# Exploratory data analysis - UNIVARIATE ANALYSIS - Demographic data

ggplot(DG_Data_NoWoe,aes(x=Profession))+geom_bar(fill="green") #Profession type : "Salaried" tops the applicants profession

ggplot(DG_Data_NoWoe,aes(x=Marital.Status..at.the.time.of.application.))+geom_bar(fill="blue") # Marital Status : "Married" status" applicants are more vs "Single"

ggplot(DG_Data_NoWoe,aes(x=Education))+geom_bar(fill="red") #Education level : "professional" / "Masters" tops the applicants edu level

ggplot(DG_Data_NoWoe,aes(x=Type.of.residence))+geom_bar(fill="purple") #Residence type : "Rented" tops the applicants type of residence

boxplot(DG_Data_NoWoe$Age)
median(DG_Data_NoWoe$Age) #Limited or few outliers found - Median Age of applicants - 45 years

boxplot(DG_Data_NoWoe$Income)
median(DG_Data_NoWoe$Income) #No outliers found - Median income of applicants - 27 units

boxplot(DG_Data_NoWoe$No.of.months.in.current.company)
median(DG_Data_NoWoe$No.of.months.in.current.company) #Limited or few outliers found - Median No.of.months.in.current.company - 34

boxplot(DG_Data_NoWoe$No.of.months.in.current.residence)
median(DG_Data_NoWoe$No.of.months.in.current.residence) #No outliers found - Median No.of.months.in.current.residence - 10


# Exploratory data analysis - UNIVARIATE ANALYSIS - Credit Bureau data


ggplot(CB_Data_NoWoe,aes(x=factor(Presence.of.open.auto.loan)))+geom_bar(stat="count") #Very few applicants have Auto Loan

boxplot(CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months)
median(CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months,na.rm = T)
#Limited or Few outliers - Median No.of.times.90.DPD.or.worse.in.last.6.months - 0

boxplot(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
median(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,na.rm = T)
#Outliers are present - Median No.of.Inquiries.in.last.12.months..excluding.home...auto.loans - 3

boxplot(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
median(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,na.rm = T)
#Outliers are present - Median No.of.Inquiries.in.last.6.months..excluding.home...auto.loans - 1

boxplot(CB_Data_NoWoe$Avgas.CC.Utilization.in.last.12.months,names=c("Avgas.CC.Utilization.in.last.12.months"))
median(CB_Data_NoWoe$Avgas.CC.Utilization.in.last.12.months,na.rm = T)
#Limited or Few outliers - Median number of avg credit card usage - 15

boxplot(CB_Data_NoWoe$No.of.trades.opened.in.last.6.months)
median(CB_Data_NoWoe$No.of.trades.opened.in.last.6.months,na.rm = T)
#Outliers are present - Median No.of.trades.opened.in.last.6.months - 2

boxplot(CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months)
median(CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months,na.rm = T)
#Outliers are present - Median No.of.times.90.DPD.or.worse.in.last.12.months - 0

boxplot(CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months)
median(CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months,na.rm = T)
#Outliers are presnet - Median No.of.times.60.DPD.or.worse.in.last.12.months - 0

boxplot(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months)
median(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months,na.rm = T)
#Outliers are present - Median No.of.times.60.DPD.or.worse.in.last.6.months - 0

boxplot(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months)
median(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months,na.rm = T)
#Outliers are present - Median No.of.times.30.DPD.or.worse.in.last.6.months - 0

boxplot(CB_Data$No.of.times.30.DPD.or.worse.in.last.12.months)
median(CB_Data$No.of.times.30.DPD.or.worse.in.last.12.months,na.rm = T)
#Outliers are present - Median No.of.times.30.DPD.or.worse.in.last.12.months - 0

boxplot(CB_Data$No.of.trades.opened.in.last.12.months)
median(CB_Data$No.of.trades.opened.in.last.12.months,na.rm = T)
#Outliers are present - Median No.of.trades.opened.in.last.12.months - 0

boxplot(CB_Data$Total.No.of.Trades)
median(CB_Data$Total.No.of.Trades,na.rm = T)
#Outliers are present - Median Total.No.of.Trades - 6

boxplot(CB_Data$No.of.PL.trades.opened.in.last.12.months)
median(CB_Data$No.of.PL.trades.opened.in.last.12.months,na.rm = T)
#Outliers are present - Median No.of.PL.trades.opened.in.last.12.months - 2

boxplot(CB_Data$No.of.PL.trades.opened.in.last.6.months)
median(CB_Data$No.of.PL.trades.opened.in.last.6.months,na.rm = T)
#Outliers are present - Median No.of.PL.trades.opened.in.last.6.months - 1

# Exploratory data analysis - BIVARIATE ANALYSIS - Demographic data

ggplot(DG_Data_NoWoe,aes(x=Marital.Status..at.the.time.of.application.,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(DG_Data_NoWoe,aes(y=Age,x=factor(Performance.Tag)))+geom_boxplot()
ggplot(DG_Data_NoWoe,aes(y=Income,x=factor(Performance.Tag)))+geom_boxplot()
ggplot(DG_Data_NoWoe,aes(x=Type.of.residence,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(DG_Data_NoWoe,aes(x=Education,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(DG_Data_NoWoe,aes(y=No.of.months.in.current.residence,x=factor(Performance.Tag)))+geom_boxplot()
ggplot(DG_Data_NoWoe,aes(y=No.of.months.in.current.company,x=factor(Performance.Tag)))+geom_boxplot()
ggplot(DG_Data_NoWoe,aes(x=No.of.dependents,fill=factor(Performance.Tag)))+geom_bar()
ggplot(DG_Data_NoWoe,aes(x=Gender,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(DG_Data_NoWoe,aes(x=Profession,fill=factor(Performance.Tag)))+geom_bar(position="fill")

# Exploratory data analysis - BIVARIATE ANALYSIS - Credit Bureau data


ggplot(CB_Data_NoWoe,aes(x=Presence.of.open.home.loan,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.times.90.DPD.or.worse.in.last.6.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=CB_Data$Total.No.of.Trades,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.times.90.DPD.or.worse.in.last.12.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.times.60.DPD.or.worse.in.last.6.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.times.30.DPD.or.worse.in.last.6.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.times.60.DPD.or.worse.in.last.12.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.times.30.DPD.or.worse.in.last.12.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data_NoWoe,aes(x=Presence.of.open.auto.loan,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data_NoWoe,aes(x=No.of.trades.opened.in.last.6.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.trades.opened.in.last.12.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.PL.trades.opened.in.last.6.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=CB_Data$No.of.PL.trades.opened.in.last.12.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")


# Merging the data and finding correlation

Merged_Data <- merge(DG_Data, CB_Data, by ="Application.ID",all=F)
head(Merged_Data)

#Excluding variables (Application ID and Performance Tag)
Merged_Data <- Merged_Data[,-c(1,7,8,31,32)]

numdata <- sapply(Merged_Data,is.numeric)
corrdata <- cor(Merged_Data[,numdata])

#Plotting the data after correlation
corrplot(corrdata, type = "full",tl.pos = "dt",method = "circle", tl.cex = 0.5, 
         tl.col = 'Blue', order = "hclust", diag = TRUE)


### EDA Findings :

# 1.  Demographic Data alone cannot be used for predicting "Default" customers.
# 1.1 Demographic Data has very few significant variables
# 1.2 Correlation does not exist (numeric variables)

# 2.  Credit Bureau Data has more significant variable for predicting "Default" customers
# 2.2 There are a total of 9 significant variables
# 2.3 Credit bureau data has significant positive correlation (with some numeric variables).



# Performing Outlier treatment (Both Demographics / Credit Data)


summary(DG_Data$Age) 
levels(factor(DG_Data$Age))

# Excluding all applicants with age < 18
age_less_18 <- DG_Data[which(DG_Data$Age < 18),]
DG_Data <- DG_Data[-which(DG_Data$Age < 18),]

DG_Data_Bckp<-DG_Data
CB_Data_Bckp<-CB_Data

# Continuous data - Ouliers check.

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
CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months[which(CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months>2)]<-2

quantile(CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months,seq(0,1,0.01))
CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months[which(CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months>2)]<-2

quantile(CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months,seq(0,1,0.01))
CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months[which(CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months > 1)]<- 1

quantile(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months,seq(0,1,0.01))
CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months[which(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months > 2)]<- 2

quantile(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months,seq(0,1,0.01))
CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months[which(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months> 2)]<- 2

quantile(CB_Data$No.of.PL.trades.opened.in.last.6.months,seq(0,1,0.01))
CB_Data$No.of.PL.trades.opened.in.last.6.months[which(CB_Data$No.of.PL.trades.opened.in.last.6.months> 4)]<- 4

quantile(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,seq(0,1,0.01))
CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.> 6)]<- 6

# Taking backup of the datasets
DG_Model_Data <- DG_Data
CB_Model_Data <- CB_Data



# Demographic data - Data prep before Model building and Evaluation

#Convert Performance.Tag to factor.
DG_Data$Performance.Tag <- as.factor(DG_Data$Performance.Tag)

# Performing scaling to balance the data (excluding application id and dependent variable)
DG_Data[,-c(1,7)]<-data.frame(scale(DG_Data[,-c(1,7)]))

# Split into test and train datasets
set.seed(100)
indices <- sample.split(DG_Data$Performance.Tag, SplitRatio = 0.70)
DG_Data_Train <- DG_Data[indices, ]
DG_Data_Test <- DG_Data[!indices, ]

#Perform Balancing
DG_Data_Train_ROSE <- ROSE(Performance.Tag ~ ., DG_Data_Train,seed=1)$data
table(DG_Data_Train_ROSE$Performance.Tag)
prop.table(table(DG_Data_Train_ROSE$Performance.Tag))

DG_Data_FINAL <- DG_Data_Train_ROSE

# Excluding Application.ID and Changed.Perf.Tag - Train Data
DG_Data_FINAL$Application.ID <- NULL
DG_Data_FINAL$Changed.Perf.Tag <- NULL

# Excluding Application.ID and Reverse.Performance.Tag - Test Data
DG_Data_Test_other <- DG_Data_Test
DG_Data_Test_other$Application.ID <- NULL
DG_Data_Test_other$Reverse.Performance.Tag <- NULL


#Logistic Regression : Model Building - Demographic Data ONLY

DG_model_lr1 = glm(Performance.Tag ~ ., data = DG_Data_FINAL, family = "binomial")
summary(DG_model_lr1)

# Using stepwise AIC to exclude insignificant variables 

DG_model_lr2<- stepAIC(DG_model_lr1, direction="both")
summary(DG_model_lr2)
vif(DG_model_lr2)

DG_model_FINAL <- DG_model_lr2
DG_Pred = predict(DG_model_FINAL, type = "response", newdata = DG_Data_Test_other[,-6])
summary(DG_Pred)



#Logistic Regression : Model Eval

pred_val <- prediction(DG_Pred,DG_Data_Test_other$Performance.Tag)
evalSS<-performance(pred_val,"sens","spec")
evalACC<-performance(pred_val,'acc')
plot(evalACC)

sensitivity <- evalSS@y.values[[1]]
cutoff <- evalSS@alpha.values[[1]]
specificity<- evalSS@x.values[[1]]
accuracy<-evalACC@y.values[[1]]
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

#Cutoff = 0.5 - sensitivity=55% - specificity=55% - Accuracy=55%
#The above scores indicate below average / low model performance for Demographics Data.


# Merged Data Model Building - Data prep before Model building and Evaluation


#Excluding Performance tag and Changed Performance tag fields from either Demographics or Credit Data
CB_Data$Performance.Tag <- NULL
CB_Data$Changed.Perf.Tag <- NULL


combined_data <- merge(DG_Data, CB_Data, by ="Application.ID",all=F)
dim(combined_data)

#Excluding Performance tag and Changed Performance tag fields
combined_data$Changed.Perf.Tag<-NULL
combined_data$Application.ID<-NULL

#Data Scaling
combined_data[,-6]<-data.frame(scale(combined_data[,-6]))

#Splitting data into train and test
set.seed(100)
split_indices <- sample.split(combined_data$Performance.Tag, SplitRatio = 0.70)
combined_train <- combined_data[split_indices, ]
combined_test <- combined_data[!split_indices, ]

table(combined_train$Performance.Tag)
#   0     1 
# 46797   2062

#Data Balancing
combined_train_ROSE <- ROSE(Performance.Tag ~ ., combined_train,seed=1)$data
table(combined_train_ROSE$Performance.Tag)
#    0    1 
#  24429 24430  

prop.table(table(combined_train_ROSE$Performance.Tag))
# 0         1 
# 0.499 0.50


#Logistic Regression : Model Building - Combined / Merged Data

combined_model_lr1 = glm(Performance.Tag ~ ., data = combined_train_ROSE, family = "binomial")
summary(combined_model_lr1)

# stepwise AIC to exclude insignificant variables 

combined_model_lr2<- stepAIC(combined_model_lr1, direction="both")
summary(combined_model_lr2)
vif(combined_model_lr2)

#AIC:63942

#removing Age variable with lesser p value
combined_model_lr3<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                  No.of.months.in.current.company + woe.Education.binned + 
                  woe.Marital.Status..at.the.time.of.application..binned + 
                  No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.6.months + 
                  No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                  No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                  No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + 
                  No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                  Presence.of.open.home.loan + Outstanding.Balance + Total.No.of.Trades, 
                family = "binomial", data = combined_train_ROSE)
summary(combined_model_lr3)
#AIC: 63943

#removing woe.Marital.Status..at.the.time.of.application..binned  variable with lesser p value
combined_model_lr4<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                  No.of.months.in.current.company + woe.Education.binned +
                  No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.6.months + 
                  No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                  No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                  No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + 
                  No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                  Presence.of.open.home.loan + Outstanding.Balance + Total.No.of.Trades, 
                family = "binomial", data = combined_train_ROSE)
summary(combined_model_lr4)
vif(combined_model_lr4)
#AIC: 63945
#All variable are significant and vif values almost equal to 2 or lesser

final_model_lr <- combined_model_lr4

#Logistic Regression : Model Eval


combined_test_predict = predict(final_model_lr, type = "response",newdata = combined_test[,-6])
summary(combined_test_predict)

final_pred <- prediction(combined_test_predict,combined_test$Performance.Tag)
evalSS<-performance(final_pred,"sens","spec")
evalACC<-performance(final_pred,'acc')
plot(evalACC)

sensitivity <- evalSS@y.values[[1]]
cutoff <- evalSS@alpha.values[[1]]
specificity<- evalSS@x.values[[1]]
accuracy<-evalACC@y.values[[1]]
plot(cutoff,sensitivity,col="green")
lines(cutoff,specificity,col="blue")
lines(cutoff,accuracy,col="red")
legend("bottomright", legend=c("sensitivity","accuracy","specificity"),
       col=c("red","blue","green"), lty=1:2, cex=0.8)

abline(v =0.51)
matrix<-data.frame(cbind(sensitivity,specificity,accuracy,cutoff))

final_matrix<-matrix[which(matrix$cutoff>0.1&matrix$cutoff<1),]
matrix<-sapply(final_matrix,function(x) round(x,2))
matrix<-data.frame(matrix)
head(matrix[which(matrix$accuracy==matrix$sensitivity&matrix$sensitivity==matrix$specificity),])

#cutoff = 0.51 - sensitivity=63% - specificity=63% - accuracy=63%

cutoff_val <- factor(ifelse(combined_test_predict >=0.51, "1", "0"))
actual_val <- factor(ifelse(combined_test$Performance.Tag==1, "1", "0"))
confusion_mat <- confusionMatrix(cutoff_val, actual_val, positive = "0")
confusion_mat



# Application Scorecard for merged data

population = predict(final_model_lr, type = "response", newdata = combined_data[,-6])

#computing odds for good. Since the probability computed is for rejection (bad cusotmers),  Odd(good) =  (1-P(bad))/P(bad)
Odds_for_good<-sapply(population,function(x) (1-x)/x)

#computing  ln(odd(good))
ln_Odds_for_good<-sapply(Odds_for_good,function(x)log(x))

#Using the following formula for computing application score card
#400 + slope * (ln(odd(good)) - ln(10)) where slope is 20/(ln(20)-ln(10))
slope<-20/(log(20)-log(10))

application_score_card<-sapply(ln_Odds_for_good,function(x) 400 + slope * (x - log(10)))
head(application_score_card)

#making dataframe with score card
score_card_df<-cbind(combined_data,application_score_card)
summary(application_score_card)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 297.6   326.2   338.4   337.8   353.5   362.3 
#mean score for approved customers is 337.8
#High score is inversly proportional to Defaulting


###CUTOFF SCORE FOR ACCEPTING OR REJECTING AN APPLICATION

#cutoff selected for logistic regression model was 0.51
final_score<- 400 + (slope * (log((1-0.51)/0.51) - log(10)))
final_score
#CUTOFF SCORE is 332.40

ggplot(score_card_df,aes(application_score_card))+geom_histogram()
#Histogram clearly shows that the application score of applicants are below 338  are few which meets our expectation.
boxplot(application_score_card)

#No.of applicants below score 332.40
length(which(application_score_card<332.40))
#27100

#No.of applicants above score 332.40
length(which(application_score_card>332.40))
#42699

#Calculating Bank Profit

##### Profit with the final model

test_pred_log_final = predict(final_model_lr, type = "response", newdata = combined_data[,-6])
test_cutoff <- factor(ifelse(test_pred_log_final >=0.51, "1", "0"))
test_actual <- factor(ifelse(combined_data$Performance.Tag==1, "1", "0"))
conf_final <- confusionMatrix(test_cutoff, test_actual, positive = "0")
conf_final

#          Reference
#Prediction     0     1
#     0       41566  1115
#     1       25287  1831
#Profit with model will be total profit due to each true positive and each true negative


#Final Conclusion :

#1. A good model accuracy was achieved using the "Logistic Regression" Model
#Final Cutoff = 0.51
#sensitivity=63%
#specificity=63%
#Accuracy = 63% 

#2. Significant variables:
#Age 
#Income
#No.of.months.in.current.residence 
#No.of.months.in.current.company 
#woe.Profession.binned
#woe.Education.binned 
#No.of.times.60.DPD.or.worse.in.last.6.months
#No.of.times.30.DPD.or.worse.in.last.6.months
#No.of.times.90.DPD.or.worse.in.last.12.months
#No.of.times.60.DPD.or.worse.in.last.12.months 
#No.of.times.30.DPD.or.worse.in.last.12.months
#Avgas.CC.Utilization.in.last.12.months
#No.of.trades.opened.in.last.6.months
#No.of.PL.trades.opened.in.last.6.months
#No.of.PL.trades.opened.in.last.12.months

#3 Application scorecard is built on logistic regression model.
# Cutoff score = 332.40


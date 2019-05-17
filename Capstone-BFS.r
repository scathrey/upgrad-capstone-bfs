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

nrow(CB_Data)
nrow(DG_Data)
# Final cleaned up observations - 69864 obs


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


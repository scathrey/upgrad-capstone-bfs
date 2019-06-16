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

#Demo and Credit rejected records : Performance.Tag.
CB_Data_Reject <- CB_Data[which(is.na(CB_Data$Performance.Tag)),]
DG_Data_Reject <- DG_Data[which(is.na(DG_Data$Performance.Tag)),]

#Exluding missing entries
CB_Data<-CB_Data[!(is.na(CB_Data$Performance.Tag)),]
DG_Data<-DG_Data[!(is.na(DG_Data$Performance.Tag)),]

nrow(CB_Data)
nrow(DG_Data)
# Final cleaned up observations - 69864 obs

# Loading ggplot2 library
library(ggplot2)

#Univariate Analysis

# Exploratory data analysis - UNIVARIATE ANALYSIS - Demographic data

ggplot(DG_Data,aes(x=Profession))+geom_bar(fill="green") #Profession type : "Salaried" tops the applicants profession

ggplot(DG_Data,aes(x=Marital.Status..at.the.time.of.application.))+geom_bar(fill="blue") # Marital Status : "Married" status" applicants are more vs "Single"

ggplot(DG_Data,aes(x=Education))+geom_bar(fill="red") #Education level : "professional" / "Masters" tops the applicants edu level

ggplot(DG_Data,aes(x=Type.of.residence))+geom_bar(fill="purple") #Residence type : "Rented" tops the applicants type of residence

boxplot(DG_Data$Age)
median(DG_Data$Age) #Limited or few outliers found - Median Age of applicants - 45 years

boxplot(DG_Data$Income)
median(DG_Data$Income) #No outliers found - Median income of applicants - 27 units

boxplot(DG_Data$No.of.months.in.current.company)
median(DG_Data$No.of.months.in.current.company) #Limited or few outliers found - Median No.of.months.in.current.company - 34

boxplot(DG_Data$No.of.months.in.current.residence)
median(DG_Data$No.of.months.in.current.residence) #No outliers found - Median No.of.months.in.current.residence - 10


# Exploratory data analysis - UNIVARIATE ANALYSIS - Credit Bureau data


ggplot(CB_Data,aes(x=factor(Presence.of.open.auto.loan)))+geom_bar(stat="count") #Very few applicants have Auto Loan

boxplot(CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months)
median(CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months,na.rm = T)
#Limited or Few outliers - Median No.of.times.90.DPD.or.worse.in.last.6.months - 0

boxplot(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
median(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,na.rm = T)
#Outliers are present - Median No.of.Inquiries.in.last.12.months..excluding.home...auto.loans - 3

boxplot(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
median(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,na.rm = T)
#Outliers are present - Median No.of.Inquiries.in.last.6.months..excluding.home...auto.loans - 1

boxplot(CB_Data$Avgas.CC.Utilization.in.last.12.months,names=c("Avgas.CC.Utilization.in.last.12.months"))
median(CB_Data$Avgas.CC.Utilization.in.last.12.months,na.rm = T)
#Limited or Few outliers - Median number of avg credit card usage - 15

boxplot(CB_Data$No.of.trades.opened.in.last.6.months)
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
#Outliers are present - Median No.of.PL.trades.opened.in.last.6.months -


# Exploratory data analysis - BIVARIATE ANALYSIS - Demographic data

ggplot(DG_Data,aes(x=Marital.Status..at.the.time.of.application.,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(DG_Data,aes(y=Age,x=factor(Performance.Tag)))+geom_boxplot()
ggplot(DG_Data,aes(y=Income,x=factor(Performance.Tag)))+geom_boxplot()
ggplot(DG_Data,aes(x=Type.of.residence,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(DG_Data,aes(x=Education,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(DG_Data,aes(y=No.of.months.in.current.residence,x=factor(Performance.Tag)))+geom_boxplot()
ggplot(DG_Data,aes(y=No.of.months.in.current.company,x=factor(Performance.Tag)))+geom_boxplot()
ggplot(DG_Data,aes(x=No.of.dependents,fill=factor(Performance.Tag)))+geom_bar()
ggplot(DG_Data,aes(x=Gender,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(DG_Data,aes(x=Profession,fill=factor(Performance.Tag)))+geom_bar(position="fill")

# Exploratory data analysis - BIVARIATE ANALYSIS - Credit Bureau data


ggplot(CB_Data,aes(x=Presence.of.open.home.loan,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.times.90.DPD.or.worse.in.last.6.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=CB_Data$Total.No.of.Trades,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.times.90.DPD.or.worse.in.last.12.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.times.60.DPD.or.worse.in.last.6.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.times.30.DPD.or.worse.in.last.6.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.times.60.DPD.or.worse.in.last.12.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.times.30.DPD.or.worse.in.last.12.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=Presence.of.open.auto.loan,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.trades.opened.in.last.6.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.trades.opened.in.last.12.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=No.of.PL.trades.opened.in.last.6.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")
ggplot(CB_Data,aes(x=CB_Data$No.of.PL.trades.opened.in.last.12.months,fill=factor(Performance.Tag)))+geom_bar(position="fill")

#Outliers Treatment and some more bivariate analysis after changing levels of categorical variables and 
#binning for continious variables

# Age
#----------------------------------------
quantile(DG_Data$Age,seq(0,1,0.01))
summary(DG_Data$Age)

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

#-----Next Variable is "Gender"
#--------------------------------------------------------

# Checking the levels of the gender

levels(DG_Data$Gender)

# Let's replace Unknown level to M

levels(DG_Data$Gender)[1] <- "M"

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
# Checking Marital status
#---------------------------------------------------------

summary(DG_Data$Marital.Status..at.the.time.of.application.)

# Let's replace Unknown level to married

levels(DG_Data$Marital.Status..at.the.time.of.application.)[1] <- "Married"

# Plotting marital status

plot_performance(DG_Data$Marital.Status..at.the.time.of.application.,"marital")

#------------------------------------------------------------------
# No. of dependents
boxplot(DG_Data$No.of.dependents)
summary(DG_Data$No.of.dependents)
plot_performance(DG_Data$No.of.dependents,"No.of.dependents")
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#Income
#-----------------------------------------------------------------
summary(DG_Data$Income)

# So let's check the percentile distribution of income 

quantile(DG_Data$Income,seq(0,1,0.01))

# Binning the income variable and store it into "binning.income".

DG_Data$binning.income <- as.factor(cut(DG_Data$Income, breaks = c(10, 20, 30, 40, 50, 60)))


plot_performance(DG_Data$binning.income,"Income")

#---------------------------------------------------------
# Let's see the education variables
#---------------------------------------------------------

levels(DG_Data$Education)

# Reducing the levels of education variable, change spaces and others to uneducated

levels(DG_Data$Education)[c(1,4)] <- "Uneducated"

# # Let's again check the education plot
# 
plot_performance(DG_Data$Education,"Education_levels")

#------------------------------------------------------------
#Check Profession
#-----------------------------------------------------------
levels(DG_Data$Profession)

levels(DG_Data$Profession)[1] <- "SAL"

# Let's again check the profession plot
# 
plot_performance(DG_Data$Profession,"Profession")

#-----------------------------------------------
# Check Type of residence
#-----------------------------------------------
levels(DG_Data$Type.of.residence)

#Change spaces and Others to Rented

levels(DG_Data$Type.of.residence)[c(1,4)] <- "Rented"
# 
plot_performance(DG_Data$Type.of.residence,"Type.of.residence")

#-------------------------------------------------
# check no of months in current residence
#------------------------------------------------

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

#----------------------------------------------
# check no of months in current company
#------------------------------------------------

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

#------------------------------------------------------------------
# Outlier treatment of credit data and some more bivariate analysis
#------------------------------------------------------------------

#---------------------------------------------------
# check No.of.times.90.DPD.or.worse.in.last.6.months
#---------------------------------------------------
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
boxplot(CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months)
CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months[which(CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months > 1)]<- 1
plot_cb_performance(CB_Data$No.of.times.90.DPD.or.worse.in.last.6.months,"No.of.times.90.DPD.or.worse.in.last.6.months")

#---------------------------------------------------
# check No.of.times.60.DPD.or.worse.in.last.6.months
#----------------------------------------------------
sum(is.na(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months))
summary(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months)
boxplot(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months)
CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months[which(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months > 2)]<- 2
plot_cb_performance(CB_Data$No.of.times.60.DPD.or.worse.in.last.6.months,"No.of.times.60.DPD.or.worse.in.last.6.months")

#----------------------------------------------------
# check No.of.times.30.DPD.or.worse.in.last.6.months
#----------------------------------------------------
sum(is.na(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months))
summary(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months)
boxplot(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months)
CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months[which(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months> 2)]<- 2
plot_cb_performance(CB_Data$No.of.times.30.DPD.or.worse.in.last.6.months,"No.of.times.30.DPD.or.worse.in.last.6.months")

#------------------------------------------------------
# check No.of.times.90.DPD.or.worse.in.last.12.months
#-------------------------------------------------------
sum(is.na(CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months))
summary(CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months)
CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months[which(CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months>2)]<-2
plot_cb_performance(CB_Data$No.of.times.90.DPD.or.worse.in.last.12.months,"No.of.times.90.DPD.or.worse.in.last.12.months")

#----------------------------------------------------
# check No.of.times.60.DPD.or.worse.in.last.12.months
#-----------------------------------------------------
sum(is.na(CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months))
summary(CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months)
CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months[which(CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months>2)]<-2
plot_cb_performance(CB_Data$No.of.times.60.DPD.or.worse.in.last.12.months,"No.of.times.60.DPD.or.worse.in.last.12.months")

#----------------------------------------------------
# check No.of.times.30.DPD.or.worse.in.last.12.months
#----------------------------------------------------
sum(is.na(CB_Data$No.of.times.30.DPD.or.worse.in.last.12.months))
summary(CB_Data$No.of.times.30.DPD.or.worse.in.last.12.months)
quantile(CB_Data$No.of.times.30.DPD.or.worse.in.last.12.months,seq(0,1,0.01))
CB_Data$No.of.times.30.DPD.or.worse.in.last.12.months[which(CB_Data$No.of.times.30.DPD.or.worse.in.last.12.months > 5)]<- 5
plot_cb_performance(CB_Data$No.of.times.30.DPD.or.worse.in.last.12.months,"No.of.times.30.DPD.or.worse.in.last.12.months")

#---------------------------------------------
# check Avgas.CC.Utilization.in.last.12.months
#---------------------------------------------
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

#---------------------------------------------
# check No.of.trades.opened.in.last.6.months
#--------------------------------------------
sum(is.na(CB_Data$No.of.trades.opened.in.last.6.months))
summary(CB_Data$No.of.trades.opened.in.last.6.months)
boxplot(CB_Data$No.of.trades.opened.in.last.6.months)
CB_Data$No.of.trades.opened.in.last.6.months[which(CB_Data$No.of.trades.opened.in.last.6.months> 6)]<- 6
plot_cb_performance(CB_Data$No.of.trades.opened.in.last.6.months,"No.of.trades.opened.in.last.6.months")

#---------------------------------------------
# check No.of.trades.opened.in.last.12.months
#---------------------------------------------
sum(is.na(CB_Data$No.of.trades.opened.in.last.12.months))
summary(CB_Data$No.of.trades.opened.in.last.12.months)
quantile(CB_Data$No.of.trades.opened.in.last.12.months,seq(0,1,0.01))
CB_Data$No.of.trades.opened.in.last.12.months[which(CB_Data$No.of.trades.opened.in.last.12.months>21)]<-21
# Binning the No.of.trades.opened.in.last.12.months and store it into "binning.trades_12_months".

CB_Data$binning.trades_12_months <- as.factor(cut(CB_Data$No.of.trades.opened.in.last.12.months, breaks = c(0, 5, 10, 15, 20, 25)))
plot_cb_performance(CB_Data$binning.trades_12_months,"No.of.trades.opened.in.last.12.months")

#----------------------------------------------
# check No.of.PL.trades.opened.in.last.6.months
#----------------------------------------------
sum(is.na(CB_Data$No.of.PL.trades.opened.in.last.6.months))
summary(CB_Data$No.of.PL.trades.opened.in.last.6.months)
quantile(CB_Data$No.of.PL.trades.opened.in.last.6.months,seq(0,1,0.01))
CB_Data$No.of.PL.trades.opened.in.last.6.months[which(CB_Data$No.of.PL.trades.opened.in.last.6.months> 4)]<- 4

plot_cb_performance(CB_Data$No.of.PL.trades.opened.in.last.6.months,"No.of.PL.trades.opened.in.last.6.months")

#------------------------------------------
# No.of.PL.trades.opened.in.last.12.months
#------------------------------------------
sum(is.na(CB_Data$No.of.PL.trades.opened.in.last.12.months))
summary(CB_Data$No.of.PL.trades.opened.in.last.12.months)
quantile(CB_Data$No.of.PL.trades.opened.in.last.12.months,seq(0,1,0.01))
CB_Data$No.of.PL.trades.opened.in.last.12.months[which(CB_Data$No.of.PL.trades.opened.in.last.12.months > 9)]<- 9

plot_cb_performance(CB_Data$No.of.PL.trades.opened.in.last.12.months,"No.of.PL.trades.opened.in.last.12.months")

#-----------------------------------------------------------------
# No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
sum(is.na(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))
summary(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
quantile(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,seq(0,1,0.01))
CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.> 6)]<- 6
plot_cb_performance(CB_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")

#------------------------------------------------------------------
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
#-------------------------------------------------------------------
sum(is.na(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))
summary(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
boxplot(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
quantile(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,seq(0,1,0.01))
CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.[which(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. >15)]<- 15

plot_cb_performance(CB_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")

#------------------------------
# Presence.of.open.home.loan
#------------------------------
sum(is.na(CB_Data$Presence.of.open.home.loan))
summary(CB_Data$Presence.of.open.home.loan)

CB_Data$Presence.of.open.home.loan <- as.factor(CB_Data$Presence.of.open.home.loan)
levels(CB_Data$Presence.of.open.home.loan)
plot_cb_performance(CB_Data$Presence.of.open.home.loan,"Presence.of.open.home.loan")

#-------------------------------------
# check Outstanding.Balance
#-------------------------------------
sum(is.na(CB_Data$Outstanding.Balance))
summary(CB_Data$Outstanding.Balance)
boxplot(CB_Data$Outstanding.Balance)
# Binning the Outstanding.Balance and store it into "Outstanding.Balance".

CB_Data$binning.Outstanding.Balance <- as.factor(cut(CB_Data$Outstanding.Balance, breaks = c(0, 10000, 100000, 1000000, 3000000, 5500000)))
plot_cb_performance(CB_Data$binning.Outstanding.Balance,"Outstanding.Balance")

#-----------------------
# Total.No.of.Trades
#-----------------------
sum(is.na(CB_Data$Total.No.of.Trades))
summary(CB_Data$Total.No.of.Trades)
quantile(CB_Data$Total.No.of.Trades,seq(0,1,0.01))
CB_Data$Total.No.of.Trades[which(CB_Data$Total.No.of.Trades>31)]<-31
plot_cb_performance(CB_Data$Total.No.of.Trades,"Total.Trades")

#----------------------------
# Presence.of.open.auto.loan
#----------------------------
sum(is.na(CB_Data$Presence.of.open.auto.loan))
summary(CB_Data$Presence.of.open.auto.loan)

CB_Data$Presence.of.open.auto.loan <- as.factor(CB_Data$Presence.of.open.auto.loan)
levels(CB_Data$Presence.of.open.auto.loan)
plot_cb_performance(CB_Data$Presence.of.open.auto.loan,"Presence.of.open.auto.loan")

str(DG_Data)
str(CB_Data)


# Delete binning columns
DG_Data <- DG_Data[,-c(13:16)]
CB_Data <- CB_Data[,-c(20:22)]


#Merging demographic and credit bureau data

Merged_Data <- merge(DG_Data,CB_Data, by="Application.ID", all = F)

str(Merged_Data)
#Remove one Performance Tag
Merged_Data <- Merged_Data[,-c(12)]
colnames(Merged_Data)[29]<- "Perf.Tag"

numdata <- sapply(Merged_Data,is.numeric)
corrdata <- cor(Merged_Data[,numdata])

library(corrplot)
#Plotting the data after correlation
corrplot(corrdata, type = "full",tl.pos = "dt",method = "circle", tl.cex = 0.5, 
         tl.col = 'Blue', order = "hclust", diag = TRUE)

# Weight Of Evidence Analaysis (WOE) and Information Value (IV)


# WOE - Replace missing values in the data
# IV - Find the most significant variables in the data

# ************** Demographic data *************
# Information value (IV) Analysis
str(DG_Data)
IV_demo <- create_infotables(data=DG_Data, y="Performance.Tag", parallel = TRUE)
IV_demo$Summary

binning_demo <- woe.binning(DG_Data, 'Performance.Tag', DG_Data)
df.with.binned.vars.added <- woe.binning.deploy(DG_Data, binning_demo,
                                                min.iv.total=0.02,
                                                add.woe.or.dum.var='woe')

str(df.with.binned.vars.added)
demo_woe <- df.with.binned.vars.added[,-c(1:11)]
str(demo_woe)
#Keep only woe values with min.iv 0.02
demo_woe <- demo_woe[,-c(2,4,6)]
demo_woe$Performance.Tag <- as.factor(ifelse(demo_woe$Performance.Tag == 1, "yes", "no"))

set.seed(1)

split_indices <- sample.split(demo_woe$Performance.Tag, SplitRatio = 0.70)

train_demo <- demo_woe[split_indices, ]
test_demo <- demo_woe[!split_indices, ]

nrow(train_demo)/nrow(DG_Data)

nrow(test_demo)/nrow(DG_Data)
str(test_demo)

#Treating the data imbalance
library(ROSE)
train_demo <- ROSE(Performance.Tag ~ ., train_demo,seed=1)$data
library(DMwR)

train_demo$Performance.Tag <- as.factor(train_demo$Performance.Tag)
prop.table(table(train_demo$Performance.Tag))
str(train_demo)
### Building the Logistic Regression Model for Demographic data
library(MASS)

library(car)

logistic_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train_demo)

summary(logistic_1)

#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

logistic_2 <- stepAIC(logistic_1, direction = "both")
summary(logistic_2)
vif(logistic_2)

DG_model_FINAL <- logistic_2
#Logistic Regression : Model Eval
predictions_logit_demo <- predict(DG_model_FINAL, newdata = test_demo, type = "response")
# Cutoff for logistic regression demo to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response_demo <- as.factor(ifelse(predictions_logit_demo >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response_demo, test_demo$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_demo <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_demo) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_demo)
}

#---------------------------------------------------------    


# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_demo = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_demo[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_demo[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_demo[,2],col="darkgreen",lwd=2)
lines(s,OUT_demo[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_demo <- s[which(abs(OUT_demo[,1]-OUT_demo[,2])<0.03)]


# Let's use the probability cutoff of 49.5%.

predicted_response_demo <- factor(ifelse(predictions_logit_demo >= 0.4950, 'yes', 'no'))
# Creating confusion matrix for identifying the model evaluation.
conf_demo <- confusionMatrix(predicted_response_demo, test_demo$Performance.Tag, positive = 'yes')

conf_demo

#Cutoff = 0.495 - sensitivity=59% - specificity=57% - Accuracy=58%
#The above scores indicate below average / low model performance for Demographics Data.


### EDA Findings :

# 1.  Demographic Data alone cannot be used for predicting "Default" customers.
# 1.1 Demographic Data has very few significant variables
# 1.2 Correlation does not exist (numeric variables)

# 2.  Credit Bureau Data has more significant variable for predicting "Default" customers
# 2.2 There are a total of 9 significant variables
# 2.3 Credit bureau data has significant positive correlation (with some numeric variables).

#-------------------------------------------------------------
#IV ans WOE Analysis for combined Demographic and Credit Data
#-------------------------------------------------------------
# Information value (IV) Analysis
IV_credit <- create_infotables(data=Merged_Data, y="Perf.Tag", parallel = TRUE)
IV_credit$Summary
str(Merged_Data)
#--------------------------------------------------------- 
# WOE analysis and using only min iv 0.02
#--------------------------------------------
binning <- woe.binning(Merged_Data, 'Perf.Tag', Merged_Data)
df.with.binned.vars.added <- woe.binning.deploy(Merged_Data, binning,
                                                min.iv.total=0.02,
                                                add.woe.or.dum.var='woe')

str(df.with.binned.vars.added)

#--Keeping only woe values
data_woe <- df.with.binned.vars.added[,-c(30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62)]
data_woe$No.of.PL.trades.opened.in.last.12.months <- data_woe$woe.No.of.PL.trades.opened.in.last.12.months.binned
data_woe$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- data_woe$woe.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..binned
data_woe$No.of.trades.opened.in.last.12.months <- data_woe$woe.No.of.trades.opened.in.last.12.months.binned
data_woe$Avgas.CC.Utilization.in.last.12.months <- data_woe$woe.Avgas.CC.Utilization.in.last.12.months.binned 
data_woe$No.of.times.30.DPD.or.worse.in.last.6.months <- data_woe$woe.No.of.times.30.DPD.or.worse.in.last.6.months.binned
data_woe$No.of.times.30.DPD.or.worse.in.last.12.months <- data_woe$woe.No.of.times.30.DPD.or.worse.in.last.12.months.binned
data_woe$No.of.PL.trades.opened.in.last.6.months <- data_woe$woe.No.of.PL.trades.opened.in.last.6.months.binned 
data_woe$No.of.times.90.DPD.or.worse.in.last.12.months <- data_woe$woe.No.of.times.90.DPD.or.worse.in.last.12.months.binned
data_woe$No.of.times.60.DPD.or.worse.in.last.6.months <- data_woe$woe.No.of.times.60.DPD.or.worse.in.last.6.months.binned
data_woe$Total.No.of.Trades<- data_woe$woe.Total.No.of.Trades.binned 
data_woe$No.of.times.60.DPD.or.worse.in.last.12.months<- data_woe$woe.No.of.times.60.DPD.or.worse.in.last.12.months.binned
data_woe$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- data_woe$woe.No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..binned
data_woe$No.of.trades.opened.in.last.6.months <- data_woe$woe.No.of.trades.opened.in.last.6.months.binned
data_woe$No.of.times.90.DPD.or.worse.in.last.6.months<- data_woe$woe.No.of.times.90.DPD.or.worse.in.last.6.months.binned
data_woe$No.of.months.in.current.residence<- data_woe$woe.No.of.months.in.current.residence.binned 
data_woe$Income <- data_woe$woe.Income.binned
data_woe$No.of.months.in.current.company<- data_woe$woe.No.of.months.in.current.company.binned
str(data_woe)
data_woe <- data_woe[,-c(1,2,3,4,5,7,8,9,25,26,28)]
data_woe <- data_woe[,-c(19:35)]

library(tidyr)
library(tibble)
library(tidyverse)

#--check NA values
colSums(is.na(data_woe))

# Split the data into train and test data

data_woe$Perf.Tag <- as.factor(ifelse(data_woe$Perf.Tag == 1, "yes", "no"))

set.seed(1)

split_indices <- sample.split(data_woe$Perf.Tag, SplitRatio = 0.70)

train <- data_woe[split_indices, ]
summary(train)
test <- data_woe[!split_indices, ]

nrow(train)/nrow(Merged_Data)

nrow(test)/nrow(Merged_Data)

#--Treating data imbalance
train <- ROSE(Perf.Tag ~ ., train,seed=1)$data
train$Perf.Tag <- as.factor(train$Perf.Tag)
prop.table(table(train$Perf.Tag))
nrow(train)

### Building the Logistic Regression Model for combined data

logistic_1 <- glm(Perf.Tag ~ ., family = "binomial", data = train)

summary(logistic_1)

#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

logistic_2 <- stepAIC(logistic_1, direction = "both")
summary(logistic_2)
vif(logistic_2)

#Removing No.of.times.60.DPD.or.worse.in.last.6.months

logistic_3 <- glm(formula = Perf.Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + 
                    No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                    No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
                    No.of.trades.opened.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                  family = "binomial", data = train)

summary(logistic_3)
vif(logistic_3)

#Remove No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
logistic_4 <- glm(formula = Perf.Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + 
                    No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                    No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
                    No.of.trades.opened.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                  family = "binomial", data = train)
summary(logistic_4)
vif(logistic_4)

#Remove No.of.times.30.DPD.or.worse.in.last.6.months
logistic_5 <- glm(formula = Perf.Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + 
                    No.of.times.90.DPD.or.worse.in.last.12.months + 
                    No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
                    No.of.trades.opened.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                  family = "binomial", data = train)
summary(logistic_5)
vif(logistic_5)

#Remove No.of.trades.opened.in.last.12.months
logistic_6 <- glm(formula = Perf.Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + 
                    No.of.times.90.DPD.or.worse.in.last.12.months + 
                    No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
                    No.of.PL.trades.opened.in.last.12.months + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                  family = "binomial", data = train)
summary(logistic_6)
vif(logistic_6)


logistic_final <- logistic_6
#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(logistic_final, newdata = test, type = "response")
summary(predictions_logit)
#--------------------------------------------------------- 

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 5%.

predicted_response <- factor(ifelse(predictions_logit >= 0.45, 'yes', 'no'))
summary(predicted_response)

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$Perf.Tag, positive = 'yes')

conf

library(ROCR)

pred <- ROCR::prediction(predictions_logit,test$Perf.Tag)
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

#The plot shows optimal cutoff at 0.519

# Let's use the probability cutoff of 51.9%.

predicted_response <- factor(ifelse(predictions_logit >= 0.52,'yes', 'no'))
summary(predicted_response)
levels(predicted_response)
# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$Perf.Tag, positive = 'yes')

conf

# So final figures are:
#Accuracy : 0.6307
#Sensitivity : 0.61991       
#Specificity : 0.63118

library(ROCR)
#on testing  data

performance_measures_test<- performance(pred, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#0.254

#--------------------------------------------------------- 
#Let's use K fold validation with logistic regression on already identified variables

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
model <- train(Perf.Tag ~ 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+ 
                 Avgas.CC.Utilization.in.last.12.months +
                 No.of.times.30.DPD.or.worse.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months+
                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.trades.opened.in.last.6.months + 
                 No.of.months.in.current.residence + Income + 
                 No.of.months.in.current.company ,  data=train, method="glm", family=binomial(),
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

cutoff_cv <- s[which(abs(OUT_cv[,1]-OUT_cv[,2])<0.01)]


# Let's use the probability cutoff of 4.95%.

predicted_response_cv <- factor(ifelse(predictions_logit_cv[,2] >= 0.5148, 'yes', 'no'))
# Creating confusion matrix for identifying the model evaluation.
conf_cv <- confusionMatrix(predicted_response_cv, test$Perf.Tag, positive = 'yes')

conf_cv
# Results
#Sensitivity : 0.62557        
#Specificity : 0.61753  
#Accuracy : 0.6179

#--------------------------------------------------------- 
#Let's use randomforest with logistic regression final model variables
library(randomForest)

# Building the model 
random_rf <- randomForest(Perf.Tag ~ 
                            No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                            No.of.trades.opened.in.last.12.months +Avgas.CC.Utilization.in.last.12.months + 
                            No.of.times.30.DPD.or.worse.in.last.6.months + 
                            No.of.times.30.DPD.or.worse.in.last.12.months + 
                            No.of.PL.trades.opened.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months +
                            No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                            No.of.trades.opened.in.last.6.months + 
                            No.of.months.in.current.residence + Income + 
                            No.of.months.in.current.company, data = train, proximity = F, do.trace = F, mtry = 5)
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

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.02)]


# Let's use the probability cutoff of 42.57%.

predicted_response_rf <- factor(ifelse(rf_pred[,2] >= 0.4257, 'yes', 'no'))
# Creating confusion matrix for identifying the model evaluation.
conf_rf <- confusionMatrix(predicted_response_rf, test$Perf.Tag, positive = 'yes')

conf_rf

#Results
#Sensitivity : 0.62670
#Specificity : 0.61469
#Balanced Accuracy : 0.62070

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

train_num_norm <- as.data.frame(lapply(train[,1:17], normalize ))
test_num_norm <- as.data.frame(lapply(test[,1:17], normalize ))

train_num_norm$Perf.Tag <- as.factor(ifelse(train$Perf.Tag == 'yes', 1, 0))
test_num_norm$Perf.Tag <- as.factor(ifelse(test$Perf.Tag == 'yes', 1, 0))

# build the neural network (NN) formula

set.seed(1234567890)
train_nn <- train_num_norm
test_nn <- test_num_norm
str(train_nn)

# Building the model 
m8 <- nnet(Perf.Tag~., data=train_nn,size=20,maxit=10000,decay=.001, linout=F, trace = F)
table(test_nn$Perf.Tag,predict(m8,newdata=test_nn, type="class"))
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
# Let's use the probability cutoff of 99%.

predicted_response_nn <- factor(ifelse(nn_pred >= 0.99, '1', '0'))
summary(predicted_response_nn)
levels(predicted_response_nn)
# Creating confusion matrix for identifying the model evaluation.
conf_nn <- confusionMatrix(predicted_response_nn, test_nn$Perf.Tag, positive = '1')

conf_nn

#Results of neural network

#Accuracy : 0.43

#--------------------------------------------------------------------
#Evaluating final logistic regression model on rejected population
#Merging demographic and credit bureau rejected data

Rejected_Data <- merge(DG_Data_Reject,CB_Data_Reject, by="Application.ID", all = F)

str(Rejected_Data)
#Remove one Performance Tag
Rejected_Data <- Rejected_Data[,-c(12)]
colnames(Rejected_Data)[29]<- "Perf.Tag"

#--check NA values
colSums(is.na(Rejected_Data))

#There are 35 rows with missing Avgas.CC.Utilization.in.last.12.months, ignore them
Rejected_Data<-Rejected_Data[!(is.na(Rejected_Data$Avgas.CC.Utilization.in.last.12.months)),]

# Predicting probabilities of responding

predictions_reject <- predict(logistic_final, newdata = Rejected_Data , type = "response")
summary(predictions_reject)
#--------------------------------------------------------- 
# Let's use the probability cutoff of 52.0%.

predicted_reject <- factor(ifelse(predictions_reject >= 0.52,'yes', 'no'))
summary(predicted_reject)

# So out of 1390 rejected records, our model predicts default for 1387 records
# This implies 99.78% accuracy on the rejected data

#-------------------------------------------------------------------------------------------
# Predicting probabilities of responding for the whole selected applicants dataset using logistic regression model
# Application Scorecard for merged data

population = predict(logistic_final, type = "response", newdata = data_woe)

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
score_card_df<-cbind(Merged_Data,application_score_card)
summary(application_score_card)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#308.3   322.7   335.7   339.3   358.3   367.9
#mean score for approved customers is 339.3
#Low score is proportional to Defaulting


###CUTOFF SCORE FOR ACCEPTING OR REJECTING AN APPLICATION

#cutoff selected for logistic regression model was 0.52
final_score<- 400 + (slope * (log((1-0.52)/0.52) - log(10)))
final_score
#CUTOFF SCORE is 331.25

#------------------------------------------------------------------------------------
#Applicant scorecard for rejected population

#computing odds for good. Since the probability computed is for rejection (bad cusotmers),  Odd(good) =  (1-P(bad))/P(bad)
Odds_for_good<-sapply(predictions_reject,function(x) (1-x)/x)

#computing  ln(odd(good))
ln_Odds_for_good<-sapply(Odds_for_good,function(x)log(x))

#Using the following formula for computing application score card
#400 + slope * (ln(odd(good)) - ln(10)) where slope is 20/(ln(20)-ln(10))
slope<-20/(log(20)-log(10))

application_score_card_rejected<-sapply(ln_Odds_for_good,function(x) 400 + slope * (x - log(10)))
head(application_score_card_rejected)

#making dataframe with score card
score_card_rejected<-cbind(Rejected_Data,application_score_card_rejected)
summary(application_score_card_rejected)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#314.7   322.9   324.9   324.7   326.7   331.5
#--------------------------------------------------------------------------

ggplot(score_card_df,aes(application_score_card))+geom_histogram()
#Histogram clearly shows that the application score of applicants are below 338  are few which meets our expectation.
boxplot(application_score_card)

#No.of applicants below score 331.25
length(which(application_score_card<331.25))
#26569

#No.of applicants above score 331.25
length(which(application_score_card>331.25))
#43295

#Calculating Bank Profit

##### Profit with the final model
test_pred_log_final = predict(logistic_final, type = "response", newdata = data_woe)
test_cutoff <- factor(ifelse(test_pred_log_final >=0.52, "1", "0"))
test_actual <- factor(ifelse(data_woe$Perf.Tag=='yes', "1", "0"))
levels(test_cutoff)
levels(data_woe$Perf.Tag)
conf_final <- confusionMatrix(test_cutoff, test_actual, positive = "1")
conf_final

#           Reference
#Prediction     0     1
#         0 42216  1079
#         1 24701  1868

str(score_card_df)
Merged_default <- filter(score_card_df, score_card_df$application_score_card < 331.25, score_card_df$Perf.Tag == 1)
summary_profit <- sum(Merged_default$Outstanding.Balance, na.rm = TRUE)
summary_profit
#Profit is #2338483691 units to the bank
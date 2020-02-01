getwd()
#rm(list=ls())
cr<-read.csv("Credit (1).csv",na.strings=c("",NA))
library(dplyr)

?read.csv
options(scipen=999)#SWITCHES OFF THE SCIENTIFIC NOTATIONS IN TERMS OF NUMBER REPRESENTATUIONS.


##Data Exploration##
# DEALING WITH FIANNCIAL DATASET
# DEMOGRAPHIC VARIABLES LIKE GENDER , RATIO ETC
# This is a credit card dataset
# NPA STATUS PERSON EXPERIENCED 180 DAYS PAST DUE DELINQUENCY OR WORSE
# IF THE PERSON BEING DELINQUENT FOR THE PAST 180 DAYS OR NOT
# GIVES ME AN IDEA THAT WHO ARE MY GOOD CUSTOMER AND WHO ARE BAD CUSTOMERS
# PERSON WHO IS DELINWUENT FRO PAST 180 DAYS WILL BE BAD CUST FOR ME AND VICEVERSA
str(cr)
# 1 means bad customer
# 0 means is a good  customer
#Sanity check
#Identify outliers, replace them
#Approaches to impute missing values
#Bin the data-> Quantile function, ntile() for binning
#Partitioning data: test and training samples

names(cr)
#Duplicate columns present, remove them
cr<-cr[,-c(1,12)]
names(cr)

#Sanity check

summary(cr)

#Missing values
index<-which(is.na(cr$Good_Bad))
index
cr<-cr[-index,]
summary(cr$Good_Bad)
summary(cr)

#Look at individual summaries
#RevolvingUtilizationOfUnsecuredLines is a percentage variable
#If I go back to summary statistics, I see something odd here.
# this is ratio variable or a percentage variable and look at the maximum
#value that it has. Also take a look at the mean value that it has. 

summary(cr$RevolvingUtilizationOfUnsecuredLines) #Ratio variable


#how many observations are zero
cr%>%filter(RevolvingUtilizationOfUnsecuredLines==0)%>%nrow()

# SINCE IT IS A RATIO VARIALE 
#this is ratio variable or percentage variable, ideally its value 
#should not be more than 1 or 2.

# how many observations in my dataset corresponding to this dataset have a 
# magnitude greater than 0.99.
cr%>%filter(RevolvingUtilizationOfUnsecuredLines>=0.99)%>%nrow()
# 14,000 observations have a value greater than 0.99.


#Percentile breakup
#it tells me what percentage of my data has a particular magnitude
10878/150000
14383/150000


quantile(cr$RevolvingUtilizationOfUnsecuredLines,p=c(1:100)/100)
# 99% of my data has a magnitude less than equal to 1.09 and
# there are just 1% observations, which have magnitude greater than this number. 


# WHAT PERCENATGE HAVE A PARTICULAR TYPE OF MAGNITUDE
# DISCUSSION WITH STAKE HOLDER

# will go back to my stakeholder and have a discussion with my holder and tell
# him that this is what I found out when I explored your data. So he might
# give me an input that "Usually this variable never has a magnitude greater than 2". 



#Discuss with client, 2 is the limit on the number, replace
# cap the values of this particular variable. 


#how many rows in my dataset I have which have a magnitude 
# which less than or equal to 2.
cr%>%filter(RevolvingUtilizationOfUnsecuredLines<=2)%>%nrow()


# filter out those observations from my data whose value is more 
# than 2 for this particular variable
cr%>%filter(RevolvingUtilizationOfUnsecuredLines<=2)->cr

# taken out the values in my dataset for this particular variable
# whose magnitude is more than 2.  
summary(cr$RevolvingUtilizationOfUnsecuredLines)


# age
summary(cr$age)

cr%>%filter(age==0)%>%nrow()
# one row conatins 0 value 
quantile(cr$age,p=(1:100)/100)

cr%>%filter(age!=0)->cr # removed the row containing 0
summary(cr$age)

# gender
summary(cr$Gender)


# region
summary(cr$Region)

# monthly income
summary(cr$MonthlyIncome)

cr%>%filter(MonthlyIncome==0)%>%nrow()

quantile(cr$MonthlyIncome,p=c(1:100)/100,na.rm=TRUE)

cr%>%filter(MonthlyIncome>25000)%>%nrow()



quantile(cr$MonthlyIncome,p=c(990:1000)/1000,na.rm=TRUE)

quantile(cr$MonthlyIncome,p=c(9990:10000)/10000,na.rm=TRUE)

str(cr$MonthlyIncome)
summary(cr$MonthlyIncome)
#We find after discussions that '0' here means a missing value
# stake holder says that o inside a monthly revenue columns means a missing values.
cr$MonthlyIncome<-ifelse(cr$MonthlyIncome==0,NA,cr$MonthlyIncome)
summary(cr$MonthlyIncome)

# rented_ownhouse
summary(cr$Rented_OwnHouse)

# occupation
summary(cr$Occupation)


# education
summary(cr$Education)


# number of time 30.59days past due not worse
summary(cr$NumberOfTime30.59DaysPastDueNotWorse)

quantile(cr$NumberOfTime30.59DaysPastDueNotWorse,p=c(1:100)/100)

quantile(cr$NumberOfTime30.59DaysPastDueNotWorse,p=c(990:1000)/1000,na.rm=TRUE)
quantile(cr$NumberOfTime30.59DaysPastDueNotWorse,p=c(9990:10000)/10000,na.rm=TRUE)

# We find on discussions with stakeholders that large numbers are missing values

cr$NumberOfTime30.59DaysPastDueNotWorse<-ifelse(cr$NumberOfTime30.59DaysPastDueNotWorse==98,NA,cr$NumberOfTime30.59DaysPastDueNotWorse)

cr$NumberOfTime30.59DaysPastDueNotWorse<-ifelse(cr$NumberOfTime30.59DaysPastDueNotWorse==96,NA,cr$NumberOfTime30.59DaysPastDueNotWorse)
summary(cr$NumberOfTime30.59DaysPastDueNotWorse)


# debt ratio
summary(cr$DebtRatio)

cr%>%filter(DebtRatio==0)%>%nrow()

quantile(cr$DebtRatio,p=c(1:100)/100)

##Cap at 2## (After discussions with stakeholders)

cr$DebtRatio<-ifelse(cr$DebtRatio>2,2,cr$DebtRatio)
summary(cr$DebtRatio)

#number of open credit lines and loans
summary(cr$NumberOfOpenCreditLinesAndLoans)

quantile(cr$NumberOfOpenCreditLinesAndLoans,p=c(1:100)/100,na.rm=TRUE)
quantile(cr$NumberOfOpenCreditLinesAndLoans,p=c(990:1000)/1000,na.rm=TRUE)

#Higher magnitude numbers represent missing value

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans==58,NA,cr$NumberOfOpenCreditLinesAndLoans)

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans==57,NA,cr$NumberOfOpenCreditLinesAndLoans)

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans==56,NA,cr$NumberOfOpenCreditLinesAndLoans)

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans==54,NA,cr$NumberOfOpenCreditLinesAndLoans)

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans>24,NA,cr$NumberOfOpenCreditLinesAndLoans)



summary(cr$NumberOfOpenCreditLinesAndLoans)



summary(cr$NumberOfTimes90DaysLate)

quantile(cr$NumberOfTimes90DaysLate,p=c(1:100)/100)

quantile(cr$NumberOfTimes90DaysLate,p=c(990:1000)/1000,na.rm=TRUE)

#Higher numbers represent missing value

cr$NumberOfTimes90DaysLate<-ifelse(cr$NumberOfTimes90DaysLate==98,NA,cr$NumberOfTimes90DaysLate)

cr$NumberOfTimes90DaysLate<-ifelse(cr$NumberOfTimes90DaysLate==96,NA,cr$NumberOfTimes90DaysLate)

summary(cr$NumberRealEstateLoansOrLines)

quantile(cr$NumberRealEstateLoansOrLines,p=c(1:100)/100)

quantile(cr$NumberRealEstateLoansOrLines,p=c(990:1000)/1000,na.rm=TRUE)

cr%>%filter(NumberRealEstateLoansOrLines==54)%>%nrow()

cr$NumberRealEstateLoansOrLines<-ifelse(cr$NumberRealEstateLoansOrLines==54,NA,cr$NumberRealEstateLoansOrLines)

cr$NumberRealEstateLoansOrLines<-ifelse(cr$NumberRealEstateLoansOrLines==32,NA,cr$NumberRealEstateLoansOrLines)

cr$NumberRealEstateLoansOrLines<-ifelse(cr$NumberRealEstateLoansOrLines>9,NA,cr$NumberRealEstateLoansOrLines)



summary(cr$NumberOfTime60.89DaysPastDueNotWorse)

quantile(cr$NumberOfTime60.89DaysPastDueNotWorse,p=c(1:100)/100,na.rm=TRUE)

cr$NumberOfTime60.89DaysPastDueNotWorse<-ifelse(cr$NumberOfTime60.89DaysPastDueNotWorse==98,NA,cr$NumberOfTime60.89DaysPastDueNotWorse)

cr$NumberOfTime60.89DaysPastDueNotWorse<-ifelse(cr$NumberOfTime60.89DaysPastDueNotWorse==96,NA,cr$NumberOfTime60.89DaysPastDueNotWorse)




summary(cr$NumberOfDependents)
str(cr$NumberOfDependents)
unique(cr$NumberOfDependents)

cr$NumberOfDependents<-as.character(cr$NumberOfDependents)
str(cr$NumberOfDependents)
####################data exploration is finished#############

##Approach towards missing value treatment and data binning

summary(cr)

## See the behaviour of  dvwrt the idv which has missing values to arrive  at a missing value imputation

#NumberOfTime30.59DaysPastDueNotWorse
table1<-table(cr$NumberOfTime30.59DaysPastDueNotWorse,cr$Good_Bad)
table1
table2 <- table(cr$NumberOfTime60.89DaysPastDueNotWorse,cr$Good_Bad)
table2
bad_rate<-table1[,1]/rowSums(table1)
bad_rate

summary(cr$NumberOfTime30.59DaysPastDueNotWorse)
ind2<-which(is.na(cr$NumberOfTime30.59DaysPastDueNotWorse))
ind2

table(cr$Good_Bad[ind2])/length(ind2)

table((cr$Good_Bad==1)/(nrow(cr$Good_Bad)))
summary(cr$Good_Bad)
9972/149628
## One estimate of NA will be 6
cr$NumberOfTime30.59DaysPastDueNotWorse[ind2]<-6
summary(cr$NumberOfTime30.59DaysPastDueNotWorse)
hist(cr$NumberOfTime30.59DaysPastDueNotWorse)


##Divide into deciles and see the event rate
library(dplyr)

cr%>%mutate(quantile=ntile(MonthlyIncome,10))%>%group_by(Good_Bad,quantile)%>%summarize(N=n())%>%filter(Good_Bad=="Bad")->dat

cr%>%mutate(quantile=ntile(MonthlyIncome,10))%>%group_by(quantile)%>%summarize(N=n())->dat1

dat$Percentage<-dat$N/dat1$N




#Replace with values in 8th quantile
#What is 8th quantile?
quantile(cr$MonthlyIncome,p=(0:10)/10,na.rm=T)
cr$MonthlyIncome[is.na(cr$MonthlyIncome)]<-9200
summary(cr$MonthlyIncome)

##Data Prepration requires binnig data and then collapsing the number of bins
#Age variable

cr%>%mutate(Quantile=ntile(age,10))%>%group_by(Good_Bad,Quantile)%>%summarize(N=n())%>%filter(Good_Bad=="Bad")->dat2

cr%>%mutate(Quantile=ntile(age,10))%>%group_by(Quantile)%>%summarize(N=n())->dat3

dat2$Percent<-dat2$N/dat3$N

quantile(cr$age,p=(0:10)/100,na.rm=T)

##Partitioning data##
set.seed(100)

indexP<-sample(1:nrow(cr),0.70*nrow(cr),replace = F)
train_cr<-cr[indexP,]
test_cr<-cr[-indexP,]

##Be cautious of balanced and unbalanced samples
#Use caret() 
install.packages("caret")
library(caret)
indexPC<-createDataPartition(y=cr$Good_Bad,times = 1,p=0.70,list=F)
train_crC<-cr[indexPC,]
test_crC<-cr[-indexPC,]

table(train_crC$Good_Bad)/nrow(train_crC)
table(test_crC$Good_Bad)/nrow(test_crC)




#Libraries ----
  ##For Data Manipulation and Inspection ----
library(dplyr)
library(DescTools)
library(psych)

  ##For Correlation and Covariance ----
library(PerformanceAnalytics)
library(corrplot)

  ##For PCA ----
library(factoextra)

#Data Overview ----
  ##Working Directory ----
  ##Where is my working directory? ----
getwd()

  ##Setting the working directory ----
setwd("C:/Users/Yannis/Desktop/RStudio")

  ##Reading the file in the working directory ----
df<-read.csv("C:/Users/Yannis/Desktop/RStudio/Attrition/Attrition_project.csv",header = T,sep = ",")
str(df)
  ##Dataset Inspection ----
class(df)

#Viewing the dataset
View(df)

#Taking a peak into the top cases of the dataset
head(df)

#Taking a peak into the bottom cases of the dataset
tail(df)

  ##Dataset Dimensions ----
dim(df)

#Number of rows (Number of observations)
nrow(df) #or
length(df[,1])

#Number of columns (Number of variables)
ncol(df) #or
length(df[1,])

#Names of the variables
colnames(df)

#Names of the observations (if they have, in this case they don't)
rownames(df)

  ##Dataset Structure ----
#Structure of the dataset
str(df)

#Summary statistics of the variables of the dataset
summary(df)
glimpse(df)
describe(df)
Desc(df)

#Data Manipulation ----
  ##Age ----
#Variable counter
k=0
k<-k+1

#Name has weird characters so i change it
names(df)[1]<-c("Age")

#Inspect the data
df$Age

#This variable should be numerical, is it?
is.numeric(df$Age)

  ##Attrition ----
#Variable counter
k<-k+1

#Inspect the data
df$Attrition

#This variable should be factor, is it?
is.factor(df$Attrition)

#This variable should be unordered, is it?
is.ordered(df$Attrition)

#Making the variable unordered factor
df$Attrition<-factor(df$Attrition,ordered=FALSE,levels =c("No","Yes"),labels = c(0,1))
str(df$Attrition)
df$Attrition

#Checking if the variable is factor
is.factor(df$Attrition)

#Checking if the variable is unordered
is.ordered(df$Attrition)

#Inspect if the data have the correct raw data structure
df$Attrition

#Checking if every single observation is labeled
sum(table(df$Attrition))==nrow(df)

  ##BusinessTravel ----
#Variable counter
k<-k+1

#Inspect the data
df$BusinessTravel

#This variable should be factor, is it?
is.factor(df$BusinessTravel)

#This variable should be ordered, is it?
is.ordered(df$BusinessTravel)

#Making the variable ordered factor
df$BusinessTravel<-factor(df$BusinessTravel,ordered = TRUE,levels = c("Non-Travel","Travel_Rarely","Travel_Frequently"),labels=c(1,2,3))
str(df$BusinessTravel)
df$BusinessTravel

#Checking if the variable is factor
is.factor(df$BusinessTravel)

#Checking if the variable is ordered
is.ordered(df$BusinessTravel)

#Inspect if the data have the correct raw data structure
df$BusinessTravel

#Checking if every single observation is labeled
sum(table(df$BusinessTravel))==nrow(df)

  ##DailyRate ----
#Variable counter
k<-k+1

#Inspect the data
df$DailyRate

#This variable should be numerical, is it?
is.numeric(df$DailyRate)

  ##Department ----
#Variable counter
k<-k+1

#Inspect the data
df$Department

#This variable should be factor, is it?
is.factor(df$Department)

#This variable should be unordered, is it?
is.ordered(df$Department)
df$Department

#Making the variable unordered factor
df$Department<-factor(df$Department,ordered=FALSE,levels = c("Sales","Research & Development","Human Resources"),labels=c(1,2,3))
str(df$Department)
df$Department

#Checking if the variable is factor
is.factor(df$Department)

#Checking if the variable is unordered
is.ordered(df$Department)

#Inspect if the data have the correct raw data structure
df$Department

#Checking if every single observation is labeled
sum(table(df$Department))==nrow(df)

  ##DistanceFromHome ----
#Variable counter
k<-k+1

#Inspect the data
df$DistanceFromHome

#This variable should be numerical, is it?
is.numeric(df$DistanceFromHome)

  ##Education ----
#Variable counter
k<-k+1

#Inspect the data
df$Education

#This variable should be factor, is it?
is.factor(df$Education)

#This variable should be ordered, is it?
is.ordered(df$Education)

#Making the variable unordered factor
df$Education<-factor(df$Education,ordered = TRUE,levels=c(1,2,3,4,5))
df$Education
str(df)

#Checking if the variable is factor
is.factor(df$Education)

#Checking if the variable is ordered
is.ordered(df$Education)

#Inspect if the data have the correct raw data structure
df$Education

#Checking if every single observation is labeled
sum(table(df$Education))==nrow(df)

  ##EducationField ----
#Variable counter
k<-k+1

#Inspect the data
df$EducationField

#This variable should be factor, is it?
is.factor(df$EducationField)

#This variable should be unordered, is it?
is.ordered(df$EducationField)

#Making the variable unordered factor
df$EducationField<-factor(df$EducationField,ordered = FALSE,levels = c("Human Resources","Life Sciences","Marketing","Medical","Technical Degree","Other"),labels=c(1,2,3,4,5,6))
str(df$EducationField)
df$EducationField

#Checking if the variable is factor
is.factor(df$EducationField)

#Checking if the variable is unordered
is.ordered(df$EducationField)

#Inspect if the data have the correct raw data structure
df$EducationField

#Checking if every single observation is labeled
sum(table(df$EducationField))==nrow(df)

  ##EnvironmentSatisfaction ----
#Variable counter
k<-k+1

#Inspect the data
df$EnvironmentSatisfaction

#This variable should be factor, is it?
is.factor(df$EnvironmentSatisfaction)

#This variable should be ordered, is it?
is.ordered(df$EnvironmentSatisfaction)

#Making the variable ordered factor
df$EnvironmentSatisfaction<-factor(df$EnvironmentSatisfaction,ordered=TRUE,levels=c(1,2,3,4))
str(df$EnvironmentSatisfaction)

#Checking if the variable is factor
is.factor(df$EnvironmentSatisfaction)

#Checking if the variable is ordered
is.ordered(df$EnvironmentSatisfaction)

#Inspect if the data have the correct raw data structure
df$EnvironmentSatisfaction

#Checking if every single observation is labeled
sum(table(df$EnvironmentSatisfaction))==nrow(df)

  ##EmployeeCount ----
#Variable counter
k<-k+1

#Inspect the data
df$EmployeeCount

#This variable should be numerical, is it?
is.numeric(df$EmployeeCount)

#How many data values have EmployeeCount equal to 1
count(df,EmployeeCount,EmployeeCount==1)

#EmployeeCount is always 1 its the number of employees answering the questions
#This variable does not provide any information about the data

  ##EmployeeNumber ----
#Variable counter
k<-k+1

#Inspect the data
df$EmployeeNumber

#This variable should be numerical, is it?
is.numeric(df$EmployeeNumber)

#EmployeeNumber is the ID number of the employee, thus it is not useful

  ##Gender ----
#Variable counter
k<-k+1

#Inspect the data
df$Gender

#This variable should be factor, is it?
is.factor(df$Gender)

#This variable should be unordered, is it?
is.ordered(df$Gender)

#Making the variable unordered factor
df$Gender<-factor(df$Gender,ordered = FALSE,levels = c("Female","Male"),labels = c(0,1))
str(df$Gender)

#Checking if the variable is factor
is.factor(df$Gender)

#Checking if the variable is unordered
is.ordered(df$Gender)

#Inspect if the data have the correct raw data structure
df$Gender

#Checking if every single observation is labeled
sum(table(df$Gender))==nrow(df)

  ##HourlyRate ----
#Variable counter
k<-k+1

#Inspect the data
df$HourlyRate

#This variable should be numerical, is it?
is.numeric(df$HourlyRate)

  ##JobInvolvement ----
#Variable counter
k<-k+1

#Inspect the data
df$JobInvolvement

#This variable should be factor, is it?
is.factor(df$JobInvolvement)

#This variable should be ordered, is it?
is.ordered(df$JobInvolvement)

#Making the variable ordered factor
df$JobInvolvement<-factor(df$JobInvolvement,ordered=TRUE,levels=c(1,2,3,4))
str(df$JobInvolvement)

#Checking if the variable is factor
is.factor(df$JobInvolvement)

#Checking if the variable is ordered
is.ordered(df$JobInvolvement)

#Inspect if the data have the correct raw data structure
df$JobInvolvemente

#Checking if every single observation is labeled
sum(table(df$JobInvolvement))==nrow(df)

  ##JobLevel ----
#Variable counter
k<-k+1

#Inspect the data
df$JobLevel

#This variable should be factor, is it?
is.factor(df$JobLevel)

#This variable should be ordered, is it?
is.ordered(df$JobLevel)

#Making the variable ordered factor
df$JobLevel<-factor(df$JobLevel,ordered=TRUE,levels=c(1,2,3,4,5))
str(df$JobLevel)

#Checking if the variable is factor
is.factor(df$JobLevel)

#Checking if the variable is ordered
is.ordered(df$JobLevel)

#Inspect if the data have the correct raw data structure
df$JobLevel

#Checking if every single observation is labeled
sum(table(df$JobLevel))==nrow(df)

  ##JobRole ----
#Variable counter
k<-k+1

#Inspect the data
View(df)

#This variable should be factor, is it?
is.factor(df$JobRole)

#This variable should be unordered, is it?
is.ordered(df$JobRole)

#Making the variable unordered factor
df$JobRole<-factor(df$JobRole,ordered = FALSE,levels=c("Healthcare Representative"
                                                       ,"Human Resources"
                                                       ,"Laboratory Technician"
                                                       ,"Manager"
                                                       ,"Manufacturing Director"
                                                       ,"Research Director"
                                                       ,"Research Scientist"
                                                       ,"Sales Executive"
                                                       ,"Sales Representative"),labels=c(1,2,3,4,5,6,7,8,9))
str(df$JobRole)
df$JobRole

#Checking if the variable is factor
is.factor(df$JobRole)

#Checking if the variable is unordered
is.ordered(df$JobRole)

#Inspect if the data have the correct raw data structure
df$JobRole

#Checking if every single observation is labeled
sum(table(df$JobRole))==nrow(df)

  ##JobSatisfaction ----
#Variable counter
k<-k+1

#Inspect the data
df$JobSatisfaction

#This variable should be factor, is it?
is.factor(df$JobSatisfaction)

#This variable should be ordered, is it?
is.ordered(df$JobSatisfaction)

#Making the variable ordered factor
df$JobSatisfaction<-factor(df$JobSatisfaction,ordered=TRUE,levels=c(1,2,3,4))
str(df$JobSatisfaction)

#Checking if the variable is factor
is.factor(df$JobSatisfaction)

#Checking if the variable is ordered
is.ordered(df$JobSatisfaction)

#Inspect if the data have the correct raw data structure
df$JobSatisfaction

#Checking if every single observation is labeled
sum(table(df$JobSatisfaction))==nrow(df)

  ##MaritalStatus ----
#Variable counter
k<-k+1

#Inspect the data
df$MaritalStatus

#This variable should be factor, is it?
is.factor(df$MaritalStatus)

#This variable should be unordered, is it?
is.ordered(df$MaritalStatus)
df$MaritalStatus

#Making the variable unordered factor
df$MaritalStatus<-factor(df$MaritalStatus,ordered = FALSE,levels = c("Single","Married","Divorced"),labels = c(1,2,3))
str(df$MaritalStatus)

#Checking if the variable is factor
is.factor(df$MaritalStatus)

#Checking if the variable is unordered
is.ordered(df$MaritalStatus)

#Inspect if the data have the correct raw data structure
df$MaritalStatus

#Checking if every single observation is labeled
sum(table(df$MaritalStatus))==nrow(df)
df$MaritalStatus

  ##MontlyIncome ----
#Variable counter
k<-k+1

#Inspect the data
df$MonthlyIncome

#This variable should be numerical, is it?
is.numeric(df$MonthlyIncome)

  ##MonthlyRate ----
#Variable counter
k<-k+1

#Inspect the data
df$MonthlyRate

#This variable should be numerical, is it?
is.numeric(df$MonthlyRate)

  ##NumCompaniesWorked ----
#Variable counter
k<-k+1

#Inspect the data
df$NumCompaniesWorked

#This variable should be numerical, is it?
is.numeric(df$NumCompaniesWorked)

  ##Over18 ----
#Variable counter
k<-k+1

#Inspect the data
df$Over18

#This variable should be factor, is it?
is.factor(df$Over18)

#This variable should be unordered, is it?
is.ordered(df$Over18)

#Making the variable unordered factor
df$Over18<-factor(df$Over18,ordered=FALSE,levels = c("N","Y"),labels = c(0,1))
str(df$Over18)

#Checking if the variable is factor
is.factor(df$Over18)

#Checking if the variable is unordered
is.ordered(df$Over18)

#Inspect if the data have the correct raw data structure
df$Over18

#Checking if every single observation is labeled
sum(table(df$Over18))==nrow(df)
df$
  #Everyone is over 18 years old, this variable doesn't provide any information about the data

  ##OverTime ----
#Variable counter
k<-k+1

#Inspect the data
df$OverTime

#This variable should be factor, is it?
is.factor(df$OverTime)

#This variable should be unordered, is it?
is.ordered(df$Overtime)

#Making the variable unordered factor
df$OverTime<-factor(df$OverTime,levels=c('No','Yes'),labels = c(0,1))
str(df$OverTime)

#Checking if the variable is factor
is.factor(df$OverTime)

#Checking if the variable is unordered
is.ordered(df$Overtime)

#Inspect if the data have the correct raw data structure
df$OverTime

#Checking if every single observation is labeled
sum(table(df$Overtime))==nrow(df)

  ##PercentSalaryHike ----
#Variable counter
k<-k+1

#Inspect the data
df$PercentSalaryHike

#This variable should be numerical, is it?
is.numeric(df$PercentSalaryHike)

  ##PerformanceRating ----
#Variable counter
k<-k+1

#Inspect the data
df$PerformanceRating

#This variable should be factor, is it?
is.factor(df$PerformanceRating)

#This variable should be ordered, is it?
is.ordered(df$PerformanceRating)

#Making the variable ordered factor
df$PerformanceRating<-factor(df$PerformanceRating,ordered=TRUE,levels = c(1,2,3,4))
str(df$PerformanceRating)

#Checking if the variable is factor
is.factor(df$PerformanceRating)

#Checking if the variable is ordered
is.ordered(df$PerformanceRating)

#Inspect if the data have the correct raw data structure
df$PerformanceRating

#Checking if every single observation is labeled
sum(table(df$PerformanceRating))==nrow(df)

  ##RelationshipSatisfaction ----
#Variable counter
k<-k+1

#Inspect the data
df$RelationshipSatisfaction

#This variable should be factor, is it?
is.factor(df$RelationshipSatisfaction)

#This variable should be ordered, is it?
is.ordered(df$RelationshipSatisfaction)

#Making the variable ordered factor
df$RelationshipSatisfaction<-factor(df$RelationshipSatisfaction,ordered=TRUE,levels = c(1,2,3,4))
str(df$RelationshipSatisfaction)

#Checking if the variable is factor
is.factor(df$RelationshipSatisfaction)

#Checking if the variable is ordered
is.ordered(df$RelationshipSatisfaction)

#Inspect if the data have the correct raw data structure
df$RelationshipSatisfaction

#Checking if every single observation is labeled
sum(table(df$RelationshipSatisfaction))==nrow(df)

  ##StandardHours ----
#Variable counter
k<-k+1

#Inspect the data
df$StandardHours

#This variable should be numerical, is it?
is.numeric(df$StandardHours)

#Every value is equal to 80, this variable does not provide any information about the data
count(df,StandardHours,StandardHours==80)

  ##StockOptionLevel ----
#Variable counter
k<-k+1

#Inspect the data
df$StockOptionLevel

#This variable should be factor, is it?
is.factor(df$StockOptionLevel)

#Making the variable factor
df$StockOptionLevel<-factor(df$StockOptionLevel)
str(df$StockOptionLevel)

#Checking if the variable is factor
is.factor(df$StockOptionLevel)

#Inspect if the data have the correct raw data structure
df$StockOptionLevel

#Checking if every single observation is labeled
sum(table(df$PerformanceRating))==nrow(df)

  ##TotalWorkingYears ----
#Variable counter
k<-k+1

#Inspect the data
df$TotalWorkingYears

#This variable should be numerical, is it?
is.numeric(df$TotalWorkingYears)

  ##TrainingTimesLastYear ----
#Variable counter
k<-k+1

#Inspect the data
df$TrainingTimesLastYear

#This variable should be numerical, is it?
is.numeric(df$TrainingTimesLastYear)

  ##WorkLifeBalance ----
#Variable counter
k<-k+1

#Inspect the data
df$WorkLifeBalance

#This variable should be factor, is it?
is.factor(df$WorkLifeBalance)

#This variable should be ordered, is it?
is.ordered(df$WorkLifeBalance)

#Making the variable ordered factor
df$WorkLifeBalance<-factor(df$WorkLifeBalance,ordered=TRUE,levels = c(1,2,3,4))
str(df$WorkLifeBalance)

#Checking if the variable is factor
is.factor(df$WorkLifeBalance)

#Checking if the variable is ordered
is.ordered(df$WorkLifeBalance)

#Inspect if the data have the correct raw data structure
df$WorkLifeBalance

#Checking if every single observation is labeled
sum(table(df$WorkLifeBalance))==nrow(df)

  ##YearsAtCompany ----
#Variable counter
k<-k+1

#Inspect the data
df$YearsAtCompany

#This variable should be numerical, is it?
is.numeric(df$YearsAtCompany)

  ##YearsInCurrentRole ----
#Variable counter
k<-k+1

#Inspect the data
df$YearsInCurrentRole

#This variable should be numerical, is it?
is.numeric(df$YearsInCurrentRole)

  ##YearsSinceLastPromotion ----
#Variable counter
k<-k+1

#Inspect the data
df$YearsSinceLastPromotion

#This variable should be numerical, is it?
is.numeric(df$YearsSinceLastPromotion)

  ##YearsWithCurrManager ----
#Variable counter
k<-k+1

#Inspect the data
df$YearsWithCurrManager

#This variable should be numerical, is it?
is.numeric(df$YearsWithCurrManager)

#Post-Manipulation Analysis ----
str(df)
summary(df)
glimpse(df)

#k represents the total number of variables being examined, is this equal to the true number of variables in the dataset?
k==ncol(df)

#There are 35 variables in the dataset, removing variables EmployeeCount, EmployeeNumber, StandardHours and Over18 because they do not provide any information about the data
#31 variables left
names(df)
df<-df[,-c(9,10,22,27)]
dim(df)
names(df)

  ##Numerical Dataset ----

#Numerical dataset
numdf<-df %>% dplyr::select(Age,
                            DailyRate,
                            DistanceFromHome,
                            HourlyRate,
                            MonthlyIncome,
                            MonthlyRate,
                            NumCompaniesWorked,
                            PercentSalaryHike,
                            TotalWorkingYears,
                            TrainingTimesLastYear,
                            YearsAtCompany,
                            YearsInCurrentRole,
                            YearsSinceLastPromotion,
                            YearsWithCurrManager)
str(numdf)

#Number of variables in numdf
n_numdf<-ncol(numdf);n_numdf

  ##Categorical Dataset ----
categdf<-df %>% dplyr::select(-c(Age,
                                 DailyRate,
                                 DistanceFromHome,
                                 HourlyRate,
                                 MonthlyIncome,
                                 MonthlyRate,
                                 NumCompaniesWorked,
                                 PercentSalaryHike,
                                 TotalWorkingYears,
                                 TrainingTimesLastYear,
                                 YearsAtCompany,
                                 YearsInCurrentRole,
                                 YearsSinceLastPromotion,
                                 YearsWithCurrManager))
str(categdf)

#Number of variables in categdf
n_categdf<-ncol(categdf);n_categdf

#Is the total number of variables equal to 32?
n_numdf+n_categdf==ncol(df)


#Principal Component Analysis ----
  ##Defining the numerical dataset ----
numdf
str(numdf)
names(numdf)
pcanumdf<-numdf

  ##Correlation Structure ----
cor(pcanumdf)
corrplot(cor(pcanumdf), type = "upper", method= "number")

  ##PCA with Matrices ----

#Checking the amount and type of variables
names(pcanumdf)

    ###Standardizing with mean-adjusted values ----
X1<-pcanumdf[,1] - mean(pcanumdf[,1])
X2<-pcanumdf[,2] - mean(pcanumdf[,2])
X3<-pcanumdf[,3] - mean(pcanumdf[,3])
X4<-pcanumdf[,4] - mean(pcanumdf[,4])
X5<-pcanumdf[,5] - mean(pcanumdf[,5])
X6<-pcanumdf[,6] - mean(pcanumdf[,6])
X7<-pcanumdf[,7] - mean(pcanumdf[,7])
X8<-pcanumdf[,8] - mean(pcanumdf[,8])
X9<-pcanumdf[,9] - mean(pcanumdf[,9])
X10<-pcanumdf[,10] - mean(pcanumdf[,10])
X11<-pcanumdf[,11] - mean(pcanumdf[,11])
X12<-pcanumdf[,12] - mean(pcanumdf[,12])
X13<-pcanumdf[,13] - mean(pcanumdf[,13])
X14<-pcanumdf[,14] - mean(pcanumdf[,14])

    ###Creating the Covariance Matrix ----
matrices.covm <- matrix(c(cov(X1,X1), cov(X1,X2), cov(X1,X3),cov(X1,X4),cov(X1,X5),cov(X1,X6),cov(X1,X7),cov(X1,X8),cov(X1,X9),cov(X1,X10),cov(X1,X11),cov(X1,X12),cov(X1,X13),cov(X1,X14)
                          ,cov(X2,X1), cov(X2,X2), cov(X2,X3),cov(X2,X4),cov(X2,X5),cov(X2,X6),cov(X2,X7),cov(X2,X8),cov(X2,X9),cov(X2,X10),cov(X2,X11),cov(X2,X12),cov(X2,X13),cov(X2,X14)
                          ,cov(X3,X1), cov(X3,X2), cov(X3,X3),cov(X3,X4),cov(X3,X5),cov(X3,X6),cov(X3,X7),cov(X3,X8),cov(X3,X9),cov(X3,X10),cov(X3,X11),cov(X3,X12),cov(X3,X13),cov(X3,X14)
                          ,cov(X4,X1), cov(X4,X2), cov(X4,X3),cov(X4,X4),cov(X4,X5),cov(X4,X6),cov(X4,X7),cov(X4,X8),cov(X4,X9),cov(X4,X10),cov(X4,X11),cov(X4,X12),cov(X4,X13),cov(X4,X14)
                          ,cov(X5,X1), cov(X5,X2), cov(X5,X3),cov(X5,X4),cov(X5,X5),cov(X5,X6),cov(X5,X7),cov(X5,X8),cov(X5,X9),cov(X5,X10),cov(X5,X11),cov(X5,X12),cov(X5,X13),cov(X5,X14)
                          ,cov(X6,X1), cov(X6,X2), cov(X6,X3),cov(X6,X4),cov(X6,X5),cov(X6,X6),cov(X6,X7),cov(X6,X8),cov(X6,X9),cov(X6,X10),cov(X6,X11),cov(X6,X12),cov(X6,X13),cov(X6,X14)
                          ,cov(X7,X1), cov(X7,X2), cov(X7,X3),cov(X7,X4),cov(X7,X5),cov(X7,X6),cov(X7,X7),cov(X7,X8),cov(X7,X9),cov(X7,X10),cov(X7,X11),cov(X7,X12),cov(X7,X13),cov(X7,X14)
                          ,cov(X8,X1), cov(X8,X2), cov(X8,X3),cov(X8,X4),cov(X8,X5),cov(X8,X6),cov(X8,X7),cov(X8,X8),cov(X8,X9),cov(X8,X10),cov(X8,X11),cov(X8,X12),cov(X8,X13),cov(X8,X14)
                          ,cov(X9,X1), cov(X9,X2), cov(X9,X3),cov(X9,X4),cov(X9,X5),cov(X9,X6),cov(X9,X7),cov(X9,X8),cov(X9,X9),cov(X9,X10),cov(X9,X11),cov(X9,X12),cov(X9,X13),cov(X9,X14)
                          ,cov(X10,X1), cov(X10,X2), cov(X10,X3),cov(X10,X4),cov(X10,X5),cov(X10,X6),cov(X10,X7),cov(X10,X8),cov(X10,X9),cov(X10,X10),cov(X10,X11),cov(X10,X12),cov(X10,X13),cov(X10,X14)
                          ,cov(X11,X1), cov(X11,X2), cov(X11,X3),cov(X11,X4),cov(X11,X5),cov(X11,X6),cov(X11,X7),cov(X11,X8),cov(X11,X9),cov(X11,X10),cov(X11,X11),cov(X11,X12),cov(X11,X13),cov(X11,X14)
                          ,cov(X12,X1), cov(X12,X2), cov(X12,X3),cov(X12,X4),cov(X12,X5),cov(X12,X6),cov(X12,X7),cov(X12,X8),cov(X12,X9),cov(X12,X10),cov(X12,X11),cov(X12,X12),cov(X12,X13),cov(X12,X14)
                          ,cov(X13,X1), cov(X13,X2), cov(X13,X3),cov(X13,X4),cov(X13,X5),cov(X13,X6),cov(X13,X7),cov(X13,X8),cov(X13,X9),cov(X13,X10),cov(X13,X11),cov(X13,X12),cov(X13,X13),cov(X13,X14)
                          ,cov(X14,X1), cov(X14,X2), cov(X14,X3),cov(X14,X4),cov(X14,X5),cov(X14,X6),cov(X14,X7),cov(X14,X8),cov(X14,X9),cov(X14,X10),cov(X14,X11),cov(X14,X12),cov(X14,X13),cov(X14,X14)
),
nrow=14,
ncol=14,
byrow=TRUE,
dimnames=list(c("Age",
                "DailyRate",
                "DistanceFromHome",
                "HourlyRate",
                "MonthlyIncome",
                "MonthlyRate",
                "NumCompaniesWorked",
                "PercentSalaryHike",
                "TotalWorkingYears",
                "TrainingTimesLastYear",
                "YearsAtCompany",
                "YearsInCurrentRole",
                "YearsSinceLastPromotion",
                "YearsWithCurrManager"),
              c("Age",
                "DailyRate",
                "DistanceFromHome",
                "HourlyRate",
                "MonthlyIncome",
                "MonthlyRate",
                "NumCompaniesWorked",
                "PercentSalaryHike",
                "TotalWorkingYears",
                "TrainingTimesLastYear",
                "YearsAtCompany",
                "YearsInCurrentRole",
                "YearsSinceLastPromotion",
                "YearsWithCurrManager")));matrices.covm

round(matrices.covm,digits=2)

    ###Getting the Eigenstuff ----
matrices.eigen<-eigen(matrices.covm);matrices.eigen

    ###Getting the Eigenvectors (prcomp$rotation or princomp$loadings) ----
#These are the vectors with which the standardized values are multiplied and then added to create the PC's
matrices.loadings<-matrices.eigen$vectors;matrices.loadings
round(matrices.loadings,digits=4)

    ###Getting the Eigenvalues ---- 
    ###Variance of each of the Pc's ----
matrices.var<-matrices.eigen$values;matrices.var

    ###Variance Contribution of each of the Pc's (summary(pca)) ----
matrices.var.per<-round(matrices.var/sum(matrices.var)*100, digits = 2);matrices.var.per

    ###Standard Deviation of each of the Pc's (summary(pca)) ----
matrices.sdev<-sqrt(matrices.eigen$values);matrices.sdev

    ###Coordinates of the variables ----
matrices.coord<-matrices.loadings*matrices.sdev;matrices.coord

    ###Quality of Representation ----
#The variables' Components Squared Cosine
matrices.cos2<-matrices.coord^2;matrices.cos2
corrplot(matrices.cos2, is.corr=FALSE)
    ###Getting PC1 and PC2 (prcomp$x or princomp$scores) ----
pc1 <- X1 * matrices.loadings[1,1] + X2 * matrices.loadings[2,1] + X3 * matrices.loadings[3,1] + X4 * matrices.loadings[4,1] + X5 * matrices.loadings[5,1] + X6 * matrices.loadings[6,1] + X7 * matrices.loadings[7,1] + X8 * matrices.loadings[8,1] + X9 * matrices.loadings[9,1] + X10 * matrices.loadings[10,1] + X11 * matrices.loadings[11,1] + X12 * matrices.loadings[12,1] + X13 * matrices.loadings[13,1] + X14 * matrices.loadings[14,1]
pc1

pc2 <- X1 * matrices.loadings[1,2] + X2 * matrices.loadings[2,2] + X3 * matrices.loadings[3,2] + X4 * matrices.loadings[4,2] + X5 * matrices.loadings[5,2] + X6 * matrices.loadings[6,2] + X7 * matrices.loadings[7,2] + X8 * matrices.loadings[8,2] + X9 * matrices.loadings[9,2] + X10 * matrices.loadings[10,2] + X11 * matrices.loadings[11,2] + X12 * matrices.loadings[12,2] + X13 * matrices.loadings[13,2] + X14 * matrices.loadings[14,2]
pc2

    ###PC1 and PC2 in a data frame ----
matrices.PC1andPC2<-data.frame(PC1 = pc1, PC2 = pc2);matrices.PC1andPC2

  ##Principal Component Analysis using princomp ----
princomp.pca <- princomp(pcanumdf, scores=TRUE, cor=T)
summary(princomp.pca)
?princomp

    ###The Attributes of princomp PCA ----
attributes(princomp.pca)
princomp.pca$sdev

    ###The Standard Deviation of each of the Pc's ----
princomp.sdev <- princomp.pca$sdev;princomp.sdev

    ###Variance of each of the Pc's ----
princomp.var <- princomp.sdev^2;princomp.var

    ###Proportion of Variance of each of the Pc's ----
princomp.var.per <- round(princomp.var/sum(princomp.var)*100, digits = 2);princomp.var.per

    ###PCA Loadings for PC1 and PC2 ----
princomp.loadings <- princomp.pca$loadings[,1:2];princomp.loadings
round(princomp.loadings,digits=4)

    ###PCA Scores for PC1 and PC2 ----
princomp.scores <- princomp.pca$scores[,1:2];princomp.scores

    ###Graphical Representation of PCA ----
princomp.screeplot<-fviz_eig(princomp.pca);princomp.screeplot
fviz_pca_ind(princomp.pca)
fviz_pca_var(princomp.pca,xlab = "PC1",ylab = "PC2")
fviz_pca_biplot(princomp.pca,xlab = "PC1",ylab = "PC2")

    ###Results for Variables ----
princomp.res.var<- get_pca_var(princomp.pca);princomp.res.var

    ###Coordinates of the Variables ----
princomp.coord<-princomp.res.var$coord;princomp.coord

    ###Contribution of Variables to the PCs ----
princomp.contrib<-princomp.res.var$contrib;princomp.contrib
round(princomp.contrib,digits=2)
corrplot(princomp.contrib,is.corr=FALSE,method="number") 

    ###Quality of Representation ----
princomp.cos<-princomp.res.var$cos2;princomp.cos
round(princomp.cos,digits=2)
corrplot(princomp.cos,is.corr=FALSE,method="circle")

    ###Extracting PC1 and PC2 ----
princomp.PC1andPC2 <- data.frame(princomp.pca$scores[ ,c(1,2)]);princomp.PC1andPC2
princomp.PC1andPC2

    ###Column binding PC1 and PC2 with pcanumdf ----
princomp.pcanumdf<-cbind(pcanumdf,princomp.PC1andPC2);princomp.pcanumdf

  ##Principal Component Analysis using prcomp ----
prcomp.pca <- prcomp(pcanumdf,scale=T)
summary(prcomp.pca)

    ###The Attributes of prcomp PCA ----
attributes(prcomp.pca)

    ###Variance of each of the Pc's ----
prcomp.var <- prcomp.pca$sdev^2;prcomp.var

    ###Proportion of Variance of each of the Pc's ----
prcomp.var.per <- round(prcomp.var/sum(prcomp.var)*100, digits = 2);prcomp.var.per

    ###PCA Rotation ----
prcomp.pca$rotation[,1:2]

    ###PCA Scores ----
prcomp.pca$x

    ###Graphical Representation ----
prcomp.screeplot<-fviz_eig(prcomp.pca);prcomp.screeplot
fviz_pca_ind(prcomp.pca)
fviz_pca_var(prcomp.pca,xlab = "PC1",ylab = "PC2")
fviz_pca_biplot(prcomp.pca,xlab = "PC1",ylab = "PC2")

    ###Results for Variables----
prcomp.res.var <- get_pca_var(prcomp.pca);prcomp.res.var

    ###Coordinates ----
prcomp.coord<-prcomp.res.var$coord;prcomp.coord

    ###Contributions to the PCs ----
prcomp.contrib<-prcomp.res.var$contrib;prcomp.contrib
round(prcomp.contrib[,1:2],digits=3)

    ###Quality of Representation ----
prcomp.cos2<-prcomp.res.var$cos2;prcomp.cos2
round(prcomp.cos2[,1:2],digits=3)

    ###Extracting the 1st and 2nd PC ----
prcomp.PC1andPC2 <- data.frame(prcomp.pca$x[ ,c(1,2)]);prcomp.PC1andPC2

    ###Column bind with pcanumdf ----
prcomp.pcanumdf<-cbind(pcanumdf,prcomp.PC1andPC2);prcomp.pcanumdf

#Clustering - Silhouette Method ----
#CLUSTERING IS DONE BY SETTING cor=F IN princomp, thus using the covariance matrix

  ##princomp PCA with cor=F ----
princomp.pca.corF <- princomp(pcanumdf, scores=TRUE, cor=F)

  ##Extracting PC1 and PC2 ----
princomp.PC1andPC2.corF <- data.frame(princomp.pca.corF$scores[ ,c(1,2)]);princomp.PC1andPC2.corF

  ##Defining the Number of Clusters ----
pricomp.scaled<-scale(princomp.PC1andPC2.corF)
fviz_nbclust(pricomp.scaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

  ##Clustering ----
set.seed(123)
km_res <- kmeans(princomp.PC1andPC2.corF, centers = 3, nstart = 20); km_res
fviz_cluster(km_res, princomp.PC1andPC2.corF, ellipse.type = "norm", main="Clusters")
km_res$cluster

#Centers are important because they represent the whole cluster character.
km_res$centers

#Clustered df
clusteredf<-cbind(pcanumdf,km_res$cluster)
colnames(clusteredf)[15]<-c("Cluster_group")
names(clusteredf)

  ##Finding the observations in each cluster  ----
cluster1<-clusteredf %>% filter(Cluster_group==1);cluster1
summary(cluster1)
corrplot(cor(cluster1[,-15]), type = "upper", method= "number")
nrow(cluster1)

cluster2<-clusteredf %>% filter(Cluster_group==2);cluster2
summary(cluster2)
corrplot(cor(cluster2[,-15]), type = "upper", method= "number")
nrow(cluster2)

cluster3<-clusteredf %>% filter(Cluster_group==3);cluster3
summary(cluster3)
corrplot(cor(cluster3[,-15]), type = "upper", method= "number")
nrow(cluster3)


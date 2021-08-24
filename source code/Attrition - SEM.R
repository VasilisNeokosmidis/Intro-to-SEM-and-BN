#Libraries ----
  ##For Data Manipulation and Inspection ----
library(dplyr)
library(DescTools)
library(psych)

  ##For Correlation and Covariance ----
library(PerformanceAnalytics)
library(corrplot)

  ##For SEM ----
library(lavaan)
library(semPlot)
library(semTools)

#Data Overview ----
  ##Working Directory ----
  ##Where is my working directory? ----
getwd()

  ##Setting the working directory ----
setwd("C:/Users/Yannis/Desktop/PAMAK_THESIS_R_CODES")

  ##Reading the file in the working directory ----
df<-read.csv("C:/Users/Yannis/Desktop/PAMAK_THESIS_R_CODES/Attrition_project.csv",header = T,sep = ",")
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

#Data manipulation ----
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

  ##Numerical dataset ----

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

  ##Categorical dataset ----
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


#Structural Equation Modeling ----
  ##Data Preparation ----
names(df)
names(numdf)

#Making Education, BusinessTravel, EnvironmentSatisfaction, JobInvolvement, JobLevel, JobSatisfaction, RelationshipSatisfaction, PerformanceRating and WorkLifeBalance pseudonumerical and adding them to the numdf
numdf<-cbind(numdf,as.numeric(df$Education))
numdf<-cbind(numdf,as.numeric(df$BusinessTravel))
numdf<-cbind(numdf,as.numeric(df$EnvironmentSatisfaction))
numdf<-cbind(numdf,as.numeric(df$JobInvolvement))
numdf<-cbind(numdf,as.numeric(df$JobLevel))
numdf<-cbind(numdf,as.numeric(df$JobSatisfaction))
numdf<-cbind(numdf,as.numeric(df$RelationshipSatisfaction))
numdf<-cbind(numdf,as.numeric(df$PerformanceRating))
numdf<-cbind(numdf,as.numeric(df$WorkLifeBalance))

names(numdf)

#Respecifying the names
names(numdf)
names(numdf)[15]<-c("Education")
names(numdf)[16]<-c("BusinessTravel")
names(numdf)[17]<-c("EnvironmentSatisfaction")
names(numdf)[18]<-c("JobInvolvement")
names(numdf)[19]<-c("JobLevel")
names(numdf)[20]<-c("JobSatisfaction")
names(numdf)[21]<-c("RelationshipSatisfaction")
names(numdf)[22]<-c("PerformanceRating")
names(numdf)[23]<-c("WorkLifeBalance")

names(numdf)

#Checking the structure, making sure everything is ok
str(numdf$Education)
str(numdf$BusinessTravel)
str(numdf$EnvironmentSatisfaction)
str(numdf$JobInvolvement)
str(numdf$JobLevel)
str(numdf$JobSatisfaction)
str(numdf$RelationshipSatisfaction)
str(numdf$PerformanceRating)
str(numdf$WorkLifeBalance)

    ###Correlation table ----

#Reassigning the variables for simplicity
X1<-numdf[,1]
X2<-numdf[,2]
X3<-numdf[,3]
X4<-numdf[,4]
X5<-numdf[,5]
X6<-numdf[,6]
X7<-numdf[,7]
X8<-numdf[,8]
X9<-numdf[,9]
X10<-numdf[,10]
X11<-numdf[,11]
X12<-numdf[,12]
X13<-numdf[,13]
X14<-numdf[,14]
X15<-numdf[,15]
X16<-numdf[,16]
X17<-numdf[,17]
X18<-numdf[,18]
X19<-numdf[,19]
X20<-numdf[,20]
X21<-numdf[,21]
X22<-numdf[,22]
X23<-numdf[,23]

#The correlation table of the 23 variables
data.cor<-cor(numdf)

#Renaming the rows and columns of the data.cor
row.names(data.cor)=
colnames(data.cor)=
                c(names(numdf)[1],
                  names(numdf)[2],
                  names(numdf)[3],
                  names(numdf)[4],
                  names(numdf)[5],
                  names(numdf)[6],
                  names(numdf)[7],
                  names(numdf)[8],
                  names(numdf)[9],
                  names(numdf)[10],
                  names(numdf)[11],
                  names(numdf)[12],
                  names(numdf)[13],
                  names(numdf)[14],
                  names(numdf)[15],
                  names(numdf)[16],
                  names(numdf)[17],
                  names(numdf)[18],
                  names(numdf)[19],
                  names(numdf)[20],
                  names(numdf)[21],
                  names(numdf)[22],
                  names(numdf)[23])

#Standard deviation of the variables
sd.cor<-c(sd(X1),
          sd(X2),
          sd(X3),
          sd(X4),
          sd(X5),
          sd(X6),
          sd(X7),
          sd(X8),
          sd(X9),
          sd(X10),
          sd(X11),
          sd(X12),
          sd(X13),
          sd(X14),
          sd(X15),
          sd(X16),
          sd(X17),
          sd(X18),
          sd(X19),
          sd(X20),
          sd(X21),
          sd(X22),
          sd(X23))

#Renaming the names of the sd.cor
names(sd.cor)=c(names(numdf)[1],
                names(numdf)[2],
                names(numdf)[3],
                names(numdf)[4],
                names(numdf)[5],
                names(numdf)[6],
                names(numdf)[7],
                names(numdf)[8],
                names(numdf)[9],
                names(numdf)[10],
                names(numdf)[11],
                names(numdf)[12],
                names(numdf)[13],
                names(numdf)[14],
                names(numdf)[15],
                names(numdf)[16],
                names(numdf)[17],
                names(numdf)[18],
                names(numdf)[19],
                names(numdf)[20],
                names(numdf)[21],
                names(numdf)[22],
                names(numdf)[23])

#Converting correlation to covariance
#data.cov<-cor2cov(data.cor,sd.cor)

  ##Factor Analysis ----
    ###Model Specification ----
SEMfactor<-'
    ##Company Years
    CY =~ YearsSinceLastPromotion  + YearsInCurrentRole + YearsAtCompany + YearsWithCurrManager 
'

    ###Model Identification ----
factor_fit<-sem(SEMfactor,sample.cov=data.cor,sample.nobs=1470);factor_fit
dim(data.cor)
    ###Model Estimation ----
summary(factor_fit,standardized=TRUE,rsquare=TRUE,fit.measures=TRUE)

    ###Model Visualization ----
semPaths(factor_fit,what="paths",whatLabels = "std"
         ,layout="spring"
         ,style="Lisrel"
         ,rotation=2
         ,sizeLat2=10
         ,sizeLat=10
         ,sizeMan=4
         ,residScale=8
         ,font=2
         ,label.cex=1.3)

    ###Model Evaluation ----
#Reliability and validity of the factor

#Convergent validity
#All the loadings are stat. signif. and AVE>0.7
AVE<-reliability(factor_fit);AVE

#Construct validity
fitmeasures(factor_fit,c('chisq','rmsea','gfi','agfi','rmr','nfi','tli','cfi','pgfi','pnfi'))

#Discriminant validity
#Through model's parameter

#Internal reliability
cron_alpha<-reliability(factor_fit)[1];cron_alpha

#Construct reliability
#Computing Composite Reliability (CR)
SL<-standardizedSolution(factor_fit)
SL
SL <- SL$est.std[SL$op == "=~"]
SL

#Residual variances
RE<-1 - SL^2

#CR
CR<-sum(SL)^2 / (sum(SL)^2 + sum(RE))
CR

#AVE reliability
AVE<-reliability(factor_fit)[5];AVE

  ##SEM ----

    ###Constructing the Model based on Theory and the Correlation table----
corrplot(cor(numdf),type="upper",method="number")

    ###Model Specification ----
sem_model<-'
  #Measurement Models
    ##Company Years
    CY =~ YearsSinceLastPromotion  + YearsInCurrentRole + YearsAtCompany + YearsWithCurrManager
   
  #Regressions
  MonthlyIncome~ JobLevel + TotalWorkingYears
  TotalWorkingYears~ JobLevel + NumCompaniesWorked
  JobLevel~ CY
 
  #Exogenous effects
    ##Age
     TotalWorkingYears~ Age
      JobLevel~Age
    
  #Covariances
PercentSalaryHike~~PerformanceRating
'

    ###Model Identification ----
    sem_fit<-sem(sem_model,sample.cov=data.cor,sample.nobs=1470);sem_fit
    
    ###Model Estimation ----
summary(sem_fit,standardized=TRUE,rsquare=TRUE,fit.measures=TRUE)

#Matrix A of RAM approach
View(semMatrixAlgebra(sem_fit, A))

#Matrix S of RAM approach
View(semMatrixAlgebra(sem_fit, S))

#Matrix F of RAM approach
View(semMatrixAlgebra(sem_fit, F))

    ###Model Visualization ----
semPaths(sem_fit,what="paths",whatLabels = "std"
         ,layout="spring"
         ,style="Lisrel"
         ,rotation=2
         ,sizeLat2=10
         ,sizeLat=10
         ,sizeMan=4
         ,residScale=8
         ,font=2
         ,label.cex=1.3)

    ###Model Evaluation ----
model.eval<-fitmeasures(sem_fit,c('chisq','rmsea','gfi','agfi','rmr','nfi','tli','cfi','pgfi','pnfi'));model.eval

    ###Model Modification ----
model_mod<-modificationindices(sem_fit,sort. = T)
subset(model_mod,mi>4)

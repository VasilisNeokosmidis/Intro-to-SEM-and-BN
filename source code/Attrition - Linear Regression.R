#Libraries ----
  ##For Data manipulation and Inspection ----
library(psych)
library(dplyr)
library(DescTools)

  ##For Linear Regression ----
library(olsrr)
library(MASS)
library(MLmetrics)
library(DMwR)
library(normtest)
library(nortest)
library(lmtest)
library(car)

#Data Overview ----
  ##Working directory ----
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

  ##Dataset dimensions ----
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


#Linear Regression ----
#FitAll and FitStart for the algorithm
FitAll <- lm(MonthlyIncome~.,data=numdf)
FitStart <- lm(MonthlyIncome~1, data=numdf)

  ##Forward method ----
step(FitStart,direction="forward", scope=formula(FitAll))
ols_step_forward_aic(FitAll,prem=0.05)
plot(ols_step_forward_aic(FitAll,prem=0.05))
  ##Backward method ----
step(FitAll, direction="backward", trace=F)
ols_step_backward_aic(FitAll,prem=0.05,details=T)
plot(ols_step_backward_aic(FitAll,prem=0.05))

  ##Stepwise method ----
step(FitStart, direction="both", scope=formula(FitAll))
ols_step_both_aic(FitAll, details=T)
plot(ols_step_both_aic(FitAll))
step.model <- stepAIC(FitAll, direction = "both", trace = F)
summary(step.model)

  ##Model comparison-selection ----
finalmodel<-lm(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany,data=numdf) #YearsSinceLastPromotion removed because p-value not good

  ##Checking if there is any better submodel than the finalmodel ----
allmodels<-ols_step_all_possible(finalmodel)
as.data.frame(allmodels) #The lower mallows c,SBIC,AIC, the lower the better 
plot(allmodels) #x-axis:number of predictors y-axis:index of fit, triangle=are the number of best models
ols_step_best_subset(finalmodel) #Reveals which combination of variables is best for any number of predictors

#The final model is retained.

#Dataset division ----

#Variables of interest
dfforsplit <- numdf %>% dplyr::select(MonthlyIncome,TotalWorkingYears,Age,YearsWithCurrManager,YearsAtCompany)
str(dfforsplit)

  ##Divide the dataset into training and test set ----
set.seed(1234)
indices <- sample(nrow(dfforsplit),0.7*nrow(dfforsplit))
train<-dfforsplit[indices,] #70% training
test<-dfforsplit[-indices,] #30% testing
dim(train)
dim(test)

  ##Train the model with the training set ----
myfinalmodel<-lm(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany,data=train)

  ##Prediction of MonthlyIncome in the testing set ----
pred<-predict(myfinalmodel,newdata=test)

#Placing actual and predicted MonthlyIncome in a data frame
actuals_preds <- data.frame(cbind(actuals=test$MonthlyIncome, predicteds=pred))
actuals_preds

#Metrics ----

#The actual MonthlyIncome is in the testing dataset.
actual<-test$MonthlyIncome

  ##MAPE ----
MAPE(pred,actual)

  ##All metrics ----
regr.eval(actual, pred)

  ##Min-max accuracy ----
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max));min_max_accuracy

  ##RMSE ----
RMSE(actual, pred)

#Assumptions ----

  ##Choose the desired variables from the final model ----
newdf<-numdf %>% dplyr::select(MonthlyIncome,TotalWorkingYears,Age,YearsWithCurrManager,YearsAtCompany)
newdf

  ##Data transformation ----

#Saving 2 random values from the original dataset
org.value1<-newdf[199,1];org.value1
org.value2<-newdf[200,1];org.value2

#0 values are present in the dataset, adding the number 1 to the values to take viable logarithmic transformation
for (i in nrow(newdf)){
  newdf<-newdf+1
}

#Checking if it operates well
newdf[199,1]==(org.value1+1)
newdf[199,1]

newdf[200,1]==(org.value2+1)
newdf[200,1]
newdf

#Checking if there are any negative or zero values in the dataset.
which(newdf==0)
which(newdf<0)

#Logarithmic transformation
log_numdf<-log(newdf)
summary(log_numdf)

#Square transformation
sq_numdf<-newdf^2
summary(sq_numdf)

#Square root transformation
sqrt_numdf<-sqrt(newdf)
summary(sqrt_numdf)

#Cube transformation
cube_numdf<-newdf^3
summary(cube_numdf)

#Cube root transformation
cubert_numdf<-sign(newdf) * abs(newdf)^(1/3)
summary(cubert_numdf)

#Sine transformation
sin_numdf<-sin(newdf)
summary(sin_numdf)

#Inverse transformation
inverse_numdf<-1/newdf
summary(inverse_numdf)

#z-value transformation
z_MonthlyIncome<-transform(newdf$MonthlyIncome,method=c('zscore'))
z_TotalWorkingYears<-transform(newdf$TotalWorkingYears,method=c('zscore'))
z_Age<-transform(newdf$Age,method=c('zscore'))
z_YearsWithCurrManager<-transform(newdf$YearsWithCurrManager,method=c('zscore'))
z_YearsAtCompany<-transform(newdf$YearsAtCompany,method=c('zscore'))

z_numdf<-data.frame(z_MonthlyIncome,z_TotalWorkingYears,z_Age,z_YearsWithCurrManager,z_YearsAtCompany)
z_numdf
summary(z_numdf)

#Minmax transformation
minmax_MonthlyIncome<-transform(newdf$MonthlyIncome,method=c('minmax'))
minmax_TotalWorkingYears<-transform(newdf$TotalWorkingYears,method=c('minmax'))
minmax_Age<-transform(newdf$Age,method=c('minmax'))
minmax_YearsWithCurrManager<-transform(newdf$YearsWithCurrManager,method=c('minmax'))
minmax_YearsAtCompany<-transform(newdf$YearsAtCompany,method=c('minmax'))

minmax_numdf<-data.frame(minmax_MonthlyIncome,minmax_TotalWorkingYears,minmax_Age,minmax_YearsWithCurrManager,minmax_YearsAtCompany)
minmax_numdf
summary(minmax_numdf)

#Box-Cox transformation
boxcox_MonthlyIncome<-transform(newdf$MonthlyIncome,method=c('Box-Cox'))
boxcox_TotalWorkingYears<-transform(newdf$TotalWorkingYears,method=c('Box-Cox'))
boxcox_Age<-transform(newdf$Age,method=c('Box-Cox'))
boxcox_YearsWithCurrManager<-transform(newdf$YearsWithCurrManager,method=c('Box-Cox'))
boxcox_YearsAtCompany<-transform(newdf$YearsAtCompany,method=c('Box-Cox'))

boxcox_numdf<-data.frame(boxcox_MonthlyIncome,boxcox_TotalWorkingYears,boxcox_Age,boxcox_YearsWithCurrManager,boxcox_YearsAtCompany)
boxcox_numdf
names(boxcox_numdf)
summary(boxcox_numdf)

  ##Modeling ----

#Original model
finalmodel<-lm(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany,data=numdf)

#Logarithmic model
log_finalmodel<-lm(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany,data=log_numdf)

#Square model
sq_finalmodel<-lm(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany,data=sq_numdf)

#Square root model
sqrt_finalmodel<-lm(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany,data=sqrt_numdf)

#Cube model
cube_finalmodel<-lm(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany,data=cube_numdf)

#Cube root model
cubert_finalmodel<-lm(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany,data=cubert_numdf)

#Sine model
sin_finalmodel<-lm(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany,data=sin_numdf)

#Inverse model
inverse_finalmodel<-lm(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany,data=inverse_numdf)

#z-value model
z_finalmodel<-lm(X_data~X_data.1+X_data.2+X_data.3+X_data.4,data=z_numdf)

#Min-Max model
minmax_finalmodel<-lm(X_data~X_data.1+X_data.2+X_data.3+X_data.4,data=minmax_numdf)

#Box-Cox model
boxcox_finalmodel<-lm(X_data~X_data.1+X_data.2+X_data.3+X_data.4,data=boxcox_numdf)

  ##Distribution of residuals - Linearity ----
par(mfrow=c(1,3))

  ##Linear model specification? - Ramsey RESET ----
#If p-value>0.05, then we should consider adding second and third powers of the independent
summary(finalmodel)
resettest(finalmodel,power=2:3,type="regressor",data=numdf) 

#Original model
plot(finalmodel,1,main = "Linearity - Original model")

#Logarithmic model
plot(log_finalmodel,1,main = "Linearity - Logarithmic model")

#Square model
plot(sq_finalmodel,1,main = "Linearity - Square model") #Nice!

#Square root model
plot(sqrt_finalmodel,1,main = "Linearity - Square root model")

#Cube model
plot(cube_finalmodel,1,main = "Linearity - Cube model")

#Cube root model
plot(cubert_finalmodel,1,main = "Linearity -  Cube root model")

#Sine model
plot(sin_finalmodel,1,main = "Linearity -  Sine model") #Nice!

#Inverse model
plot(inverse_finalmodel,1,main = "Linearity -  Inverse model")

#z-value model
plot(z_finalmodel,1,main = "Linearity -  z-value model")

#Minmax model
plot(minmax_finalmodel,1,main = "Linearity -  Minmax model")

#Box-Cox model
plot(boxcox_finalmodel,1,main = "Linearity -  Box-cox model")

  ##Normal Q-Q - Normality of the residuals ----

    ###Normal Q-Q plots ----
par(mfrow=c(1,3))

#Original model
plot(finalmodel,2,main = "Normality of the residuals - Original model")

#Logarithmic model
plot(log_finalmodel,2,main = "Normality of the residuals - Logarithmic model")

#Square model
plot(sq_finalmodel,2,main = "Normality of the residuals - Square model model")

#Square root model
plot(sqrt_finalmodel,2,main = "Normality of the residuals - Square root model")

#Cube model
plot(cube_finalmodel,2,main = "Normality of the residuals - Cube model")

#Cube root model
plot(cubert_finalmodel,2,main = "Normality of the residuals - Cube root model")

#Sine model
plot(sin_finalmodel,2,main = "Normality of the residuals - Sine model")

#Inverse model
plot(inverse_finalmodel,2,main = "Normality of the residuals - Inverse model")

#z-value model
plot(z_finalmodel,2,main = "Normality of the residuals - z-value model")

#Minmax model
plot(minmax_finalmodel,2,main = "Normality of the residuals - Minmax model")

#Box-Cox model
plot(boxcox_finalmodel,2,main = "Normality of the residuals - Box-Cox model")

    ###Boxplots ----
par(mfrow=c(1,3))

#Original model
boxplot(finalmodel$residuals,main = "Boxplot of the residuals - Original model")
length(boxplot.stats(finalmodel$residuals)$out)

#Logarithmic model
boxplot(log_finalmodel$residuals,main = "Boxplot of the residuals - Logarithmic model")

#Square model
boxplot(sq_finalmodel$residuals,main = "Boxplot of the residuals - Square model model")

#Square root model
boxplot(sqrt_finalmodel$residuals,main = "Boxplot of the residuals - Square root model")
length(boxplot.stats(sqrt_finalmodel$residuals)$out)

#Cube model
boxplot(cube_finalmodel$residuals,main = "Boxplot of the residuals - Cube model")

#Cube root model
boxplot(cubert_finalmodel$residuals,main = "Boxplot of the residuals - Cube root model")
length(boxplot.stats(cubert_finalmodel$residuals)$out)

#Sine model
boxplot(sin_finalmodel$residuals,main = "Boxplot of the residuals - Sine model")

#Inverse model
boxplot(inverse_finalmodel$residuals,main = "Boxplot of the residuals - Inverse model")

#z-value model
boxplot(z_finalmodel$residuals,main = "Boxplot of the residuals - z-value model")

#Minmax model
boxplot(minmax_finalmodel$residuals,main = "Boxplot of the residuals - Minmax model")

#Box-Cox model
boxplot(boxcox_finalmodel$residuals,main = "Boxplot of the residuals - Box-Cox model")

    ###Histograms ----
par(mfrow=c(1,3))

#Original model
hist(finalmodel$residuals,main = "Histogram of the residuals - Original model")

#Logarithmic model
hist(log_finalmodel$residuals,main = "Histogram of the residuals - Logarithmic model")

#Square model
hist(sq_finalmodel$residuals,main = "Histogram of the residuals - Square model model")

#Square root model
hist(sqrt_finalmodel$residuals,main = "Histogram of the residuals - Square root model")

#Cube model
hist(cube_finalmodel$residuals,main = "Histogram of the residuals - Cube model")

#Cube root model
hist(cubert_finalmodel$residuals,main = "Histogram of the residuals - Cube root model")

#Sine model
hist(sin_finalmodel$residuals,main = "Histogram of the residuals - Sine model")

#Inverse model
hist(inverse_finalmodel$residuals,main = "Histogram of the residuals - Inverse model")

#z-value model
hist(z_finalmodel$residuals,main = "Histogram of the residuals - z-value model")

#Minmax model
hist(minmax_finalmodel$residuals,main = "Histogram of the residuals - Minmax model")

#Box-Cox model
hist(boxcox_finalmodel$residuals,main = "Histogram of the residuals - Box-Cox model")

    ###Studentized residual distributions ----
par(mfrow=c(1,2))

#Function which produces the out-of-interval percentage
out.of.range.percent<-function(a){
  num.res.student<-as.numeric(a)
  
  out.of.range<-subset(num.res.student,num.res.student > 2 | num.res.student < -2)
  
  percentage<-(length(out.of.range)/1470)*100
  return(percentage)
}

#Original model
plot(rstudent(finalmodel),pch= 15, cex= .5,ylab = "Studentized Residuals - Original model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#Logarithmic model
plot(rstudent(log_finalmodel),pch= 15, cex= .5,ylab = "Studentized Residuals - Logarithmic model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#Square model
plot(rstudent(sq_finalmodel),pch= 15, cex= .5,ylab = "Studentized Residuals - Square model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#Square root model
plot(rstudent(sqrt_finalmodel),pch= 15, cex= .5,ylab = "Studentized Residuals - Square root model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#Cube model
plot(rstudent(cube_finalmodel),pch= 15, cex= .5,ylab = "Studentized Residuals - Cube model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#Cube root model
plot(rstudent(cubert_finalmodel),pch= 15, cex= .5,ylab = "Studentized Residuals - Cube root model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#Sine model
plot(rstudent(sin_finalmodel),pch= 15, cex= .5,ylab = "Studentized Residuals - Sine model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#Inverse model
plot(rstudent(inverse_finalmodel),pch= 15, cex= .5,ylab = "Studentized Residuals - Inverse model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#z-value model
plot(rstudent(z_finalmodel),pch= 15, cex= .5,ylab = "Studentized Residuals - z-value model")
abline(h=c(-2,0,2), lty= c(2,1,2)) # TIIIIIIIIIIIIIII

#Minmax model
plot(rstudent(minmax_finalmodel),pch= 15, cex= .5,ylab = "Studentized Residuals - Minmax model")
abline(h=c(-2,0,2), lty= c(2,1,2)) # TIIIIIIIIIIIIIII

#Box-Cox model
plot(rstudent(boxcox_finalmodel),pch= 15, cex= .5,ylab = "Studentized Residuals - Box-Cox model")
abline(h=c(-2,0,2), lty= c(2,1,2)) # TIIIIIIIIIIIIIII

#Creating a data frame with the out of interval percentages

percent.df <- data.frame(transformation=c('Original',
                                          'Logarithmic',
                                          'Square',
                                          'Square root',
                                          'Cube',
                                          'Cube root',
                                          'Sine',
                                          'Inverse',
                                          'z-value',
                                          'MinMax',
                                          'Box-cox'),
                         Out_of_interval_percent = c(out.of.range.percent(rstudent(finalmodel)),
                                                     out.of.range.percent(rstudent(log_finalmodel)),
                                                     out.of.range.percent(rstudent(sq_finalmodel)),
                                                     out.of.range.percent(rstudent(sqrt_finalmodel)),
                                                     out.of.range.percent(rstudent(cube_finalmodel)),
                                                     out.of.range.percent(rstudent(cubert_finalmodel)),
                                                     out.of.range.percent(rstudent(sin_finalmodel)),
                                                     out.of.range.percent(rstudent(inverse_finalmodel)),
                                                     out.of.range.percent(rstudent(z_finalmodel)),
                                                     out.of.range.percent(rstudent(minmax_finalmodel)),
                                                     out.of.range.percent(rstudent(boxcox_finalmodel))))
#Sorting the dataset, asceding order
percent.df %>% arrange(Out_of_interval_percent)

    ###Standardized residual distributions ----

#Function which produces the out-of-interval percentage
out.of.range.percent.2<-function(a){
  num.res.standard<-as.numeric(a)
  
  out.of.range<-subset(num.res.standard,num.res.standard > 2 | num.res.standard < -2)
  
  percentage<-(length(out.of.range)/1470)*100
  return(percentage)
}

#Original model
plot(rstandard(finalmodel),pch= 15, cex= .5,ylab = "Standardized Residuals - Original model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#Logarithmic model
plot(rstandard(log_finalmodel),pch= 15, cex= .5,ylab = "Standardized Residuals - Logarithmic model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#Square model
plot(rstandard(sq_finalmodel),pch= 15, cex= .5,ylab = "Standardized Residuals - Square model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#Square root model
plot(rstandard(sqrt_finalmodel),pch= 15, cex= .5,ylab = "Standardized Residuals - Square root model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#Cube model
plot(rstandard(cube_finalmodel),pch= 15, cex= .5,ylab = "Standardized Residuals - Cube model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#Cube root model
plot(rstandard(cubert_finalmodel),pch= 15, cex= .5,ylab = "Standardized Residuals - Cube root model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#Sine model
plot(rstandard(sin_finalmodel),pch= 15, cex= .5,ylab = "Standardized Residuals - Sine model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#Inverse model
plot(rstandard(inverse_finalmodel),pch= 15, cex= .5,ylab = "Standardized Residuals - Inverse model")
abline(h=c(-2,0,2), lty= c(2,1,2))

#z-value model
plot(rstandard(z_finalmodel),pch= 15, cex= .5,ylab = "Standardized Residuals - z-value model")
abline(h=c(-2,0,2), lty= c(2,1,2)) # TIIIIIIIIIIIIIII

#Minmax model
plot(rstandard(minmax_finalmodel),pch= 15, cex= .5,ylab = "Standardized Residuals - Minmax model")
abline(h=c(-2,0,2), lty= c(2,1,2)) # TIIIIIIIIIIIIIII

#Box-Cox model
plot(rstandard(boxcox_finalmodel),pch= 15, cex= .5,ylab = "Standardized Residuals - Box-Cox model")
abline(h=c(-2,0,2), lty= c(2,1,2)) # TIIIIIIIIIIIIIII

#Creating a data frame with the out of interval percentages

percent.df.2 <- data.frame(transformation=c('Original',
                                            'Logarithmic',
                                            'Square',
                                            'Square root',
                                            'Cube',
                                            'Cube root',
                                            'Sine',
                                            'Inverse',
                                            'z-value',
                                            'MinMax',
                                            'Box-cox'),
                           Out_of_interval_percent = c(out.of.range.percent.2(rstandard(finalmodel)),
                                                       out.of.range.percent.2(rstandard(log_finalmodel)),
                                                       out.of.range.percent.2(rstandard(sq_finalmodel)),
                                                       out.of.range.percent.2(rstandard(sqrt_finalmodel)),
                                                       out.of.range.percent.2(rstandard(cube_finalmodel)),
                                                       out.of.range.percent.2(rstandard(cubert_finalmodel)),
                                                       out.of.range.percent.2(rstandard(sin_finalmodel)),
                                                       out.of.range.percent.2(rstandard(inverse_finalmodel)),
                                                       out.of.range.percent.2(rstandard(z_finalmodel)),
                                                       out.of.range.percent.2(rstandard(minmax_finalmodel)),
                                                       out.of.range.percent.2(rstandard(boxcox_finalmodel))))
#Sorting the dataset, asceding order
percent.df.2 %>% arrange(Out_of_interval_percent)

#Presenting the out of interval percentage of both standardized and studentized
stand_and_stud<-cbind(percent.df,percent.df.2$Out_of_interval_percent);stand_and_stud

#Labeling the variables to be more representative of their content 
colnames(stand_and_stud)[2]<-c("Out of interval % - Standard")
colnames(stand_and_stud)[3]<-c("Out of interval % - Student")
stand_and_stud


    ###Diagnostic Tests ----

#H_0: Residuals are normally distributed
#H_1: Residuals are not normally distributed

      #### Shapiro-Wilk normality test ----

#Original model
shapiro.test(finalmodel$residuals) #H_1

#Logarithmic model
shapiro.test(log_finalmodel$residuals) #H_1

#Square model
shapiro.test(sq_finalmodel$residuals) #H_1

#Square root model
shapiro.test(sqrt_finalmodel$residuals) #H_0

#Cube model
shapiro.test(cube_finalmodel$residuals) #H_1

#Cube root model
shapiro.test(cubert_finalmodel$residuals) #H_1

#Sine model
shapiro.test(sin_finalmodel$residuals) #H_1

#Inverse model
shapiro.test(inverse_finalmodel$residuals) #H_1

#z-value model
shapiro.test(z_finalmodel$residuals) #H_1

#Minmax model
shapiro.test(minmax_finalmodel$residuals) #H_1

#Box-Cox model
shapiro.test(boxcox_finalmodel$residuals) #H_1

      ####Jarque-Bera normality test ----

#Original model
jb.norm.test(finalmodel$residuals) #H_1

#Logarithmic model
jb.norm.test(log_finalmodel$residuals) #H_1

#Square model
jb.norm.test(sq_finalmodel$residuals) #H_0

#Square root model
jb.norm.test(sqrt_finalmodel$residuals) #H_0

#Cube model
jb.norm.test(cube_finalmodel$residuals) #H_1

#Cube root model
jb.norm.test(cubert_finalmodel$residuals) #H_0

#Sine model
jb.norm.test(sin_finalmodel$residuals) #H_1

#Inverse model
jb.norm.test(inverse_finalmodel$residuals) #H_1

#z-value model
jb.norm.test(z_finalmodel$residuals) #H_1

#Minmax model
jb.norm.test(minmax_finalmodel$residuals) #H_1

#Box-Cox model
jb.norm.test(boxcox_finalmodel$residuals) #H_1

      ####Anderson-Darling normality test ----

#Original model
ad.test(finalmodel$residuals) #H_1

#Logarithmic model
ad.test(log_finalmodel$residuals) #H_1

#Square model
ad.test(sq_finalmodel$residuals) #H_1

#Square root model
ad.test(sqrt_finalmodel$residuals) #H_1

#Cube model
ad.test(cube_finalmodel$residuals) #H_1

#Cube root model
ad.test(cubert_finalmodel$residuals) #H_1

#Sine model
ad.test(sin_finalmodel$residuals) #H_1

#Inverse model
ad.test(inverse_finalmodel$residuals) #H_1

#z-value model
ad.test(z_finalmodel$residuals) #H_1

#Minmax model
ad.test(minmax_finalmodel$residuals) #H_1

#Box-Cox model
ad.test(boxcox_finalmodel$residuals) #H_1

      ####Cramer-von Mises normality test ----

#Original model
cvm.test(finalmodel$residuals) #H_1

#Logarithmic model
cvm.test(log_finalmodel$residuals) #H_1

#Square model
cvm.test(sq_finalmodel$residuals) #H_1

#Square root model
cvm.test(sqrt_finalmodel$residuals) #H_1

#Cube model
cvm.test(cube_finalmodel$residuals) #H_1

#Cube root model
cvm.test(cubert_finalmodel$residuals) #H_1

#Sine model
cvm.test(sin_finalmodel$residuals) #H_1

#Inverse model
cvm.test(inverse_finalmodel$residuals) #H_1

#z-value model
cvm.test(z_finalmodel$residuals) #H_1

#Minmax model
cvm.test(minmax_finalmodel$residuals) #H_1

#Box-Cox model
cvm.test(boxcox_finalmodel$residuals) #H_1

      ####All normality tests ----
#Original model
ols_test_normality(finalmodel$residuals)

#Logarithmic model
ols_test_normality(log_finalmodel$residuals) 

#Square model
ols_test_normality(sq_finalmodel$residuals) 

#Square root model
ols_test_normality(sqrt_finalmodel$residuals) #Nice!

#Cube model 
ols_test_normality(cube_finalmodel$residuals) 

#Cube root model
ols_test_normality(cubert_finalmodel$residuals) 

#Sine model
ols_test_normality(sin_finalmodel$residuals) 

#Inverse model
ols_test_normality(inverse_finalmodel$residuals)

#z-value model
ols_test_normality(z_finalmodel$residuals)

#Minmax model
ols_test_normality(minmax_finalmodel$residuals) 

#Box-Cox model
ols_test_normality(boxcox_finalmodel$residuals)

  ##Scale-Location - Homoscedasticity of the variance ----

#When variables are transformed to remedy normality, it might remedy heteroscedasticity as well
par(mfrow=c(1,3))

#Original model
plot(finalmodel,3,main = "Homoscedasticity of the variance - Original model")

#Logarithmic model
plot(log_finalmodel,3,main = "Homoscedasticity of the variance - Logarithmic model")

#Square model
plot(sq_finalmodel,3,main = "Homoscedasticity of the variance - Square model model")

#Square root model
plot(sqrt_finalmodel,3,main = "Homoscedasticity of the variance - Square root model")

#Cube model
plot(cube_finalmodel,3,main = "Homoscedasticity of the variance - Cube model")

#Cube root model
plot(cubert_finalmodel,3,main = "Homoscedasticity of the variance - Cube root model")

#Sine model
plot(sin_finalmodel,3,main = "Homoscedasticity of the variance - Sine model")

#Inverse model
plot(inverse_finalmodel,3,main = "Homoscedasticity of the variance - Inverse model")

#z-value model
plot(z_finalmodel,3,main = "Homoscedasticity of the variance - z-value model")

#Minmax model
plot(minmax_finalmodel,3,main = "Homoscedasticity of the variance - Minmax model")

#Box-Cox model
plot(boxcox_finalmodel,3,main = "Homoscedasticity of the variance - Box-Cox model")

    ###Diagnostic tests ----
      ####Studentized Breusch-Pagan test ----
#H_0: Homoscedasticity
#H_1: Heteroscedasticity

#Original model
bptest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=numdf, studentize = TRUE) #H_1

#Logarithmic model
bptest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=log_numdf, studentize = TRUE) #H_1

#Square model
bptest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=sq_numdf, studentize = TRUE) #H_1 

#Square root model
bptest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=sqrt_numdf, studentize = TRUE) #H_1

#Cube model
bptest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=cube_numdf, studentize = TRUE) #H_1 

#Cube root model
bptest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=cubert_numdf, studentize = TRUE) #H_1 

#Sine model 
bptest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=sin_numdf, studentize = TRUE) #H_1

#Inverse model
bptest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=inverse_numdf, studentize = TRUE) #H_1

#z-value model
bptest(X_data~X_data.1+X_data.2+X_data.3+X_data.4, data=z_numdf, studentize = TRUE) #H_1
str(z_numdf)

#Minmax model
bptest(X_data~X_data.1+X_data.2+X_data.3+X_data.4, data=minmax_numdf, studentize = TRUE) #H_1

#Box-Cox model
bptest(X_data~X_data.1+X_data.2+X_data.3+X_data.4, data=boxcox_numdf, studentize = TRUE) #H_1

  ##Cook's distance - Influential cases ----
par(mfrow=c(1,3))

#Original model
plot(finalmodel,4,main = "Residuals vs Leverage - Infuential cases - Original model")
cooks.distance(finalmodel)
sort(round(cooks.distance(finalmodel),10),decreasing=T)

#Logarithmic model
plot(log_finalmodel,4,main = "Infuential cases - Logarithmic model")
cooks.distance(log_finalmodel)
sort(round(cooks.distance(log_finalmodel),12),decreasing=T)

#Square model
plot(sq_finalmodel,4,main = "Infuential cases - Square model model")
cooks.distance(sq_finalmodel)
sort(round(cooks.distance(sq_finalmodel),12),decreasing=T)

#Square root model
plot(sqrt_finalmodel,4,main = "Infuential cases - Square root model") 
cooks.distance(sqrt_finalmodel)
sort(round(cooks.distance(sqrt_finalmodel),10),decreasing=T)

#Cube model
plot(cube_finalmodel,4,main = "Infuential cases - Cube model") 
cooks.distance(cube_finalmodel)
sort(round(cooks.distance(cube_finalmodel),11),decreasing=T)

#Cube root model
plot(cubert_finalmodel,4,main = "Infuential cases - Cube root model") 
cooks.distance(cubert_finalmodel)
sort(round(cooks.distance(cubert_finalmodel),11),decreasing=T)

#Sine model
plot(sin_finalmodel,4,main = "Infuential cases - Sine model") 
cooks.distance(sin_finalmodel)
sort(round(cooks.distance(sin_finalmodel),10),decreasing=T)

#Inverse model
plot(inverse_finalmodel,4,main = "Infuential cases - Inverse model") 
cooks.distance(inverse_finalmodel)
sort(round(cooks.distance(inverse_finalmodel),15),decreasing=T)

#z-value model
plot(z_finalmodel,main = "Infuential cases - z-value model") #4th
cooks.distance(z_finalmodel)
sort(round(cooks.distance(z_finalmodel),10),decreasing=T)

#Minmax model
plot(minmax_finalmodel,main = "Infuential cases - Minmax model") #4th
cooks.distance(minmax_finalmodel)
sort(round(cooks.distance(minmax_finalmodel),10),decreasing=T)

#Box-Cox model
plot(boxcox_finalmodel,main = "Infuential cases - Box-Cox model") #4th
cooks.distance(boxcox_finalmodel)
sort(round(cooks.distance(boxcox_finalmodel),9),decreasing=T)

  ##Multicollinearity ----

#When multicollinearity occurs, usually the variable selection is problematic, one pair of variables is highly linear, one of them must be removed

#SMALLER THAN 5 IS GOOD

#Original model
vif(finalmodel)

#Logarithmic model
vif(log_finalmodel)

#Square model
vif(sq_finalmodel)

#Square root model
vif(sqrt_finalmodel)

#Cube model
vif(cube_finalmodel)

#Cube root model
vif(cubert_finalmodel)

#Sine model
vif(sin_finalmodel)

#Inverse model
vif(inverse_finalmodel)

#z-value model
vif(z_finalmodel)

#Minmax model
vif(minmax_finalmodel)

#Box-Cox model
vif(boxcox_finalmodel)

#Creating a data frame with all values
multicollinearity<-rbind("Original model"=vif(finalmodel),
                         "Logarithmic model"=vif(log_finalmodel),
                         'Square model'=vif(sq_finalmodel),
                         'Square root model'=vif(sqrt_finalmodel),
                         'Cube model'=vif(cube_finalmodel),
                         'Cube root model'=vif(cubert_finalmodel),
                         "Sine model"=vif(sin_finalmodel),
                         'Inverse model'=vif(inverse_finalmodel),
                         "z-value model"=vif(z_finalmodel),
                         'Minmax model'=vif(minmax_finalmodel),
                         'Box-Cox model'=vif(boxcox_finalmodel));multicollinearity

#The <5 criteria is met in every data transformation.

  ##No autocorrelation ----

#Solution: Again the variable selection is problematic regarding a pair of variables.

#Original model
dwtest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=numdf) #H_0

#Logarithmic model
dwtest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=log_numdf) #H_0

#Square model
dwtest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=sq_numdf) #H_0 

#Square root model
dwtest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=sqrt_numdf) #H_0

#Cube model
dwtest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=cube_numdf) #H_0 

#Cube root model
dwtest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=cubert_numdf) #H_0 

#Sine model
dwtest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=sin_numdf) #H_0

#Inverse model
dwtest(MonthlyIncome~TotalWorkingYears+Age+YearsWithCurrManager+YearsAtCompany, data=inverse_numdf) #H_0

#z-value model
dwtest(X_data~X_data.1+X_data.2+X_data.3+X_data.4, data=z_numdf) #H_0

#Minmax model
dwtest(X_data~X_data.1+X_data.2+X_data.3+X_data.4, data=minmax_numdf) #H_0

#Box-Cox model
dwtest(X_data~X_data.1+X_data.2+X_data.3+X_data.4, data=boxcox_numdf) #H_1

  ##Library for more assumptions ----

#Library is called gvlma

#Original model
gvlma::gvlma(finalmodel)

#Logarithmic model
gvlma::gvlma(log_finalmodel) 

#Square model
gvlma::gvlma(sq_finalmodel)

#Square root model
gvlma::gvlma(sqrt_finalmodel)

#Cube model
gvlma::gvlma(cube_finalmodel)

#Cube root model
gvlma::gvlma(cubert_finalmodel) 

#Sine model
gvlma::gvlma(sin_finalmodel)

#Inverse model
gvlma::gvlma(inverse_finalmodel) 

#z-value model
gvlma::gvlma(z_finalmodel)

#Minmax model
gvlma::gvlma(minmax_finalmodel)

#Box-Cox model
gvlma::gvlma(boxcox_finalmodel)

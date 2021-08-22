#Libraries ----
  ##For Data Manipulation and Inspection ----
library(dplyr)
library(DescTools)
library(psych)

  ##For Bayesian Networks ----
    ###Necessary to run the junction function ----
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("graph", "RBGL", "Rgraphviz"),force = TRUE)
#Execute until here and choose n which stands for "none"

    ###Load the following libraries with this order ----
library(bnlearn)
library(gRbase)
library(gRain)
library(Rcpp)
library(Rgraphviz)

#Working Directory ----
getwd()

  ##Setting the working directory ----
setwd("C:/Users/Yannis/Desktop/RStudio")

#Data Overview ----
#Reading the file in the working directory
df<-read.csv("C:/Users/Yannis/Desktop/PAMAK_THESIS_R_CODES/Attrition_project.csv",header = T,sep = ",")

  ##Selecting the desired variables for the BN (from a SEM model) ----
names(df)

#Renaming the Age variables, has weird name
names(df)[1]<-c("Age")

df<-df %>% select(Age,
                  JobLevel,
                  MonthlyIncome,
                  NumCompaniesWorked,
                  TotalWorkingYears,
                  YearsAtCompany,
                  YearsInCurrentRole,
                  YearsSinceLastPromotion,
                  YearsWithCurrManager)

#Dataset Inspection
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
str(df)

#Summary statistics of the variables of the dataset
summary(df)
glimpse(df)
describe(df)
Desc(df)

#Data Preparation ----
  ##Age ----
#Information about the variable
df$Age
str(df$Age)
summary(df$Age)
hist_Age<-hist(df$Age,breaks=2)
hist_Age$counts

#Making the Categories
df$Age<-cut(df$Age,breaks=c(0,20,40,60),labels = c("young","adult","old"),include.lowest=T)

#Making Age a factor
df$Age<-factor(df$Age,levels =c("young","adult","old"))
df$Age
str(df$Age)

#Summing to 1470?
sum(table(df$Age))

#The variable was made according to the hist count?
hist_Age$counts==table(df$Age)

  ##JobLevel ----
#Information about the variable
df$JobLevel
str(df$JobLevel)
summary(df$JobLevel)
hist_JbL<-hist(df$JobLevel,breaks=2)
hist_JbL$counts

#Making the Categories
df$JobLevel<-cut(df$JobLevel,breaks=c(0,2,4,6),labels = c("low","good","excellent"),include.lowest=T)

#Making Age a factor
df$JobLevel<-factor(df$JobLevel,levels =c("low","good","excellent"))
df$JobLevel
str(df$JobLevel)

#Summing to 1470?
sum(table(df$JobLevel))

#The variable was made according to the hist count?
hist_JbL$counts==table(df$JobLevel)

  ##MonthlyIncome ----
#Information about the variable
df$MonthlyIncome
str(df$MonthlyIncome)
summary(df$MonthlyIncome)
hist_Mnl<-hist(df$MonthlyIncome,breaks=3)
hist_Mnl$counts

#Making the Categories
df$MonthlyIncome<-cut(df$MonthlyIncome,breaks=c(0,5000,10000,15000,20000),labels = c("low","medium","high","very high"),include.lowest=T)

#Making Age a factor
df$MonthlyIncome<-factor(df$MonthlyIncome,levels =c("low","medium","high","very high"))
df$MonthlyIncome
str(df$MonthlyIncome)

#Summing to 1470?
sum(table(df$MonthlyIncome))

#The variable was made according to the hist count?
hist_Mnl$counts==table(df$MonthlyIncome)

  ##NumCompaniesWorked  ----
#Information about the variable
df$NumCompaniesWorked
str(df$NumCompaniesWorked)
summary(df$NumCompaniesWorked)
hist_NCW<-hist(df$NumCompaniesWorked,breaks=4)
hist_NCW$counts

#Making the Categories
df$NumCompaniesWorked<-cut(df$NumCompaniesWorked,breaks=c(0,2,4,6,8,10),labels =c("[0,2]","(2,4]","(4,6]","(6,8]","(8,10]"),include.lowest=T)

#Making Age a factor
df$NumCompaniesWorked<-factor(df$NumCompaniesWorked,levels =c("[0,2]","(2,4]","(4,6]","(6,8]","(8,10]"))
df$NumCompaniesWorked
str(df$NumCompaniesWorked)

#Summing to 1470?
sum(table(df$NumCompaniesWorked))

#The variable was made according to the hist count?
hist_NCW$counts==table(df$NumCompaniesWorked)

  ##TotalWorkingYears ----
#Information about the variable
df$TotalWorkingYears
str(df$TotalWorkingYears)
summary(df$TotalWorkingYears)
hist_TWY<-hist(df$TotalWorkingYears,breaks=3)
hist_TWY$counts

#Making the Categories
df$TotalWorkingYears<-cut(df$TotalWorkingYears,breaks=c(0,10,20,30,40),labels =c("[0,10]","(10,20]","(20,30]","(30,40]"),include.lowest=T)

#Making Age a factor
df$TotalWorkingYears<-factor(df$TotalWorkingYears,levels =c("[0,10]","(10,20]","(20,30]","(30,40]"))
df$TotalWorkingYears
str(df$TotalWorkingYears)

#Summing to 1470?
sum(table(df$TotalWorkingYears))

#The variable was made according to the hist count?
hist_TWY$counts==table(df$TotalWorkingYears)


  ##YearsAtCompany ----
#Information about the variable
df$YearsAtCompany
str(df$YearsAtCompany)
summary(df$YearsAtCompany)
hist_YAC<-hist(df$YearsAtCompany,breaks=3)
hist_YAC$counts

#Making the Categories
df$YearsAtCompany<-cut(df$YearsAtCompany,breaks=c(0,10,20,30,40),labels =c("[0,10]","(10,20]","(20,30]","(30,40]"),include.lowest=T)

#Making Age a factor
df$YearsAtCompany<-factor(df$YearsAtCompany,levels =c("[0,10]","(10,20]","(20,30]","(30,40]"))
df$YearsAtCompany
str(df$YearsAtCompany)

#Summing to 1470?
sum(table(df$YearsAtCompany))

#The variable was made according to the hist count?
hist_YAC$counts==table(df$YearsAtCompany)

  ##YearsInCurrentRole ----
#Information about the variable
df$YearsInCurrentRole
str(df$YearsInCurrentRole)
summary(df$YearsInCurrentRole)
hist_YSL<-hist(df$YearsInCurrentRole,breaks=3)
hist_YSL$counts

#Making the Categories
df$YearsInCurrentRole<-cut(df$YearsInCurrentRole,breaks=c(0,5,10,15,20),labels = c("[0,5]","(5,10]","(10,15]","(15,20)"),include.lowest=T)

#Making YearsInCurrentRole a factor
df$YearsInCurrentRole<-factor(df$YearsInCurrentRole,levels =c("[0,5]","(5,10]","(10,15]","(15,20)"))
df$YearsInCurrentRole
str(df$YearsInCurrentRole)

#Summing to 1470?
sum(table(df$YearsInCurrentRole))

#The variable was made according to the hist count?
hist_YSL$counts==table(df$YearsInCurrentRole)
  
  ##YearsSinceLastPromotion  ----
#Information about the variable
df$YearsSinceLastPromotion
str(df$YearsSinceLastPromotion)
summary(df$YearsSinceLastPromotion)
hist_YSL<-hist(df$YearsSinceLastPromotion,breaks=3)
hist_YSL$counts

#Making the Categories
df$YearsSinceLastPromotion<-cut(df$YearsSinceLastPromotion,breaks=c(0,5,10,15),labels = c("[0,5]","(5,10]","(10,15]"),include.lowest=T)

#Making YearsSinceLastPromotion a factor
df$YearsSinceLastPromotion<-factor(df$YearsSinceLastPromotion,levels =c("[0,5]","(5,10]","(10,15]"))
df$YearsSinceLastPromotion
str(df$YearsSinceLastPromotion)

#Summing to 1470?
sum(table(df$YearsSinceLastPromotion))

#The variable was made according to the hist count?
hist_YSL$counts==table(df$YearsSinceLastPromotion)

  ##YearsWithCurrManager ----
#Information about the variable
df$YearsWithCurrManager
str(df$YearsWithCurrManager)
summary(df$YearsWithCurrManager)
hist_YWC<-hist(df$YearsWithCurrManager,breaks=3)
hist_YWC$counts

#Making the Categories
df$YearsWithCurrManager<-cut(df$YearsWithCurrManager,breaks=c(0,5,10,15,20),labels = c("[0,5]","(5,10]","(10,15]","(15,20)"),include.lowest=T)

#Making YearsWithCurrManager a factor
df$YearsWithCurrManager<-factor(df$YearsWithCurrManager,levels =c("[0,5]","(5,10]","(10,15]","(15,20)"))
df$YearsWithCurrManager
str(df$YearsWithCurrManager)

#Summing to 1470?
sum(table(df$YearsWithCurrManager))

#The variable was made according to the hist count?
hist_YWC$counts==table(df$YearsWithCurrManager)

  ##Structure of the Manipulated Dataset ----
str(df)

#Bayesian Networks ----
  ##Building the Directed Acyclic Graph (DAG) ----
    ###Shorting the variable names ----
names(df)[1:9]<-c("Age","JbL","Mnl","NCW","TWY","YAC","YIC","YSL","YWC")

    ###Empty Bayesian Network Graph ----
empty.net <- empty.graph(nodes = c("Age","JbL","Mnl","NCW","TWY","YAC","YIC","YSL","YWC"))
empty.net
plot(empty.net)

    ###The Initiation of Adding Paths to the Empty Graph ----
DAG <- empty.net

    ###Arc from YearsWithCurrentManager to JobLevel ----
DAG <- set.arc(DAG, from = "YWC", to = "JbL")
plot(DAG)
DAG

    ###Arc from YearsInCurrentRole to JobLevel ----
DAG <- set.arc(DAG, from = "YIC", to = "JbL")
plot(DAG)
DAG

    ###Arc from YearsSinceLastPromotion to JobLevel ----
DAG <- set.arc(DAG, from = "YSL", to = "JbL")
plot(DAG)
DAG

    ###Arc from YearsAtCompany to JobLevel ----
DAG <- set.arc(DAG, from = "YAC", to = "JbL")
plot(DAG)
DAG

    ###Arc from JobLevel to MonthlyIncome ----
DAG <- set.arc(DAG, from = "JbL", to = "Mnl")
plot(DAG)
DAG

    ###Arc from JobLevel to TotalWorkingYears ----
DAG <- set.arc(DAG, from = "JbL", to = "TWY")
plot(DAG)
DAG

    ###Arc from Age to JobLevel ----
DAG <- set.arc(DAG, from = "Age", to = "JbL")
plot(DAG)
DAG

    ###Arc from Age to TotalWorkingYears ----
DAG <- set.arc(DAG, from = "Age", to = "TWY")
plot(DAG)
DAG

    ###Arc from TotalWorkingYears to MonthlyIncome ----
DAG <- set.arc(DAG, from = "TWY", to = "Mnl")
plot(DAG)
DAG

    ###Arc from NumCompaniesWorked to TotalWorkingYears ----
DAG <- set.arc(DAG, from = "NCW", to = "TWY")
plot(DAG)
DAG
    
    ###Plot of the Final DAG ----
plot(DAG,main="The Directed Acyclic Graph (DAG)")
    
    ###Nodes of Final DAG ----
nodes(DAG)
    
    ###Arcs of Final DAG ----
arcs(DAG)

    ###Model String Representation of Final DAG ----
bnlearn::modelstring(DAG)

    ###Matrix Representation of the Final DAG ----
matrix.DAG <- empty.graph(nodes = c("Age","JbL","Mnl","NCW","TWY","YAC","YIC","YSL","YWC"))
arc.set <- matrix(c("YWC", "JbL",
                        "YSL", "JbL",
                        "YIC", "JbL",
                        "YAC", "JbL",
                        "Age", "JbL",
                        "Age", "TWY",
                        "JbL", "Mnl",
                        "JbL", "TWY",
                        "NCW", "TWY",
                        "TWY", "Mnl"),
                      byrow = TRUE, ncol = 2,dimnames = list(NULL, c("from", "to")))
arcs(matrix.DAG) <- arc.set
    
    #Checking if standard and matrix DAG are the same
all.equal(DAG, matrix.DAG)
   
  ##Estimation of the Parameters of DAG ----
    ###Parameter Estimation of the BN with mle ----
bn.estimation.mle <- bn.fit(DAG, data = df, method = "mle")

#Parameters of node Age
bn.estimation.mle$Age

#Parameters of node NCW
bn.estimation.mle$NCW

    ###Parameter Estimation of the BN with bayes ----
bn.estimation.bayes <- bn.fit(DAG, data = df, method = "bayes",iss=10)

#Parameters of node Age
bn.estimation.bayes$Age

#Parameters of node NCW
bn.estimation.bayes$NCW

    ###Number of Parameters of the Network ----
nparams(bn.estimation.bayes)

#Do they have the same number of parameters?
nparams(bn.estimation.bayes)==nparams(bn.estimation.mle)

  ##Network Conditional Independence Tests and Scores ----
    ###Network Arc Strength and Scores of DAG ----
#Conditional Independence Tests - Arc Strength
arcs_power<-arc.strength(DAG, data = df, criterion = "mi")
arcs_power

#Rounding up the strength
arcs_strength<-arcs_power$strength
round(arcs_strength,digits=4)

#BIC
bnlearn::score(DAG, data = df,type = "bic")

#BDE
bnlearn::score(DAG, data = df,type = "bde", iss = 10)

    ###Constructing the DAG2 ----
#Creating a 2nd DAG
DAG2<-DAG

#Repeating the 3 steps capital letter steps when all 3 conditions below are met:
#1)Significant dependence after adding the candidate arc 
#2)Theoretical justification of the candidate arc
#3)BIC and BDE significantly increased after adding the candidate arc 
#Stop when BIC and BDE can no longer be improved and all arcs are relatively supported by the data

#THE 3 STEPS

#ADDING
#DAG2 <- set.arc(DAG2, from = "NCW", to = "YAC")
#plot(DAG2)

#TESTS AND SCORES
#Conditional Independence Test
#arcs_power<-arc.strength(DAG2, data = df, criterion = "mi");arcs_power
#arcs_strength<-arcs_power$strength
#round(arcs_strength,digits=4)

#BIC
#bnlearn::score(DAG2, data = df,type = "bic")

#BDE
#bnlearn::score(DAG2, data = df,type = "bde", iss = 10)

#REMOVING
#DAG2 <- drop.arc(DAG2, from = "NCW", to = "JbL")
#plot(DAG2)

#RESULTS
#Removing arc from NCW to TWY
DAG2 <- drop.arc(DAG2, from = "NCW", to = "TWY")
plot(DAG2)

#Removing arc from YWC to JbL
DAG2 <- drop.arc(DAG2, from = "YWC", to = "JbL")
plot(DAG2)

#Removing arc from YAC to JbL
DAG2 <- drop.arc(DAG2, from = "YAC", to = "JbL")
plot(DAG2)

#Adding arc from YAC to YWC
DAG2 <- set.arc(DAG2, from = "YAC", to = "YWC")
plot(DAG2)

#Adding arc from YAC to YSL
DAG2 <- set.arc(DAG2, from = "YAC", to = "YSL")
plot(DAG2)

#Adding arc from YAC to YIC
DAG2 <- set.arc(DAG2, from = "YAC", to = "YIC")
plot(DAG2)

#Adding arc from Age to YAC
DAG2 <- set.arc(DAG2, from = "Age", to = "YAC")
plot(DAG2)

#Adding arc from YWC to YIC
DAG2 <- set.arc(DAG2, from = "YWC", to = "YIC")
plot(DAG2)

#Adding arc from NCW to YAC
DAG2 <- set.arc(DAG2, from = "NCW", to = "YAC")
plot(DAG2,main="DAG2")
plot(DAG,main="DAG")

    ###Model String of DAG2 ----
modelstring(DAG2)

    ###Network Arc Strength and Scores of DAG2 ----
#Conditional Independence Tests - Arc Strength
arcs_power<-arc.strength(DAG2, data = df, criterion = "mi");arcs_power

#Rounding up the strength
arcs_strength<-arcs_power$strength
round(arcs_strength,digits=4)

#BIC
bnlearn::score(DAG2, data = df,type = "bic")

#BDE
bnlearn::score(DAG2, data = df,type = "bde", iss = 10)

    ###Comparing DAG and DAG2 ----
#BIC
bnlearn::score(DAG, data = df,type = "bic")
bnlearn::score(DAG2, data = df,type = "bic")

#BDE
bnlearn::score(DAG, data = df,type = "bde", iss = 10)
bnlearn::score(DAG2, data = df,type = "bde", iss = 10)

  ##Estimation of the Parameters of DAG2 ----
    ###Parameter Estimation of the BN with mle ----
bn.estimation.mle2 <- bn.fit(DAG2, data = df, method = "mle")

#Parameters of node YSL
bn.estimation.mle2$YSL

#Parameters of node YWC
bn.estimation.mle2$YWC

    ###Parameter Estimation of the BN with bayes ----
bn.estimation.bayes2 <- bn.fit(DAG2, data = df, method = "bayes",iss=10)

#Parameters of node YSL
bn.estimation.bayes2$YSL

#Parameters of node YWC
bn.estimation.bayes2$YWC

    ###Number of Parameters of the Network ----
nparams(bn.estimation.bayes2)

#Do they have the same number of parameters?
nparams(bn.estimation.bayes2)==nparams(bn.estimation.mle2)

  ##Inference through Conditional Probability Tables ----
    ###Exact Inference ----
#Construction of the junction tree
junction <- compile(as.grain(bn.estimation.bayes2))

#Let's observe the levels of each variable and form some queries
str(df)

#The marginal probability of Job Level in the whole dataset
querygrain(junction, nodes = "JbL",type="marginal")$JbL

#The marginal probability of Job Level of adult people
age.adult <- setEvidence(junction, nodes = "Age", states = "adult")
querygrain(age.adult, nodes = "JbL",type="marginal")$JbL

#The marginal probability of Job Level of people given Mnl=very high
Mnl.very_high <- setEvidence(junction, nodes = "Mnl", states = "very high")
querygrain(Mnl.very_high, nodes = "JbL",type="marginal")$JbL

#The joint probability of Age and MonthlyIncome given JbL=low
Age.Mnl_given_JbL.low <- setEvidence(junction, nodes = "JbL", states = "low")
rounding<-querygrain(Age.Mnl_given_JbL.low, nodes = c("Age","Mnl"),type="joint")

#Rounding the results
round(rounding,digits=5)

#The marginal probability of Age and MonthlyIncome
querygrain(Age.Mnl_given_JbL.low, nodes = c("Age", "Mnl"), type = "marginal")

#The conditional probability of NumCompaniesWorked and YearsWithCurrentManager given YearsAtCompany=(10,20]
NCW.YWC_given_YAC10to20<-setEvidence(junction, nodes = "YAC", states = "(10,20]")
querygrain(NCW.YWC_given_YAC10to20, nodes = c("NCW", "YWC"), type = "conditional")

#The probabilities are the same in each row because NCW and YWC are d-separated by YAC
dsep(bn.estimation.bayes2, x = "NCW", y = "YWC", z = "YAC")

    ###Approximate Inference ----
#The probability of Age=adult and Mnl=low given JbL=low
cpquery(bn.estimation.bayes2, event = (Age=='adult') & (Mnl=='low'),evidence = (JbL=='low'))

#The probability of JbL=low and Mnl=low given Age=old and TWY=(20,30] or YAC=(20,30]
cpquery(bn.estimation.bayes2, event = (JbL == "low") & (Mnl == "low"),evidence = ((Age == "old") & (TWY == "(20,30]")) | (YAC == "(20,30]"))

#The probability of JbL=excellent and Mnl=very high given Age=old and TWY=(20,30] or YAC=(20,30]
cpquery(bn.estimation.bayes2, event = (JbL == "excellent") & (Mnl == "very high"),evidence = ((Age == "old") & (TWY == "(20,30]")) | (YAC == "(20,30]"))

#The probability of YAC=[0,10] given Age=old and NCW=(6,8] and TWY=(20,30]
cpquery(bn.estimation.bayes2, event = (YAC == "[0,10]"),evidence = ((Age == "old") & (NCW=="(6,8]") & (TWY == "(20,30]")))

#The probability of YAC=(10,20] given Age=old and NCW=(6,8] and TWY=(20,30]
cpquery(bn.estimation.bayes2, event = (YAC == "(20,30]"),evidence = ((Age == "old") & (NCW=="(6,8]") & (TWY == "(20,30]")))

  ##Graphical Representation of the Bayesian Network ----
    ###Fancy DAG2 plot ----
#DAG2 with fdp layout
graphviz.plot(DAG2,layout='fdp')

#Setting the color of all arcs, nodes and labels to grey
grey_graph <- list(nodes = nodes(DAG2), arcs = arcs(DAG2), col = "grey", textCol = "grey")

#Changing the color of all arcs and nodes of DAG2 to grey
grey_DAG2<-graphviz.plot(DAG2,layout='fdp',highlight = grey_graph)

#Changing the color and width of arcs of the path NCW->YAC->YWC of DAG2 to black
edgeRenderInfo(grey_DAG2) <-list(col = c("NCW~YAC" = "black", "YAC~YWC" = "black"))

#Changing the color of nodes NCW,YAC,YWC of DAG2 to black
nodeRenderInfo(grey_DAG2) <-list(col = c("NCW" = "black", "YAC" = "black","YWC"="black")
                                ,textCol = c("NCW" = "black", "YAC" = "black","YWC"="black"))

#Plotting the customized DAG2
custom_DAG2<-grey_DAG2
renderGraph(custom_DAG2)

    ###Fancy Conditional Probability Distributions ----
#The bar chart of the conditional probability of YearsInCurrentRole given YearsWithCurrentManager and YearsAtCompany
bn.fit.barchart(bn.estimation.bayes2$YIC, main = "YIC",xlab = "Pr(YIC | YWC,YAC)", ylab = "")

#The dot plot of the conditional probability of YearsInCurrentRole given YearsWithCurrentManager and YearsAtCompany
bn.fit.dotplot(bn.estimation.bayes2$YIC, main = "YIC",xlab = "Pr(YIC | YWC,YAC)", ylab = "")

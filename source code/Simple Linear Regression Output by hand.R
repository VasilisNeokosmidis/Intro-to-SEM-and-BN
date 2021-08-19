#Libraries ----
library(dplyr)
library(PerformanceAnalytics)
library(psych)
library(corrplot)
library(ggplot2)
#Creation of the dataset ----
Income<-c(6000,5000,4000,2500,3500,3300,1500,
          3200,3800,3000,2000,4150,3700,2750,1250)
Taxes<-c(900,700,500,350,400,390,200,380,480,370,240,520,410,360,180)
Gender<-c("Male","Female","Male","Female","Female","Female","Female","Male",
          "Male","Female","Female","Female","Female","Male","Male")
Gender<-factor(Gender,levels = c("Male","Female"))
Country<-c("Greece","England","Spain","Poland","Russia","Russia",
           "England","Germany","Germany","Germany","Germany",
           "England","Germany","Greece","England")
Country<-factor(Country,levels = c("Greece","England",
                                   "Spain","Poland","Russia","Germany"))
maritalstatus<-c("Single","Married","Divorced","Single","Single",
                 "Single","Single","Single","Single","Single","Married",
                 "Divorced","Divorced","Divorced","Divorced")
maritalstatus<-factor(maritalstatus,levels = c("Single","Divorced","Married"))
numberofkids<-c(4,5,1,1,1,2,3,2,0,0,0,0,0,0,0)
metresfromsea<-c(100,115,130,190,160,170,350,180,150,200,225,110,126,146,400)
Dept<-c(34562,34123,53423,54623,23166,32142,53442,54673,87543,23461,23523,32457,23126,23521,23842)
df2<-data.frame(Income,Taxes,metresfromsea,Gender,Country,maritalstatus,numberofkids,Dept)
names(df2)<-c("Income","Taxes","House distance from the sea","Gender",
              "Country","Marital Status","Number_of_kids","Dept")
df2

#Numerical variables only
numericaldf2<-df2 %>% dplyr::select('Income','Taxes','House distance from the sea','Dept');numericaldf2

#Covariance matrix ----
cov(numericaldf2)
mycov<-data.frame(cov(numericaldf2));mycov
str(mycov)

#Rounding the values
mycov<-round(mycov,digits = 0);mycov

#Omitting the upper diagonal elements
n<-2
for(i in 1:(ncol(mycov)-1)){
  for(j in n:ncol(mycov)){
    mycov[i,j]<-"-"
  }
  n<-n+1
}
mycov

#Correlation matrix ----
cor(numericaldf2)
mycor<-data.frame(cor(numericaldf2));mycor
str(mycor)

#Rounding the values
mycor<-round(mycor,digits = 2);mycor

#Omitting the upper diagonal elements
n<-2
for(i in 1:(ncol(mycor)-1)){
  for(j in n:ncol(mycor)){
    mycor[i,j]<-"-"
  }
  n<-n+1
}
mycor

  ##Graphical representation ----
chart.Correlation(numericaldf2, histogram = F, pch = 19)
pairs.panels(numericaldf2)
corrplot(cor(numericaldf2), type = "upper", method= "number")

#Simple linear regression model ----
simple.lm<-lm(df2$Income ~ df2$Taxes, data=df2)
summary(simple.lm)
attributes(simple.lm)

  ##Fitted values or predict(lm) ----
simple.lm$fitted.values
df2$simple.lm.fitted.values<-predict(simple.lm);df2$simple.lm.fitted.values

  ##Residuals ----
plot(simple.lm$residuals)

  ##Plotting the lm ----
plot(df2$Income ~ df2$Taxes, xlab=c("Taxes"), ylab=c("Income"), 
     main=c("Plot of y=Income against x=Taxes"))
mtext("Income=510.6772+6.5815(Taxes)", side=3)
abline(simple.lm, col="red")

ggplot(df2, aes(x = Taxes, y = Income)) + geom_point()

##Adding fitted values
ggplot(df2, aes(x = Taxes, y = Income)) +
  geom_point() +
  geom_point(aes(y = simple.lm.fitted.values), shape = 1)

  ##Adding the difference between actual and fitted values ----
ggplot(df2, aes(x = Taxes, y = Income)) +
  geom_segment(aes(xend = Taxes, yend = simple.lm.fitted.values)) +
  geom_point() +
  geom_point(aes(y = simple.lm.fitted.values), shape = 1)

  ##Displaying all the lines in a plot ----
ggplot(df2, aes(x = Taxes, y = Income)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  
  geom_segment(aes(xend = Taxes, yend = simple.lm.fitted.values), alpha = 1, color="red",lwd=1) +
  geom_point() +
  geom_point(aes(y = simple.lm.fitted.values), shape = 1) +
  theme_bw()

  ##Total Sum of Squares (TSS) ----
y<-df2$Income
ymean<-mean(df2$Income)
yminusymean<- (y - ymean)
yminusymeansquared<-(yminusymean)^2
TSS<-sum(yminusymeansquared)
TSS

  ##Residual Sum of Squares (RSS) ----
y<-df2$Income
ypred<-simple.lm$fitted.values
yminusypred<-(y - ypred)
yminusypredsquared<-(yminusypred)^2
RSS<-sum(yminusypredsquared)
RSS

  ##Explained Sum of Squares (ESS) ----
ypred<-simple.lm$fitted.values
ymean<-mean(df2$Income)
ypredminusymean<-(ypred - ymean)
ypredminusymeansquared<-(ypredminusymean)^2
ESS<-sum(ypredminusymeansquared)
ESS

  ##Residual standard error (RSE) ----

###Computing the number of model parameters - 1
k<-length(simple.lm$coefficients)-1;k

###Computing RSS
RSS<-sum(simple.lm$residuals**2);RSS

###Computing the total observations in dataset
n<-length(simple.lm$residuals);n

###Computing the Residual Standard Error (RSE)
simple.lm.RSE<-sqrt(RSS/(n-(1+k)));simple.lm.RSE

  ##Calculation of R^2 ----
    ###1st way:R^2=(TSS-SSE)/TSS ----
firstR2<-(TSS-RSS)/TSS
firstR2

    ###2nd way:R^2=(Var(ypred))/(Var(y)) ----
var(y)
var(ypred)
secondR2<-var(ypred)/var(y)
secondR2

    ###3rd way:R^2=cor(xy)*cor(xy) ----
thirdR2<-cor(x=df2$Income,y=df2$Taxes)^2
thirdR2

  ##Coefficients ----
simple.lm$coefficients

    ###b coefficient of simple.lm ----
x<-data.frame(df2$Taxes - mean(df2$Taxes))
xsquared<-x^2

y<-data.frame(df2$Income - mean(df2$Income))
xtimesy<-x*y

sum_xtimesy<-apply(xtimesy, 2, sum);sum_xtimesy
sum_x<-apply(xsquared, 2, sum);sum_x

b_coefficent<-sum_xtimesy/sum_x
b_coefficent

    ###Standard error of b coefficient ----
std.error.b<-sqrt((sum(simple.lm$residuals^2))/((nrow(df2)-2)*(sum((df2$Taxes-mean(df2$Taxes))^2))));std.error.b

    ###t value of b coefficient ----
tvalue.b<-b_coefficent/std.error.b;tvalue.b

#or

tvalue.b<-cor(df2$Taxes,df2$Income)*(sqrt((nrow(df2)-2)/(1-cor(df2$Taxes,df2$Income)^2)));tvalue.b

    ###p value of b coefficient ----
2*pt(-abs(tvalue.b),df=13)

    ###Correlation between predictor and outcome through b coefficient ----
cor(df2$Income,df2$Taxes)
rcorr<- b_coefficent*(sd(df2$Taxes)/sd(df2$Income));rcorr

    ###a coefficient of simple.lm ----
a_simple<-mean(df2$Income)-(b_coefficent*mean(df2$Taxes));a_simple

    ###Standard error of a coefficient ----
std.error.simple<-sqrt((sum((df2$Income-simple.lm$fitted.values)^2))/(15-2));std.error.simple
std.error.a_simple<-(std.error.simple)*sqrt(((sum(df2$Taxes^2))/(15*(sum((df2$Taxes-mean(df2$Taxes))^2)))))
std.error.a_simple

    ###t value of a coefficient ----
tvalue.a_simple<-a_simple/std.error.a_simple;tvalue.a_simple

    ###p value of a coefficient ----
pvalue.a_simple<-2*pt(-abs(tvalue.a_simple),df=13);pvalue.a_simple

  ##Relationship between covariance and correlation ----
covxy<-cov(df2$Income,df2$Taxes);covxy
sx<-sd(df2$Income);sx
sy<-sd(df2$Taxes);sy
corxy<-covxy/(sx*sy);corxy

  ##F-statistic ----
fstatisticsimple<-((TSS-RSS)/1)/((RSS)/(15-1-1));fstatisticsimple

  ##p-value of F-statistic ----
pf(fstatisticsimple,1,13,lower.tail = F)

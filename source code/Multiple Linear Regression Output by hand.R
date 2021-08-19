#Libraries ----
library(dplyr)
library(PerformanceAnalytics)
library(psych)
library(corrplot)

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

#Multiple linear regression model ----
multi.lm<-lm(Income~Taxes+`House distance from the sea`,data=df2)
summary(multi.lm)
attributes(multi.lm)

  ##Fitted values or predict(lm) ----
multi.lm$fitted.values
df2$multi.lm.fitted.values<-predict(multi.lm);df2$multi.lm.fitted.values

  ##Residuals ----
plot(multi.lm$residuals)

  ##TSS,RSS,ESS ----

    ###Residual Sum of Squares (RSS) ----
RSSmul<-sum((df2$Income - multi.lm$fitted.values)^2);RSSmul

#or

RSSmul<-sum(multi.lm$residuals^2);RSSmul

    ###Explained Sum of Squares (ESS) ----
ESSmul<-sum((multi.lm$fitted.values - mean(df2$Income))^2);ESSmul
ESSmul

#or

ESSmul<-(beta_hat[3]*sum((df2$Taxes*df2$Income)-(mean(df2$Taxes)*mean(df2$Income)))) + (beta_hat[2]*sum((df2$`House distance from the sea`*df2$Income)-(mean(df2$`House distance from the sea`)*mean(df2$Income))));ESSmul
#beta_hat is calculated later
#or

ESSmul<-sum(((df2$Income*multi.lm$fitted.values)-(mean(df2$Income)*mean(multi.lm$fitted.values))));ESSmul

    ###Total Sum of Squares (TSS) ----
#TSS(Total variation of y)=RSS(unexplained variation)+ESS(explained variation) of the mult.lm
TSSmul<-RSSmul+ESSmul
TSSmul

  ##Coefficients ----
multi.lm$coefficients

    ###b1 and b2 of multi.lm ----
n <- nrow(df2)
p <- length(coef(multi.lm))
x <- cbind(rep(1, n), df2$`House distance from the sea`, df2$Taxes)
y <- df2$Income

#b1 and b2
beta_hat <- solve(t(x) %*% x) %*% t(x) %*% y;beta_hat

#or

#Deviation sum of squares
squaredx1<-sum((df2$Taxes-mean(df2$Taxes))^2);squaredx1
squaredx2<-sum((df2$`House distance from the sea`-mean(df2$`House distance from the sea`))^2);squaredx2

#Deviation cross products
x1y<-sum(((df2$Taxes*df2$Income)-(mean(df2$Taxes)*mean(df2$Income))));x1y
x2y<-sum(((df2$`House distance from the sea`*df2$Income)-(mean(df2$`House distance from the sea`)*mean(df2$Income))));x2y
x1x2<-sum(((df2$Taxes*df2$`House distance from the sea`)-(mean(df2$Taxes)*mean(df2$`House distance from the sea`))));x1x2

#b1 and b2
b1<-((squaredx2*x1y)-(x1x2*x2y))/((squaredx1*squaredx2)-((x1x2)^2));b1
b2<-((squaredx1*x2y)-(x1x2*x1y))/((squaredx1*squaredx2)-((x1x2)^2));b2

    ###Standard error of b1 and b2 coefficients ----
var.y.x1.x2<-RSSmul/(nrow(df2)-2-1);var.y.x1.x2

std.error.b1<-sqrt((var.y.x1.x2)/(squaredx1*(1-(cor(df2$`House distance from the sea`,df2$Taxes))^2)));std.error.b1

std.error.b2<-sqrt((var.y.x1.x2)/(squaredx2*(1-(cor(df2$`House distance from the sea`,df2$Taxes))^2)));std.error.b2

    ###t value of b1 and b2 coefficients ----
tvalue.b1<-beta_hat[3]/std.error.b1;tvalue.b1
tvalue.b2<-beta_hat[2]/std.error.b2;tvalue.b2

    ###p value of b1 and b2 coefficients ----
tvalue.b1<-2*pt(-abs(tvalue.b1),df=12);tvalue.b1
tvalue.b2<-2*pt(-abs(tvalue.b2),df=12);tvalue.b2

    ###y-intercept, a ----
mean(df2$Income)
mean(df2$Taxes)
mean(df2$`House distance from the sea`)
a_multi<-mean(df2$Income)-(beta_hat[3]*mean(df2$Taxes))-(beta_hat[2]*mean(df2$`House distance from the sea`));a_multi

    ###Standard error of a coefficient !!!!!!!!!!!!!!!!!!!!!!! ----
#std.error.multi<-
#std.error.a_multi<-

    ###t value of a coefficient !!!!!!!!!!!!!!!!!!! ----
#tvalue.a_multi<-

    ###p value of a coefficient !!!!!!!!!!!!!!!!!!! ----
#pvalue.a_multi<-

  ##F-statistic ----
fstatisticmul<-((TSSmul-RSSmul)/2)/((RSSmul)/(15-2-1))
fstatisticmul

  ##p-value of F-statistic ----
pf(fstatisticmul,2,12,lower.tail = F)

  ##Residual standard error (RSE) ----

###Computing the number of model parameters - 1
k<-length(multi.lm$coefficients)-1;k

###Computing RSS
RSSmul<-sum(multi.lm$residuals**2);RSSmul

###Computing the total observations in dataset
n<-length(multi.lm$residuals);n

###Computing the Residual Standard Error (RSE)
multi.lm.RSE<-sqrt(RSSmul/(n-(1+k)));multi.lm.RSE

  ##Calculation coefficient of R^2 ----
cor(multi.lm$fitted.values,df2$Income)^2

#or

r<-(ESSmul^2)/(ESSmul*TSSmul);r


  ##Raw relations plots ----
yonx1<-lm(df2$Income ~ df2$Taxes)
yonx1res<-resid(yonx1)
plot(df2$Taxes,df2$Income,main = "Income ~ Taxes")

yonx2<-lm(df2$Income ~ df2$`House distance from the sea`)
yonx2res<-resid(yonx2)
plot(df2$`House distance from the sea`,df2$Income,main = "Income ~ HDS")

  ##Partial relation plots ----
    ###Partial relation plot between y and x1 controlling for x2 ----
x1onx2<-lm(df2$Taxes ~ df2$`House distance from the sea`)
x1onx2res<-resid(x1onx2)
plot(yonx2res~x1onx2res,main = "Income|HDS ~ Taxes|HDS")

    ###Partial relation plot between y and x2 controlling for x1 ----
x2onx1<-lm(df2$`House distance from the sea` ~ df2$Taxes)
x2onx1res<-resid(x2onx1)
plot(yonx1res~x2onx1res,main = "Income|Taxes ~ HDS|Taxes")

#House distance from the sea needs logging or something

#Comparing 2 models ----
  ##Model 1 ----
multi.lm<-lm(Income~Taxes+`House distance from the sea`,data=df2)
summary(multi.lm)

  ##Model 2 ----
multi.lm2<-lm(df2$Income~df2$Taxes+log(df2$`House distance from the sea`))
summary(multi.lm2)

    ###Residual Sum of Squares (RSS) ----
RSSmul2<-sum((df2$Income - multi.lm2$fitted.values)^2);RSSmul2

#or

RSSmul2<-sum(multi.lm$residuals^2);RSSmul2

    ###Explained Sum of Squares (ESS) ----
ESSmul2<-sum((multi.lm2$fitted.values - mean(df2$Income))^2)
ESSmul2
ESSmul

#Model 2 is slightly better

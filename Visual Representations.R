#Libraries ----
library(dplyr)
library(ggplot2)

#Creation of the dataset ----
Income<-c(6000,5000,4000,2500,3500,3300,1500,
          3200,3800,3000,2000,4150,3700,2750,1250)
Taxes<-c(900,700,500,350,400,390,200,380,480,370,240,520,410,360,180)
Gender<-c("Male","Female","Male","Female","Female","Female",
          "Female","Male","Male","Female","Female","Female","Female","Male","Male")
Gender<-factor(Gender,levels = c("Male","Female"))
Country<-c("Greece","England","Spain","Poland","Russia","Russia",
           "England","Germany","Germany","Germany","Germany",
           "England","Germany","Greece","England")
Country<-factor(Country,levels = c("Greece","England",
                                   "Spain","Poland","Russia","Germany"))
maritalstatus<-c("Single","Married","Divorced","Single","Single",
                 "Single","Single","Single","Single","Single","Married",
                 "Divorced","Divorced","Divorced","Divorced")
maritalstatus<-factor(maritalstatus,
                      levels = c("Single","Divorced","Married"))
numberofkids<-c(4,5,1,1,1,2,3,2,0,0,0,0,0,0,0)
df<-data.frame(Income,Taxes,Gender,Country,maritalstatus,numberofkids)
names(df)<-c("Income","Taxes","Gender",
             "Country","Marital Status","Number_of_kids")
df

#Boxplot with a potential outliers ----
V1 <- c(2,3,3.6,0.3,1,0.8,5,4,2,2.8,6.8,0.5,4.1,0.2,2.5,1.1,30)
boxplot(V1)

#Frequency pie chart ----
frequencytable<-count(df, "Gender")
slices<-c(6,9)
lbls <- c("Male (6)", "Female (9)")
pie(slices, labels = lbls, main="Frequency pie of Gender", 
    col=rainbow(length(lbls)))

#Relative frequency pie chart ----
frequencytable<-count(df, "Gender")
slices<-c((6/15)*100,(9/15)*100)
lbls <- c("Male (40%)", "Female (60%)")
color=c("black","orange")
pie(slices, labels = lbls, 
    main="Relative frequency pie of Gender", col=color) 

#Frequency bar chart ----
counts<-table(df$Gender)
barplot(counts, main=" Frequency bar plot of Gender", 
        ylab="Number of observations",
        xlab="Gender",col=rainbow(length(lbls)),ylim=c(0,10))

#Relative frequency bar chart ----
counts<-table(df$Gender)
color=c("black","orange")
barplot(counts/nrow(df), main=" Relative frequency bar plot of Gender",
        xlab="Relative frequency of Gender",ylab="Gender",col=color,horiz=T)

  ##Mixed variable bar chart ----
maledf<-subset(df,df$Gender=="Male")
femaledf<-subset(df,df$Gender=="Female")
maledf[,1]<- maledf$Income/nrow(maledf)
femaledf[,1]<- femaledf$Income/nrow(femaledf)
meanincomedf<-rbind(maledf,femaledf)
names(meanincomedf)[1]<-c("Mean_Income")
ggplot(meanincomedf, aes(Gender, Mean_Income)) + 
  geom_bar(stat = "identity",fill = "darkblue")+
  labs(title = "Bar chart of the mean Income of each Gender")

#Descending Pareto chart ----
table(df$`Marital Status`)
counts<-table(df$`Marital Status`)
colors=c("yellow","red","brown")
barplot(counts/nrow(df), main="Pareto chart of Marital Status",
        ylab="Relative Frequency",xlab="Marital Status",col=colors,ylim = c(0,0.6))

#Ascending Pareto chart ----
maritalstatus<-factor(maritalstatus,
                      levels = c("Married","Divorced","Single"))
df[5]<-maritalstatus
table(df$`Marital Status`)
counts<-table(df$`Marital Status`)
colors=c("brown","red","yellow")
barplot(counts/nrow(df), main="Pareto chart of Marital Status",
        ylab="Relative Frequency",xlab="Marital Status",col=colors,ylim = c(0,0.6))

#Discrete variable histogram ----
ggplot(df, aes(Number_of_kids))+geom_histogram(binwidth = 1)+
  scale_x_continuous("Number of kids", breaks = seq(0,10,by = 1))+
  scale_y_continuous("Count", breaks = seq(0,10,by = 1))+
  labs(title = "Histogram of Number of kids")

#Continuous variable histogram ----
hist(df$Income,main="Histogram of Income",xlab="Income",
     ylab="Number of observations",col="purple")

#Boxplot ----
boxplot(df$Income,main="Boxplot of Income")

  ##Mixed boxplot ----
maledf<-subset(df,df$Gender=="Male")
femaledf<-subset(df,df$Gender=="Female")
boxplot(maledf$Income,femaledf$Income,
        main="Boxplots for the Income of both Genders",
        names = c("Male Income","Female Income"),col=c("blue","red"))

#Scatterplot ----
plot(df$Income, df$Taxes, main="Scatterplot Income-Taxes",
     xlab="Income",ylab="Taxes",col=c("red"))

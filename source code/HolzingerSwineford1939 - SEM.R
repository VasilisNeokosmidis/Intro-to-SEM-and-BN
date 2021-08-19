#Libraries ----
library(lavaan)
library(semPlot)
library(semTools)
library(dplyr)

#Data Inspection ----
HolzingerSwineford1939
?HolzingerSwineford1939
HS <- HolzingerSwineford1939

  ##Selecting the variables of interest ----
  HS<-HS %>% dplyr::select('x1','x2','x3','x4','x5','x6','x7','x8','x9')

  ##Overview of the dataset ----
head(HS)
str(HS)

  ##Dimensions of the dataset ----
dim(HS)

#SEM ----
  ##Data Preparation ----
    ###Covariance Matrix ----

#Covariance matrix
HS_Cov<-cov(HS);HS_Cov
HS_Cov<-round(HS_Cov,digits=3);HS_Cov

  ##Factor Analysis ----
    ###Model Specification ----
visual_spec<-'
#Visual
visual =~ x1 + x2 + x3

#Textual
textual =~ x4 + x5 + x6

#Speed
speed =~ x7 + x8 + x9
'

    ###Model Identification ----
visual_estim<-sem(visual_spec,sample.cov=HS_Cov,sample.nobs=301);visual_estim

    ###Model Estimation ----
summary(visual_estim,standardized=TRUE,rsquare=TRUE,fit.measures=TRUE)

    ###Model Evaluation ----
#Reliability and validity of the factors

reliability(visual_estim)

#Convergent validity
#All the loadings are stat. signif. and AVE>0.7
AVE<-reliability(visual_estim)[5,];AVE

#Construct validity
fitmeasures(visual_estim,c('chisq','rmsea','gfi','agfi','rmr','nfi','tli','cfi','pgfi','pnfi'))

#Discriminant validity
#Through model's parameter

#Internal reliability
cron_alpha<-reliability(visual_estim)[1,];cron_alpha

#Construct reliability
#Computing Composite Reliability (CR)
SL<-standardizedSolution(visual_estim)
SL
SL <- SL$est.std[SL$op == "=~"]
SL

#Residual variances
RE<-1 - SL^2

#CR
CR<-sum(SL)^2 / (sum(SL)^2 + sum(RE))
CR

#AVE reliability
AVE

  ##Original SEM Model ----
    ###Model Specification ----
model_spec<-'
#Measurement model
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9

#Structural model
visual ~~ textual
visual ~~ speed
textual ~~ speed
'
    ###Model Identification ----
model_fit<-sem(model_spec,sample.cov=HS_Cov,sample.nobs=301);model_fit

    ###Model Estimation ----
summary(model_fit,standardized=TRUE,rsquare=TRUE,fit.measures=T)

#Matrix A of RAM
semMatrixAlgebra(model_fit, A)

#Matrix S of RAM
semMatrixAlgebra(model_fit, S)

#Matrix F of RAM
semMatrixAlgebra(model_fit, F)

    ###Model Visualization ----
model_visual<-semPaths(model_fit
                       ,what="paths"
                       ,whatLabels="std"
                       ,style="lisrel"
                       ,layout="spring"
                       ,rotation=1
                       ,residScale=8);model_visual

    ###Model Evaluation ----
model_eval<-fitmeasures(model_fit,c('chisq'
                                 ,'rmsea'
                                 ,'gfi'
                                 ,'agfi'
                                 ,'rmr'
                                 ,'nfi'
                                 ,'tli'
                                 ,'cfi'
                                 ,'pgfi'
                                 ,'pnfi'));model_eval

    ###Model Modification ----
model_mod<-modificationindices(model_fit,sort. = T);model_mod

#Isolating the modifications which have mi>4
subset(model_mod,mi>4)

#without x9 in visual
#chisq  rmsea    gfi   agfi    rmr    nfi    tli    cfi   pgfi   pnfi 
#85.252  0.092  0.943  0.894  0.082  0.907  0.896  0.931  0.503  0.605 

#with x9 in visual
#chisq  rmsea    gfi   agfi    rmr    nfi    tli    cfi   pgfi   pnfi 
#52.345  0.065  0.965  0.931  0.060  0.943  0.948  0.967  0.493  0.603 

  ##Modified SEM Model ----
    ###Model Specification ----
model_spec2<-'
#Measurement model
visual =~ x1 + x2 + x3 + x9
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9

#Structural model
visual ~~ textual
visual ~~ speed
textual ~~ speed
'
    ###Model Identification ----
model_fit2<-sem(model_spec2,sample.cov=HS_Cov,sample.nobs=301);model_fit2

    ###Model Estimation ----
summary(model_fit2,standardized=TRUE,rsquare=TRUE)

#Matrix A of RAM
semMatrixAlgebra(model_fit2, A)

#Matrix S of RAM
semMatrixAlgebra(model_fit2, S)

#Matrix F of RAM
semMatrixAlgebra(model_fit2, F)

    ###Model Visualization ----
model_visual2<-semPaths(model_fit2
                       ,what="paths"
                       ,whatLabels="std"
                       ,style="lisrel"
                       ,layout="spring"
                       ,rotation=2
                       ,residScale=8);model_visual2

    ###Model Evaluation ----
model_eval2<-fitmeasures(model_fit2,c('chisq'
                                    ,'rmsea'
                                    ,'gfi'
                                    ,'agfi'
                                    ,'rmr'
                                    ,'nfi'
                                    ,'tli'
                                    ,'cfi'
                                    ,'pgfi'
                                    ,'pnfi'));model_eval2

    ###Model Modification ----
model_mod2<-modificationindices(model_fit2,sort. = T);model_mod2

#Isolating the modifications which have mi>4
subset(model_mod2,mi>4)

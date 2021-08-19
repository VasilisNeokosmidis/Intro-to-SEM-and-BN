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

#About the Data ----
#Reading the dataset (.txt format)
travel_survey<-read.csv("travel_survey.txt",header=T,sep=" ")
travel.survey<-travel_survey

#Saving into csv (.csv format)
write.csv(travel_survey,"travel_survey.csv",row.names = F)

#Description:Age, Sex, Education, Occupation, Residence, and Travel of the individual regarding a train use survey
View(travel.survey)
str(travel.survey)

##Confirming that each variable belongs to the character class ----
class(travel.survey$A)
class(travel.survey$R)
class(travel.survey$E)
class(travel.survey$O)
class(travel.survey$S)
class(travel.survey$T)

#Data Manipulation ----
  ##Age ----
travel.survey$A<-factor(travel.survey$A,levels = c("young","adult","old"))
travel.survey$A
str(travel.survey$A)

  ##Residence ----
travel.survey$R<-factor(travel.survey$R,levels = c("small","big"))
travel.survey$R
str(travel.survey$R)

  ##Education ----
travel.survey$E<-factor(travel.survey$E,levels = c("high","uni"))
travel.survey$E
str(travel.survey$E)

  ##Occupation ----
travel.survey$O<-factor(travel.survey$O,levels = c("emp","self"))
travel.survey$O
str(travel.survey$O)

  ##Sex ----
travel.survey$S<-factor(travel.survey$S,levels = c("M","F"))
travel.survey$S
str(travel.survey$S)

  ##Travel ----
travel.survey$T<-factor(travel.survey$T,levels = c("car","train","other"))
travel.survey$T
str(travel.survey$T)

  ##Confirming the Manipulation of the variables ----
str(travel.survey)

#Data Inspection and Overview ----

  ##Dimension of the data ----
dim(travel.survey)

  ##Class of the dataset ----
class(travel.survey)

  ##Top Cases ----
head(travel.survey)

  ##Bottom Cases ----
tail(travel.survey)

  ##Total View of the dataset ----
View(travel.survey)

  ##Number of the observations ----
nrow(travel.survey) 
#or
length(travel.survey[,1])

  ##Names of the observations ----
rownames(travel.survey)

  ##Number of the variables ----
ncol(travel.survey) 
#or
length(travel.survey[1,])

  ##Names of the variables ----
colnames(travel.survey)

  ##Structure of the dataset ----
str(travel.survey)

  ##Summary Statistics of the dataset ----
summary(travel.survey)
glimpse(travel.survey)
describe(travel.survey)
Desc(travel.survey)

#Bayesian Networks ----
  ##Building the Directed Acyclic Graph (DAG) ----
    ###Empty Bayesian Network Graph ----
empty.net <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
empty.net
plot(empty.net)

    ###The Initiation of Adding Paths to the Empty Graph ----
DAG <- empty.net

    ###Arc from Age to Education ----
DAG <- set.arc(DAG, from = "A", to = "E")
plot(DAG)
DAG

    ###Arc from Sex to Education ----
DAG <- set.arc(DAG, from = "S", to = "E")
plot(DAG)
DAG

    ###Arc from Education to Occupation ----
DAG <- set.arc(DAG, from = "E", to = "O")
plot(DAG)
DAG

    ###Arc from Education to Residence ----
DAG <- set.arc(DAG, from = "E", to = "R")
plot(DAG)
DAG

    ###Arc from Occupation to Travel ----
DAG <- set.arc(DAG, from = "O", to = "T")
plot(DAG)
DAG

    ###Arc from Residence to Travel ----
DAG <- set.arc(DAG, from = "R", to = "T")
DAG

    ###Plot of the Final DAG ----
plot(DAG,main="The Directed Acyclic Graph (DAG)")

    ###Nodes of Final DAG ----
nodes(DAG)

    ###Arcs of Final DAG ----
arcs(DAG)

    ###Model String Representation of Final DAG ----
modelstring(DAG)

    ###Matrix Representation of the Final DAG ----
matrix.DAG <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
arc.set <- matrix(c("A", "E",
                    "S", "E",
                    "E", "O",
                    "E", "R",
                    "O", "T",
                    "R", "T"),
byrow = TRUE, ncol = 2,dimnames = list(NULL, c("from", "to")))
arcs(matrix.DAG) <- arc.set

#Checking if standard and matrix DAG are the same
all.equal(DAG, matrix.DAG)

  ##Joint Probability Distribution of the variables ----
    ###Levels of the variables ----
A.lv <- c("young", "adult", "old");A.lv
S.lv <- c("M", "F");S.lv
E.lv <- c("high", "uni");E.lv
O.lv <- c("emp", "self");O.lv
R.lv <- c("small", "big");R.lv
T.lv <- c("car", "train", "other");T.lv

    ###Frequency Table of the variables ----
tables.travel.survey<-Desc(travel.survey);tables.travel.survey

    ###Non-conditional Probabilities per variable per level ----
O.emp<-tables.travel.survey$O$freq$perc[1];O.emp
O.self<-tables.travel.survey$O$freq$perc[2];O.self
E.high<-tables.travel.survey$E$freq$perc[1];E.high
E.uni<-tables.travel.survey$E$freq$perc[2];E.uni
T.car<-tables.travel.survey$T$freq$perc[1];T.car 
T.train<-tables.travel.survey$T$freq$perc[2];T.train 
T.other<-tables.travel.survey$T$freq$perc[3];T.other
R.big<-tables.travel.survey$R$freq$perc[1];R.big
R.small<-tables.travel.survey$R$freq$perc[2];R.small

    ###Non-conditional Probability of Age ----
A.young<-tables.travel.survey$A$freq$perc[1];A.young
A.adult<-tables.travel.survey$A$freq$perc[2];A.adult
A.old<-tables.travel.survey$A$freq$perc[3];A.old

#Saving in an array
A.prob <- array(c(0.30, 0.50, 0.20)
                ,dim = 3
                ,dimnames = list(A = A.lv))
A.prob

    ###Non-conditional Probability of Sex ----
S.male<-tables.travel.survey$S$freq$perc[1];S.male
S.female<-tables.travel.survey$S$freq$perc[2];S.female

#Saving in an array
S.prob <- array(c(0.60, 0.40)
                , dim = 2
                ,dimnames = list(S = S.lv))
S.prob

    ###Conditional Probability of Occupation ----
#The probability of Education="uni"
P_E.uni<-tables.travel.survey$E$freq$perc[2]
P_E.uni

#The probability of Education="uni" and Occupation="emp"
E.uni_and_O.emp<-travel.survey %>% filter(O=="emp" & E=="uni")
P_E.uni_and_O.emp<-count(E.uni_and_O.emp)/nrow(travel.survey)
P_E.uni_and_O.emp

#The conditional probability of Occupation="emp" given Education="uni"
O.emp_given_E.uni<-P_E.uni_and_O.emp/P_E.uni
O.emp_given_E.uni

#The conditional probability of Occupation="self" given Education="uni"
O.self_given_E.uni<-1-O.emp_given_E.uni
O.self_given_E.uni

#Saving in an array
O.prob <- array(c(0.96, 0.04, 0.92, 0.08)
                ,dim = c(2, 2)
                ,dimnames = list(O = O.lv, E = E.lv))
O.prob

    ###Conditional Probability of Residence ----

#The probability of Education="uni"
P_E.uni

#The probability of Education="uni" and Residence="small"
E.uni_and_R.small<-travel.survey %>% filter(R=="small" & E=="uni")
P_E.uni_and_R.small<-count(E.uni_and_R.small)/nrow(travel.survey)
P_E.uni_and_R.small

#The conditional probability of Residence="small" given Education="uni"
R.small_given_E.uni<-P_E.uni_and_R.small/P_E.uni

#The conditional probability of Residence="big" given Education="uni"
R.big_given_E.uni<-1-R.small_given_E.uni
R.big_given_E.uni

R.prob <- array(c(0.25, 0.75, 0.20, 0.80)
                , dim = c(2, 2)
                ,dimnames = list(R = R.lv, E = E.lv))
R.prob

    ###Conditional Probability of Education ----
#The probability of Occupation="emp" and Residence="small"
R.small_and_O.emp<-travel.survey %>% filter(O=="emp" & R=="small")
P_R.small_and_O.emp<-count(R.small_and_O.emp)/nrow(travel.survey)
P_R.small_and_O.emp

#The probability of Occupation="emp", Residence="small" and Travel="car"
R.small_and_O.emp_and_T.car<-travel.survey %>% filter(O=="emp" & R=="small" & T=="car")
P_R.small_and_O.emp_and_T.car<-count(R.small_and_O.emp_and_T.car)/nrow(travel.survey)
P_R.small_and_O.emp_and_T.car

#The conditional probability of Travel="car" given Residence="small" and Occupation="emp"
T.car_given_R.small_and_O.emp<-P_R.small_and_O.emp_and_T.car/P_R.small_and_O.emp

E.prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64,0.36, 0.70, 0.30, 0.90, 0.10)
                  ,dim = c(2, 3, 2)
                  ,dimnames = list(E = E.lv, A = A.lv, S = S.lv))
E.prob

    ###Conditional Probability of Travel ----
T.prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58,
                    0.24, 0.18, 0.70, 0.21, 0.09)
                  ,dim = c(3, 2, 2)
                  ,dimnames = list(T = T.lv, O = O.lv, R = R.lv))
T.prob

  ##The Final Bayesian Network ----
    ###String Specification with model2network ----
string.Dag <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")

    ###The Local Distributions ----
lcl_distrs <- list(A = A.prob
                   , S = S.prob
                   , E = E.prob
                   , O = O.prob
                   , R = R.prob
                   , T = T.prob)

    ###Combination of DAG and lcl_distrs ----
Bayesian_network<-custom.fit(DAG, lcl_distrs)

    ###The Number of Parameters of the Final Bayesian Network ----
nparams(Bayesian_network)

    ###The Arcs of the Final Bayesian Network ----
arcs(Bayesian_network)

    ###Nodes of the Final Bayesian Network ----
nodes(Bayesian_network)

    ###Parents of the node Education ----
Bayesian_network$E$parents

    ###Children of the node Education ----
Bayesian_network$E$children

    ###The Parameters and Conditional Probability Table of the node Education ----
Bayesian_network$E

  ##Estimation of the Parameters ----
    ###Parameter Estimation of the BN with mle ----
bn.estimation.mle <- bn.fit(DAG, data = travel.survey, method = "mle")
bn.estimation.mle
str(travel.survey)
    ###Parameter Estimation of the BN with bayes ----
bn.estimation.bayes <- bn.fit(DAG, data = travel.survey, method = "bayes",iss=10)
bn.estimation.bayes

  ##Conditional Independence Tests ----
    ###The Pearson's X^2 Conditional Independence Test for Adding a Path ----
ci.test("T", "E", c("O", "R"), test = "x2", data = travel.survey)

    ###The Log-Likelihood Ratio G^2 Conditional Independence Test for Adding a Path ----
ci.test("T", "E", c("O", "R"), test = "mi", data = travel.survey)

    ###The Pearson's X^2 Conditional Independence Test for Removing a Path ----
ci.test("T", "O", "R", test = "x2", data = travel.survey)

    ###The Log-Likelihood Ratio G^2 Conditional Independence Test for Removing a Path ----
ci.test("T", "O", "R", test = "mi", data = travel.survey)

    ###The Strength of the Arcs of the Original DAG with Pearson's X^2 ----
arc.strength(DAG, data = travel.survey, criterion = "x2")

    ###The Strength of the Arcs of the Original DAG with Log-Likelihood Ratio G^2 ----
arc.strength(DAG, data = travel.survey, criterion = "mi")

  ##Network Scores ----
    ###The Bayesian Information Criterion Score ----
score(DAG, data = travel.survey, type = "bic")

    ###The Bayesian Dirichlet Equivalent Uniform Score ----
score(DAG, data = travel.survey, type = "bde", iss = 10)

  ##Comparing 2 models ----
    ###The Modified DAG ----
modified_DAG <- set.arc(DAG, from = "E", to = "T")

    ###The Number of Parameters ----
nparams(modified_DAG,travel.survey)

    ###The BIC Score of the Modified DAG ----
score(modified_DAG, data = travel.survey, type = "bic")

  ##The hill-climbing Algorithm ----
    ###The hill-climbing Algorithm using BIC ----
hill_climbing_learning_bic <- hc(travel.survey,score = "bic")
modelstring(hill_climbing_learning_bic)
plot(hill_climbing_learning_bic)

    ###The BIC Score of the DAG from hc ----
score(hill_climbing_learning_bic, data = travel.survey, type = "bic")

    ###The Arc Strength of the DAG from hc using BIC ----
arc.strength(hill_climbing_learning_bic, data = travel.survey, criterion = "bic")

    ###The hill-climbing Algorithm using BDeu ----
hill_climbing_learning_bde <- hc(travel.survey,score = "bde")
modelstring(hill_climbing_learning_bde)
plot(hill_climbing_learning_bde)

    ###The BDeu Score of the DAG from hc ----
score(hill_climbing_learning_bde, data = travel.survey, type = "bde")

    ###The Arc Strength of the DAG from hc using BDeu ----
arc.strength(hill_climbing_learning_bde, data = travel.survey, criterion = "bde")

  ##Inference and Queries ----
    ###Inference through DAG ----
#Is S and R d-separated?
dsep(DAG, x = "S", y = "R")

#Is O and R d-separated?
dsep(DAG, x = "O", y = "R")

#Is there a path from S to R?
path(DAG, from = "S", to = "R")

#Is S and R d-separated, while conditioning on E?
dsep(DAG, x = "S", y = "R", z = "E")

#Is O and R d-separated, while conditioning on E?
dsep(DAG, x = "O", y = "R", z = "E")

#Is A and S d-separated?
dsep(DAG, x = "A", y = "S")

#Is A and S d-separated, while conditioning on Z?
dsep(DAG, x = "A", y = "S", z = "E")    

    ###Inference through Conditional Probability Tables ----
#Construction of the junction tree
junction <- compile(as.grain(Bayesian_network))

#Let's observe the levels of each variable and form some queries
str(travel.survey)

#The behavior of all the individuals of the dataset towards their favorite mean of transport
querygrain(junction, nodes = "T")$T

#The behavior of men towards their favorite mean of transport
j_sex.male <- setEvidence(junction, nodes = "S", states = "M")
querygrain(j_sex.male, nodes = "T")$T

#The behavior of employed individuals towards their favorite mean of transport
emp_travel <- setEvidence(junction, nodes = "O", states = "emp")
querygrain(emp_travel, nodes = "T")$T

#The behavior of self-employed individuals towards their favorite mean of transport
self_travel <- setEvidence(junction, nodes = "O", states = "self")
querygrain(self_travel, nodes = "T")$T

#The relationship between Education and Travel given that Occupation=emp
E.T_given_O.emp <- setEvidence(junction, nodes = "O", states = "emp")
ExT.cpt <- querygrain(E.T_given_O.emp, nodes = c("E", "T"),type = "joint")
ExT.cpt

#The marginal distribution of Education and Travel
querygrain(E.T_given_O.emp, nodes = c("E", "T"), type = "marginal")

#The distribution of Education conditional on Travel
querygrain(E.T_given_O.emp, nodes = c("E", "T"), type = "conditional")

#The probability of Education=high and Travel=car given Occupation=emp
cpquery(Bayesian_network, event = (E == "high") & (T == "car"),evidence = (O == "emp"))

#The probability of Education=high and Travel=car given Occupation=emp (more accurate to the exact)
cpquery(Bayesian_network, event = (E == "high") & (T == "car"),evidence = (O == "emp"),n=10^6)

#The probability of Education=high and Travel=car given Occupation=emp with likelihood weighting (even more accurate to the exact)
cpquery(Bayesian_network, event = (E == "high") & (T == "car"),evidence = list(O = "emp"), method = "lw")

#The probability of Travel=car and Sex=M given Age=young with likelihood weighting
cpquery(Bayesian_network, event = (T == "car") & (S=="M"),evidence = list(A = "young"), method = "lw")

#The probability of Travel=car and Sex=F given Age=young with likelihood weighting
cpquery(Bayesian_network, event = (T == "car") & (S=="F"),evidence = list(A = "young"), method = "lw")

#The probability of Sex=F and Travel=car
cpquery(Bayesian_network, event = (S == "F") & (T == "car"),evidence = ((A == "young") & (E == "uni")) | (A == "adult"))

#The random observations for Education and Travel that match O=emp
ExT <- cpdist(Bayesian_network, nodes = c("E", "T"),evidence = (O == "emp"))
head(ExT)

#The contingency table of probabilities of Education and Travel given Occupation=emp 
prop.table(table(ExT))

  ##Graphical Representation of the Bayesian Network ----
    ###Fancy DAG plot ----
#DAG with dot layout
graphviz.plot(DAG,layout='dot')

#Changing the colors of all the arcs and nodes to grey
hlight <- list(nodes = nodes(DAG), arcs = arcs(DAG), col = "grey", textCol = "grey")

#Using hlight as input in graphviz.plot
grey_DAG<-graphviz.plot(DAG,layout='dot',highlight = hlight)

#Customizing the color and width of arcs of the path A->E->O->T of the DAG
edgeRenderInfo(grey_DAG) <-list(col = c("A~E" = "black", "E~O" = "black","O~T" = "black")
                          ,lwd = c("A~E" = 3, "E~O" = 3,"O~T" = 3))

#Customizing the color of nodes A,E,O,T of the DAG
nodeRenderInfo(grey_DAG) <-list(col = c("A" = "black", "E" = "black", "O" = "black", "T" = "black")
                                ,textCol = c("A" = "black", "E" = "black", "O" = "black", "T" = "black"))

#Plotting the customized DAG
renderGraph(grey_DAG)

    ###Fancy Conditional Probability Distributions ----
#The bar chart of the conditional probability of Travel given Residence and Occupation
bn.fit.barchart(bn.estimation.mle$T, main = "Travel",xlab = "Pr(T | R,O)", ylab = "")

#The dot plot of the conditional probability of Travel given Residence and Occupation
bn.fit.dotplot(bn.estimation.mle$T, main = "Travel",xlab = "Pr(T | R,O)", ylab = "")

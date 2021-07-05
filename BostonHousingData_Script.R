# Install
install.packages("FactoMineR")
# Load
library("FactoMineR")

1.
# Loading the raw Data
BostonHousingData=read.csv('C:/Users/harsha/Desktop/Stats+R/CSV Files/BostonHousingData.csv',
                           na.strings=c(""," ","NA","NULL"), stringsAsFactors = T)

BostonHousingData
View(BostonHousingData)

2.
# number of observations and variable
dim(BostonHousingData)
summary(BostonHousingData)


3.
# Exploring the data set
str(BostonHousingData)

4.
# convert the categorical variables to factors if they are

factor_cols=c("RAD","CHAS")

for (cat_cols in factor_cols) {
  BostonHousingData[ , cat_cols]=as.factor(BostonHousingData[ ,cat_cols])
}

class(BostonHousingData)
str(BostonHousingData)

5. 
#TargetVariable is in continuous nature hence - Regression Model

6.
# Checking and treating missing values

colSums(is.na(BostonHousingData))

#No Missing Values hence process Further

#####################################################################

7. 
#detection of outliers and treatment of extreme outliers 

# CRIM

boxplot(BostonHousingData$CRIM, horizontal = T)

max(BostonHousingData$CRIM)

quantiles=quantile(BostonHousingData$CRIM, c(0.98, 0.983))

quantiles

quantiles_final=quantile(BostonHousingData$CRIM, 0.983)

quantiles_final

BostonHousingData$CRIM=ifelse(BostonHousingData$CRIM>quantiles_final,quantiles_final, BostonHousingData$CRIM)

boxplot(BostonHousingData$CRIM, horizontal = T)
max(BostonHousingData$CRIM)

#ZN

boxplot(BostonHousingData$ZN, horizontal = T)

max(BostonHousingData$ZN)

quantiles=quantile(BostonHousingData$ZN, c(0.88, 0.861))

quantiles

quantiles_final=quantile(BostonHousingData$ZN, 0.861)

quantiles_final

BostonHousingData$ZN=ifelse(BostonHousingData$ZN>quantiles_final,quantiles_final, BostonHousingData$ZN)

boxplot(BostonHousingData$ZN, horizontal = T)
max(BostonHousingData$ZN)

# RM
boxplot(BostonHousingData$RM, horizontal = T)
max(BostonHousingData$RM)

quantiles=quantile(BostonHousingData$RM, c(0.975, 0.99))

quantiles

quantiles_final=quantile(BostonHousingData$RM, 0.99)

quantiles_final

BostonHousingData$RM=ifelse(BostonHousingData$RM>quantiles_final,quantiles_final, BostonHousingData$RM)

boxplot(BostonHousingData$RM, horizontal = T)
max(BostonHousingData$RM)


# DIS 

boxplot(BostonHousingData$DIS, horizontal = T)
max(BostonHousingData$DIS)

quantiles=quantile(BostonHousingData$DIS, c(0.99, 0.991))

quantiles

quantiles_final=quantile(BostonHousingData$DIS, 0.991)

quantiles_final

BostonHousingData$DIS=ifelse(BostonHousingData$DIS>quantiles_final,quantiles_final, BostonHousingData$DIS)

boxplot(BostonHousingData$DIS, horizontal = T)
max(BostonHousingData$DIS)


# LSTAT

boxplot(BostonHousingData$LSTAT, horizontal = T)
max(BostonHousingData$LSTAT)

quantiles=quantile(BostonHousingData$LSTAT, c(0.98, 0.989))

quantiles

quantiles_final=quantile(BostonHousingData$LSTAT, 0.989)

quantiles_final

BostonHousingData$LSTAT=ifelse(BostonHousingData$LSTAT>quantiles_final,quantiles_final, BostonHousingData$LSTAT)

boxplot(BostonHousingData$LSTAT, horizontal = T)
max(BostonHousingData$LSTAT)

7.
# Explore each "Potential" predictor for distribution and Quality
#Univariate Analysis

#Exploring Single Continuous variable

hist(BostonHousingData$MEDV)
summary(BostonHousingData$MEDV)

#Exploring Multiple Continuous Variable

ColsForHist=c("MEDV","CRIM","ZN","INDUS","NOX","RM","AGE","DIS","TAX","PTRATIO","B",
              "LSTAT")

ColsForHist
par(mfrow=c(3,4))

for (hist_cols in ColsForHist) {
  hist(BostonHousingData[ ,c(hist_cols)], main = paste('Histogram of:',hist_cols),
       col = brewer.pal(8,"Paired"))
  
}

#Exploring single categorical column

par(mfrow=c(1,1))
table(BostonHousingData$CHAS)
barplot(table(BostonHousingData$CHAS))

#Exploring Multiple categorical column

ColsForBar=c("CHAS","RAD")
ColsForBar
par(mfrow=c(1,2))
for (bar_cols in ColsForBar) {
  barplot(table(BostonHousingData[ ,c(bar_cols)]), main = paste('Barplot of:',bar_cols),
                col = brewer.pal(4,"Paired"))
}

##############################################################################
8.
#Bivariate Analysis
## Visual Relationship between predictors and target variable

#Continuous Vs Continuous - Scatter Plot

par(mfrow=c(1,1))
plot(x=BostonHousingData$AGE, y=BostonHousingData$MEDV, col = "Blue")

#For Multiple columns 

ContinuousCols=c("MEDV","CRIM","ZN","INDUS","NOX","RM","AGE","DIS","TAX","PTRATIO","B",
                 "LSTAT")
ContinuousCols

plot(BostonHousingData[ ,ContinuousCols], col = "Blue")

#Continuous Vs categorical -Box plot

Categorical_cols=c("CHAS","RAD")
par(mfrow=c(1,2))
for (bar_cols in Categorical_cols) {
  boxplot(MEDV~(BostonHousingData[ ,c(bar_cols)]), data = BostonHousingData,
          main = paste('Box Plot of:',bar_cols), col = brewer.pal(8,"Paired"))
}

#########################################################################
9.
#strength of relationship between predictors and target variables 

#continuous Vs Continuous -- Correlation Analysis

cor(BostonHousingData[ ,c('MEDV','AGE')], use = "complete.obs")

# Negative correlation between MEDV and Age as r is negative and close to 0

ContinuousCols=c("MEDV","CRIM","ZN","INDUS","NOX","RM","AGE","DIS","TAX","PTRATIO","B",
                 "LSTAT")

CorrData=cor(BostonHousingData[ ,ContinuousCols], use = "complete.obs")
CorrData

#Final continuous columns to be selected for modeling

names(CorrData['MEDV',][abs(CorrData['MEDV',])>0.4])

#CRIM, INDUS,NOX,TAX, RM,PTRATIO,LSTAT -are selected for model


#Continuous Vs Categorical Correlation strength -- ANOVA

#Null Hypothesis --the variable(MEDV, CHAS) are not correlated 

summary(aov(MEDV ~ BostonHousingData$CHAS, data = BostonHousingData))
str(BostonHousingData)

#so BostonHousingData, there are total 506 observation 
#so Degree of Freedom is :506-1=505
#residual 1 for CHAS hence 504 values distributed across residuals

#probability is <0.5 hence we reject the Null hypotheis and say MEDV and CHAS
#are correlated 

#Loop for ANOVA test

colsforAnova=c("CHAS","RAD")
for (Aovcols in colsforAnova) {
  Anovaresult=summary(aov(BostonHousingData$MEDV ~ BostonHousingData[ ,c(Aovcols)]))
  print(Aovcols)
  print(Anovaresult)
}

# shows CHAS and RAD are highly correlated 

#################################################################################
10.
#Generating the Data frame for Machine Learning

InputData=BostonHousingData
TargetVariableName='MEDV'
TargetVariableName

BestPredictorName=c("CRIM", "INDUS", "NOX","RM","TAX","PTRATIO","LSTAT")
BestPredictorName

TargetVariable=InputData[ ,c(TargetVariableName)]
TargetVariable
str(TargetVariable)

PredictorVariable=InputData[ ,BestPredictorName]
PredictorVariable
str(PredictorVariable)

#creating the final data to be used   for ML

DataForML=data.frame(TargetVariable,PredictorVariable)
str(DataForML)
head(DataForML)

###############################################################################
11.
#Sampling |Splitting data into 70% for training and 30% for testing

set.seed(123)
TrainingSampleIndex=sample(1:nrow(DataForML), size = 0.7*nrow(DataForML))
length(TrainingSampleIndex)

DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTrain

DataForMLTest=DataForML[ -TrainingSampleIndex, ]
DataForMLTest

dim(DataForMLTest)
dim(DataForMLTrain)

###########################################################################
12.
## Creating Predictive models on training data to check the accuracy of each algorithm
##Linear Regression ####

#Simple Linear Regression 

Model_Reg=lm(TargetVariable ~ AGE , data = DataForMLTrain)
Model_Reg

plot(DataForMLTrain$AGE, DataForMLTrain$TargetVariable, Xlab = 'AGE', ylab = 'MEDV', col = 'Green')

abline(Model_Reg, col = "blue")

#Plot shows negative relation between MEDV and age , as age goes high MEDV reduce.

#####Multiple Linear Regression ###############

Model_Reg=lm(TargetVariable ~., data = DataForMLTrain)

summary(Model_Reg)


Model_Reg_2=lm(TargetVariable ~.-CRIM, data = DataForMLTrain)
summary(Model_Reg_2)


Model_Reg_3=lm(TargetVariable ~ INDUS+RM+TAX+
                 PTRATIO+LSTAT, data = DataForMLTrain)

summary(Model_Reg_3)


Model_Reg_4=lm(TargetVariable ~ INDUS+RM+
               PTRATIO+LSTAT, data = DataForMLTrain)
summary(Model_Reg_4)

#removed(CRIM)
Model_Reg_5=lm(TargetVariable ~ RM+ 
                 PTRATIO+LSTAT, data = DataForMLTrain)
summary(Model_Reg_5)

#Multiple R-Squared o.6643 & Adjusted R-Sqaured --0.6614

#############################################################################
13.
#Checking Accuracy of Model on Testing Data

head(DataForMLTest)
DataForMLTest$Pred_LM=predict(Model_Reg_5, DataForMLTest)
head(DataForMLTest)

###########################################################################
14.
#Calculating absolute percentage error for each prediction

DataForMLTest$LM_APE=100*(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_LM)/
                            DataForMLTest$TargetVariable)

head(DataForMLTest)

MeanAPE=mean(DataForMLTest$LM_APE)
MedianAPE=median(DataForMLTest$LM_APE)
print(paste('## Mean Acuracy of Linear Model is:', 100 - MeanAPE))
print(paste('## Median Accuracy of Linear Model is:', 100 - MedianAPE))

#MeanAPE=15.88
#MedianAPE = 12.36
#Mean Accuracy of Linear Model - 82.72%
#Median Accuracy of Linear Model - 87.194%



##############################################################################
############Decision Tree #################

library(party)

Model_CTREE=ctree(TargetVariable~. , data = DataForMLTrain)
Model_CTREE

plot(Model_CTREE)

###########################################################################
#checking Accuracy of model  on Testing Data

DataForMLTest$Pred_CTREE=as.numeric(predict(Model_CTREE, DataForMLTest))
head(DataForMLTest)

DataForMLTest$CTREE_APE = 100*(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_CTREE)/
                                 DataForMLTest$TargetVariable)
head(DataForMLTest)

print(paste('## Mean Accuracy of Decision Tree Model is:', 100 - mean(DataForMLTest$CTREE_APE)))
print(paste('## Median Accuracy of Decision Tree Model is:', 100 - median(DataForMLTest$CTREE_APE)))

# Mean Accuracy of Decision Tree - 85.33%
#Median Accuracy of Decision Tree - 90.30%

#As decision Tree somewhat high accuracy compare to Linear Model hence, we can go for Decision 
# to get accurate model.

################ PCA (Principle component analysis)######################################
# will perform PCA analysis on only on continuous variables to form correlation coefficient.

library(MASS)

DataForPCA =BostonHousingData


# considering Continuous columns only

DataForPCA[ ,c("CHAS","RAD")] = NULL

str(DataForPCA)

# Checking for Missing Value
colSums(is.na(DataForPCA))

# Separate the Target and Predictor  variables 

TargetVariable = DataForPCA[ , C(TargetvariableName)]

names(DataForPCA)

!names(DataForPCA) %in% TargetVariableName

# running the whole command i will get all the prdictor columns

PredictorVariables = DataForPCA[ , !names(DataForPCA) %in% TargetVariableName]

str(PredictorVariables)
head(PredictorVariables)


# creating Unsupervised PCA Model on this above data

PredictorPCA = prcomp(PredictorVariables, scale = T)

summary(scale(PredictorVariables))

# taking the summary of PCA

PCAsummary = data.frame(summary(PredictorPCA)$importance)
PCAsummary

# To extraxt how many principal components are there 

Principalcomponents = 1:ncol(PCAsummary)

#2nd Row : Data Frame of Individual Variances
# 3rd Row : Data Frame of Culumlative Variance

IndividualVarianceExplained = PCAsummary[2,]*100
CumulativeVarianceExplained = PCAsummary[3,]*100

# to check on plot

plot(Principalcomponents, IndividualVarianceExplained, main = "Individual Variance")

lines(Principalcomponents, IndividualVarianceExplained)

# cumulative

plot(Principalcomponents, CumulativeVarianceExplained, main = "Cumulative Variance")

lines(Principalcomponents, CumulativeVarianceExplained)

# generating all 11 components

PrincipalcomponentsValues = predict(PredictorPCA)

head(PrincipalcomponentsValues)

# we are creating dataframe with 7 components 

PrincipalcomponentsValuesTop = PrincipalcomponentsValues[ ,c(1:7)]
??some.function

cor(PrincipalcomponentsValuesTop)

# adding targetvarible to best pricipal components which are new predictors.

DataForML = data.frame(TargetVariable, PrincipalcomponentsValuesTop)

# will add categorical variables, which we have deleted

DataForML$CHAS = BostonHousingData$CHAS
DataForML$RAD  = BostonHousingData$RAD

head(DataForML)
str(DataForML)


#Sampling |Splitting data into 70% for training and 30% for testing

set.seed(123)
TrainingPCA=sample(1:nrow(DataForML), size = 0.7*nrow(DataForML))
length(TrainingPCA)

DataForMLTrain=DataForML[TrainingPCA, ]
DataForMLTrain

DataForMLTest=DataForML[ -TrainingPCA, ]
DataForMLTest

dim(DataForMLTest)
dim(DataForMLTrain)

###########################################################################
12.
## Creating Predictive models on training data to check the accuracy of each algorithm
##Linear Regression ####

install.packages("pls")
# Load
library("pls")

pcr_model = plsr(TargetVariable~., data = DataForMLTrain)

summary(pcr_model)

#############################################################################
13.
#Checking Accuracy of Model on Testing Data

head(DataForMLTest)
DataForMLTest$Pred = predict(pcr_model, DataForMLTest)
head(DataForMLTest)


validationplot(pcr_model,val.type ="RMSEP")
pls.RMSEP<-RMSEP(pcr_model)

plot(pls.RMSEP,main="RMSEP PLS",xlab="Components")

min = which.min(pls.RMSEP$val)
points(min,min(pls.RMSEP$val),pch=1,col="red")
plot(pcr_model, ncomp=9, asp=1, line= TRUE)


# use 7 components 


DataForMLTest$Pred2 = predict(pcr_model,DataForMLTest,ncomp=7)

pls.eval = data.frame(obs=DataForMLTest$TargetVariable ,pred=DataForMLTest$Pred2[,1,1])

defaultSummary(pls.eval)

RMSE     : 4.8692009
Rsquared : 0.7361674
MAE      : 3.5110436

min_max_accuracy <- mean(apply(pls.eval, 1, min) / apply(pls.eval, 1, max))

min_max_accuracy  : 85.93%


###########################################################################

# we can see after PCA the accuracy increase by 4% and R- Squared value has increased by 0.07 value.
# so pCA gives good accuracy compared to Linear Regression 



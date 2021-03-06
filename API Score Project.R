
##Predicting Academic Performance Index (API) Score 
#from California Department of Eduaction 
#through Linear Regression


library(car) #cross validation
library(ggplot2) #ggplot
library(lattice)
library(MASS)
library(dplyr)
library(caret) #linear
library(leaps) #regsubsets
library(glmnet)
library(olsrr)
library(e1071)
library(sandwich) 
library(tidyr)
library(purrr) #keep
library(ModelMetrics) #mse
library(corrplot)

set.seed(1234)

Base05<- read.csv("/Users/DavidKwon/Desktop/Practice/project1/API 2005 Base Data.csv")


#"The 2005 API (Academic Performance Index) summarizes a school's, an LEA's (local educational agency, is a school district or county office of education), or the state's performance on the spring 2005 Standardized Testing and Reporting (STAR) Program and 2005 California High School Exit Examination (CAHSEE)."
#"The 2005 API Base summarizes a subgroup's (e.g STAR Program scores, Ethnic/racial subgroup/ parents' degrees/ English Learners ...) performance on the spring 2005 STAR Program and 2005 CAHSEE. It serves as the baseline score, or starting point, of performance of that subgroup"

#Therefore, we are going to remove those variables that are created by our dependent variable such as SIM_RANK, ST_RANK
#we are also going to remove one of the variable, RTYPE or STYPE, since they are overlapping

Base05<-select(Base05,c(-"RTYPE", -"SIM_RANK",-"ST_RANK"))

#find NAs rows of the dependent variables and removes the rows that includes NAs

Base05<-Base05[-which(is.na(Base05$API05B)),] 


#creating function that finds NAs for each variables 

findingNA<-function(x){
  length(which(is.na(x)))/length(x)
}


for(i in 1:dim(Base05)[2]){
  if(findingNA(Base05[,i])>0.1){
    print(i)
  }
}

#we are going to remove the variables that have 10% of the elements is NAs 
#I also removed the "X" variable (school or district code), which is 1

newbase<-Base05[,c(-1, -3, -4, -5, -42, -46, -47, -48, -66)]

new.base<-newbase[complete.cases(newbase),]



#lets explore our dependent variable with ggplot

###histogram
ggplot(data=new.base, aes(new.base$API05B)) + 
  geom_histogram(aes(y=..density..), binwidth=10) +
  geom_density(aes(y=..density..), color="red")+
  labs(title="Histogram for Academic Performance Index 2005 Base", x="API 2005 Base", y="Frequency") +
  geom_vline(xintercept = mean(new.base$API05B), show.legend = TRUE, color="red") +
  geom_vline(xintercept = median(new.base$API05B), show.legend = TRUE, color="blue")

#As the graph shows, we can notice the API05B seems to be normally distributed but it's skewed a little. 

#Our independent factor variables

new.base[1:97] %>% keep(is.factor) %>% gather() %>%
  ggplot(aes(value))+
  facet_wrap(~key,scales="free")+
  geom_bar()





#Our independent numerical variable 
new.base[1:20] %>% keep(is.numeric) %>% gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales="free")+
  geom_histogram(bins=30)+
  geom_density()

new.base[21:40] %>% keep(is.numeric) %>% gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales="free")+
  geom_histogram(bins=30)+
  geom_density()

new.base[41:60] %>% keep(is.numeric) %>% gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales="free")+
  geom_histogram(bins=30)+
  geom_density()

new.base[61:80] %>% keep(is.numeric) %>% gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales="free")+
  geom_histogram(bins=30)+
  geom_density()

new.base[81:96] %>% keep(is.numeric) %>% gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales="free")+
  geom_histogram(bins=30)+
  geom_density()




#most of our independent numerical variables are highly skewed
#we might want to perform log tranformation, but I will do later. 



##########splitting training and test datset - cross validation

split<-createDataPartition(y=new.base$API05B,p=0.7,list=FALSE)

training.newbase <- new.base[split,]
test.newbase <- new.base[-split,]

#The first model with whole data
lm<-lm(API05B~., data=training.newbase)
summary(lm)

plot(lm)

#There's a lot of NAs of Beta, which implies that it's not estimatable. 
#We are going to perform variable selection using regsubsets-backward

###regsubsets - model selection - backward - nvmax=10

###Model Selection through stepwise backward selection from regsubsets
#I let the maximum number of predictors be 10, since it's going to be too complex to interpret or to build a model if we have more than 10 predictors

reg1<-regsubsets(API05B~., data=training.newbase, really.big=TRUE,nvmax=10, method = "backward")

reg.summary<-summary(reg1)

reg.summary

#we are going to see the changes of R^2, Adjusted R^2 (how well the model explained), Residuals Sum of Sqaured (Residuals), Cp (size of the bias), BIC 

#R Squared
plot(reg.summary$rsq, xlab="number of variables", ylab="Rsquared", type="l")
max(reg.summary$rsq)
which.max(reg.summary$rsq)
points(3, reg.summary$rsq[3], col="red",cex=1,pch=20)
points(4, reg.summary$rsq[4], col="red",cex=1,pch=20)

#As the graph shows, we can notice a significant change of R^2 at 3 and 4 of number of variables
#After that, we see the graph is slightly increasing 
#I assume that those points are the critical points.  


#Residuals Sum of Squared
plot(reg.summary$rss, xlab="number of variables", ylab="RSS", type="l")
min(reg.summary$rss)
which.min(reg.summary$rss)
points(3, reg.summary$rss[3], col="red",cex=1,pch=20)
points(4, reg.summary$rss[4], col="red",cex=1,pch=20)

#Adjusted R Squared
plot(reg.summary$adjr2, xlab="number of variables", ylab="Adjusted R^2", type="l")
max(reg.summary$adjr2)
which.max(reg.summary$adjr2)
points(3,reg.summary$adjr2[3],col="red",cex=1,pch=20)
points(4,reg.summary$adjr2[4],col="red",cex=1,pch=20)

#CP
#From Penn State Univ Stat online Course..
#"An underspecified model is a model in which important predictors are missing. And, an underspecified model yields biased regression coefficients and biased predictions of the response. Well, in short, Mallows' CP statistic estimates the size of the bias that is introduced into the predicted responses by having an underspecified model."
#In short, CP implies the size of the bias

plot(reg.summary$cp, xlab="number of variables", ylab="Cp", type="l")
min(reg.summary$cp)
which.min(reg.summary$cp)
points(3, reg.summary$cp[3],col="red",cex=1,pch=20)
points(4, reg.summary$cp[4],col="red",cex=1,pch=20)

#BIC
plot(reg.summary$bic, xlab="number of variables", ylab="BIC", type="l")
min(reg.summary$bic)
which.min(reg.summary$bic)
points(3, reg.summary$bic[3],col="red",cex=1,pch=20)
points(4, reg.summary$bic[4],col="red",cex=1,pch=20)

#We would try to choose 4 number of predictors for our model first. 

a<-names(coef(reg1,which(reg.summary$bic==reg.summary$bic[4])))

reg.summary$rsq[4]
reg.summary$adjr2[4]


training.newbase1<-select(training.newbase,c("API05B",a[2:5]))
rownames(training.newbase1)<-1:nrow(training.newbase1)

#plots between response and predictors
plot(training.newbase1)

#GRAD_SCH with others shows some curves, it looks like log graph
#VCST_M911 seems to be too skewed

cor<-cor(training.newbase1)
corrplot(cor, 
         method=c("circle"),
         title="Correlation between variables",
         type=c("full"))
#too less correlation between VCST_M911 and any others. 

training.newbase1 %>% keep(is.numeric) %>% gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales="free")+
  geom_histogram(bins=30)+
  geom_density()

#we might want to exclude the VCST_M911, but let's see how the model would be

lm1<-lm(API05B~., data=training.newbase1)
summary(lm1)
par(mfrow=c(2,2))
plot(lm1)
#adj R^2 68%, and there looks some patterns and some outliers 

a<-predict(lm1,newdata=test.newbase)
b<-test.newbase$API05B

par(mfrow=c(1,1))
plot(a,b)
#predicted values vs test values of API05B
#looks like linear, and some outliers

predictions1 <- lm1 %>% predict(test.newbase)
# Model performance
data.frame(
  MSE = mse(test.newbase$API05B, predictions1),
  RMSE = RMSE(predictions1, test.newbase$API05B),
  R2 = R2(predictions1, test.newbase$API05B)
)

#acceptable MSE, 68% of R^2 (68% of the variance explained)



#########Outliers - Cooks Distance

#finding outliers in our model

#There are several ways to see outliers in our model
outlierTest(lm1,n.max=50,cutoff=0.05)

qqPlot(lm1,main="QQ Plot")

avPlots(lm1)

influencePlot(lm1,id.method="identify",main="influential plot",sub="circle size is proportial to cook's distance")

#However,
#I'm using Standardized Residuals to detect outliers

#from Springers Text book "Linear Regression"

#"In summary, an outlier is a point whose standardized residual falls outside the interval from -2 to 2. Recall that a bad leverage point is a leverage point which is also an outlier. Thus, a bad leverage point is a leverage point whose standardized residual falls outside the interval from -2 to 2."

o1<-which(rstandard(lm1, infl = lm.influence(lm1, do.coef = FALSE),
                    sd=sqrt(deviance(lm1)/df.residual(lm1)),
                    type=c("sd.1","predictive"))>4)

o2<-which(rstandard(lm1, infl = lm.influence(lm1, do.coef = FALSE),
                    sd=sqrt(deviance(lm1)/df.residual(lm1)),
                    type=c("sd.1","predictive"))<(-4))


outliers <- c(o1,o2)
length(outliers)

#####The data set after removing outliers
training.newbase2<-training.newbase1[-outliers,]

#investigating linear models between response and each predictors
lm.test1<-lm(API05B~MEALS, data=training.newbase2)
summary(lm.test1)
par(mfrow=c(2,2))
plot(lm.test1)
#adj R^2 29.5%, plots looks ok except for normal Q-Q plot

lm.test2<-lm(API05B~SMOB, data=training.newbase2)
summary(lm.test2)
plot(lm.test2)
#adj R^2 40%, seems heteroscedasticity

lm.test3<-lm(API05B~GRAD_SCH, data=training.newbase2)
summary(lm.test3)
plot(lm.test3)
#R^2 39%, plots shows extreme heteroscedasticity

lm.test4<-lm(API05B~VCST_M911, data=training.newbase2)
summary(lm.test4)
plot(lm.test4)
#R^2 0.1%, plots shows extreme patterns showing heteroscedasticity

#####Model after removing outliers
lm2<-lm(API05B~., data=training.newbase2)
summary(lm2)
plot(lm2)

#adj R^2 71%, plots looks ok except for showing some pattern of the heteroscedasticity. 

#I may decide to drop the VCST_M911

#R^2 getting higher, plots for model such as 'fitted values vs residuals', 'Normal Q-Q', or 'sqrt of Standardized residuals vs fitted values' becomes much better after removing outliers

training.newbase3<-select(training.newbase2, -c("VCST_M911"))

lm3<-lm(API05B~., data=training.newbase3)
summary(lm3)
plot(lm3)

#still showing some patterns, I will perform diagnosis of normality of residuals, multicollinearity, and heteroscedasticity from now. 

a1<-predict(lm3,newdata = test.newbase)

predictions2 <- lm3 %>% predict(test.newbase)
# Model performance
data.frame(
  MSE = mse(test.newbase$API05B, predictions2),
  RMSE = RMSE(predictions2, test.newbase$API05B),
  R2 = R2(predictions2, test.newbase$API05B)
)


#########Normality (of residuals)

#We don't have to worry about the assumption of normality of residuals (the error between the dependent variable and the independent variables) because when the sample size is sufficiently large, the Central Limit Theorem ensures that the distribution of residiual will be approximately normality. 

par(mfrow=c(1,1))
qplot(residuals(lm3),
      geom="histogram",
      binwidth= 10,
      main="Histogram of Residuals of our model",
      xlab="Residuals of our model",
      ylab="Frequency")

#Residuals on our model are approximately normal distributed. 






##########Multicolliniearity

# From STHDA website.. 

#definition

##In multiple regression , two or more predictor variables might be correlated with each other. This situation is referred as collinearity.

#There is an extreme situation, called multicollinearity, where collinearity exists between three or more variables even if no pair of variables has a particularly high correlation. This means that there is redundancy between predictor variables.

vif(lm3)

#More than 4 VIF score must be removed, but we dont have any.






##########heteroscedasciticity (non-constant residuals)
par(mfrow=c(2,2))
plot(lm3)

#As we see the first plot of the model (fitted value vs residuals), 
#it seems like there's heteroscedasticity exists, so I'm going to perform transformation. 


#box-cox

par(mfrow=c(1,1))
bc<-boxcox(lm3)

lambda<-bc$x[which.max(bc$y)]
lambda

#boxcox + log transformation
lm4<-lm((API05B)^(lambda)~log(MEALS+1)+log(SMOB+1)+log(GRAD_SCH+1), data=training.newbase3)
summary(lm4)
par(mfrow=c(2,2))
plot(lm4)

#It looks improved. 


a2<-sqrt(predict(lm4,newdata = test.newbase))



#Model Progress as graph
par(mfrow=c(1,1))
plot(a,b,
     main="First model prediction",
     xlab="predicted values",
     ylab="values in test dataset")
abline(a=0,b=1,col="red")
plot(a1,b,
     main="Removed outliers",
     xlab="predicted values",
     ylab="values in test dataset")
abline(a=0,b=1,col="red")
plot(a2,b,
     main="After transformation - Final model",
     xlab="predicted values",
     ylab="values in test dataset")
abline(a=0,b=1,col="red")


predictions3 <- lm4 %>% predict(test.newbase)

# Model performance
data.frame(
  MSE = mse(test.newbase$API05B, predictions3),
  RMSE = RMSE(predictions3, test.newbase$API05B),
  R2 = R2(predictions3, test.newbase$API05B)
)

#model performance changing process

c.m<-data.frame(MSE=
                  c(mse(test.newbase$API05B, predictions1),
                    mse(test.newbase$API05B, predictions2),
                    mse(test.newbase$API05B, predictions3)
                  ),
                RMSE=
                  c(RMSE(predictions1,test.newbase$API05B),
                    RMSE(predictions2,test.newbase$API05B),
                    RMSE(predictions3,test.newbase$API05B)
                  ),
                R2=
                  c(R2(predictions1,test.newbase$API05B),
                    R2(predictions2,test.newbase$API05B),
                    R2(predictions3,test.newbase$API05B)
                  ))


rownames(c.m)<-c("First model","Removed Outliers","After transformation-final model")

c.m

summary(lm4)

#Conclusion

#I conclude that we are able to predict the API score for the future with the 3 of predictors, which are described at below, as 65% variance explained. 

#As interpretation of the Betas (each coefficients), 
#For example, 
#for a 10% increase in GRAD_SCH, Beta3 * log(1.1+1) = 49966 * log(1.1+1) = 37071.64.
#API score is transformed with lambda, which is power to the 2, from box-cox transformation, 

#Therefore, sqrt(Beta3 * log(1.1+1)) = sqrt(49966 * log(1.1+1)) = 192.54 increase in API05B score
#In other words, 1 percent change in GRAD_SCH is associated with Beta3 * log(201/100) change in (API05B)^(lambda), where lambda is 2


#For a 1% increase in SMOB, abs(Beta1) * log(2.01) = 36637*log(2.01) = 25577.56 
#Therefore, sqrt(abs(Beta1)*log(2.01)) = sqrt(36637 * log(2.01)) = 159.92 decrease in API05B score
#In other words, 1 percent change in SMOB is associated with abs(Beta1) * log(201/100) change in (API05B)^(lambda), where lambda is 2


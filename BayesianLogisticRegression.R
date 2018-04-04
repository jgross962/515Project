# Bayesian Logistic Regression

# Import Files
setwd("C:/Users/jgros/Documents/GitHub/515Project/data")
temp = list.files(pattern="*.csv")

for (i in 1:length(temp)){
  assign(temp[i], read.csv(temp[i]))
}


setwd("C:/Users/jgros/Documents/GitHub/515Project/Ratings")
temp = list.files(pattern="*.xlsx")

library(readxl)
for (i in 1:length(temp)){
  assign(temp[i], readxl_example(temp[i]))
}


## NEED TO CALCULATE THINGS AND SPLIT INTO TRAINING TESTING DATA HERE

##Logistic Regression
logist= function(x){
  ff= 1/(1 + exp(-x))
  return(ff)
}
# install.packages("arm")
library(arm)
# ?bayesglm
bayesModel= bayesglm(y.train~x.train, family=gaussian(link =identity),
                     prior.mean = 0, prior.scale = NULL)

#Alternate
# bayesModel= bayesglm(y.train~x.train, family=family=binomial(link=logit),
#                      prior.mean = 0, prior.scale = NULL)
bayes.train.MSE = mean((y.train-bayesModel$fitted.values)^2)
bayesModel.LogistPredict = logist(x.test%*%fit6$coefficients)
bayesModel.test.MSE = mean((bayesModel.LogistPredict-y.test)^2)

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

############################ Define Function to make OOS Predicts #########################################################

OOSPredicts = function(x,y, test.indexes = sample(length(y),as.integer(length(y)/10))){
  # NOTE: THIS IS NOT DOING INTERNAL CROSS-VALIDATION right now elastic net models, b/c too slow 
  # Run Code below function scipt once to determine hyper parameter (lambda) for elastic net models
  
  #test.indexes = sample(1074,107)
  x.train = x[-test.indexes,]
  x.test = x[test.indexes,]
  y.train = y[-test.indexes]
  y.test = y[test.indexes]
  
  
  ##Now predict them
  logist<- function(x){
    ff<- 1/(1 + exp(-x))
    return(ff)
  }
  
  # For fits 1,2, 3 we optimize the hyperparameters once, for efficiency (loses some accuracy)
  # Lasso
  fit1<- glmnet(x = x.train, y = y.tra
                t)
  
  
  # Elastic Net, Alpha = .5
  fit2<- glmnet(y = y.train, x= x.train, alpha=0.5, family='binomial')
  fit2.predict = predict(fit2, s= best.lambda2, newx = x.test)
  fit2.logistPred = logist(fit2.predict)
  
  # Elastic Net, Alpha = .25
  fit3<- glmnet(y = y.train, x= x.train, alpha=0.25, family='binomial')
  fit3.predict = predict(fit3, s= best.lambda3, newx = x.test)
  fit3.logistPred = logist(fit3.predict)
  
  # Fit4 not published in paper -- so not doing it
  #fit4<- cv.glmnet(y = Y, x= Xfull, alpha=0, family='binomial', type='mse')
  
  ## Skipping FindIt since documentation changed (per)
  
  # Bayesian GLM -- Revisit -- probably not working right
  fit6<- bayesglm(y.train~x.train-1, family=binomial(link=logit))
  fit6.predict = logist(x.test%*%fit6$coefficients)
  
  # Fit 7 = Boosted Trees is not published ### SKipping
  
  # Fit 8 = BART
  fit8<- bart(x.train=x.train, y.train=factor(y.train), x.test=x.test, ndpost=1000, nskip=500, usequants=T)
  fit8.pred<- pnorm(apply(fit8$yhat.test, 2, mean))
  
  # Fit 9  = RandomForest
  fit9<- randomForest(y = factor(y.train), x = x.train)
  X.test.forest = x.test
  colnames(X.test.forest) = colnames(x.train)
  fit9.pred.raw = predict(fit9,newdata = X.test.forest,type = "prob" )
  fit9.pred = fit9.pred.raw[,2]
  
  
  # Fit 10 = Skipped in Paper and SLF_round2 code
  
  # Fit 11 = KRLS
  # Gets errors if all vals in column have same value (i.e. all 0 or all 1)
  x.train.krls = x.train[,-1]
  x.test.krls = x.test[,-1]
  
  bad.KRLS.index=numeric()
  sums = as.numeric(apply(x.train.krls,2,sum))
  bad.KRLS.index = which(sums==0)
  bad.KRLS.index = c(bad.KRLS.index, which(sums==dim(x.train.krls)[1]))
  
  if (length(bad.KRLS.index)>0){
    x.train.krls = x.train.krls[,-bad.KRLS.index]
    x.test.krls = x.test.krls[,-bad.KRLS.index]
  }
  fit11<- krls(X = x.train.krls, y = y.train, derivative=F)
  
  fit11.predict = predict(fit11,newdata = x.test.krls )$fit
  
  # Fit 12 = SVM-SMO
  .jinit(parameters="-Xmx4g")
  subset.index = (1:length(y))[-test.indexes]
  fit12 <- SMO(y ~ ., data = data.frame(y=factor(y),x), control = Weka_control(M = TRUE ) , subset = subset.index)
  fit12.predict =predict(fit12, newdata= data.frame(x[test.indexes,]), type="probability" )[,2]
  
  
  # Fit 13 = Simple Mean
  fit13.predict = mean(y.train)
  fit13.RSME= sqrt(mean((fit13.predict-y.test)^2))
  
  # # W/O FIt 12 (Java)
  # models = rbind("Lasso", "Elastic Net (a = .5)","Elastic Net (a = .25)", "Bayesian GLM", "BART", "Random Forest", "KRLS" , "Simple Average")#"SVM_SMO"
  # Preds.All = cbind(fit1.logistPred,fit2.logistPred,fit3.logistPred,fit6.predict,fit8.pred,fit9.pred,fit11.predict,  fit13.predict)#fit12
  
  
  # W/ Fit 12 (Java)
  models = rbind("Lasso", "Elastic Net (a = .5)","Elastic Net (a = .25)", "Bayesian GLM", "BART", "Random Forest", "KRLS", "SVM_SMO", "Simple Average")
  Preds.All = cbind(fit1.logistPred, fit2.logistPred ,fit3.logistPred,fit6.predict,fit8.pred,fit9.pred,fit11.predict, fit12.predict,  fit13.predict)
  
  colnames(Preds.All) = models
  return(Preds.All)
}


#### Load Libraries

library(glmnet)
library(arm)
library(BayesTree)
library(randomForest)
library(KRLS)
library(rjava)
library(randomForest)
library(KRLS)
library(rJava)
library(RWeka)

###### GLMnet Hyper Parameters
# To improve runtime, with some loss of precision:
# determine hyperparameters for glmnet models once, instead of reoptimizing hyperparameters via cross validation everytime

# Lasso
fit1<- cv.glmnet(x = Xfull.original, y = Y.original, alpha=1, family='binomial', type='mse')
best.lambda1 = fit1$lambda.min

# Elastic Net, Alpha = .5
fit2<- cv.glmnet(y = Y.original, x= Xfull.original, alpha=0.5, family='binomial', type='mse')
best.lambda2 = fit2$lambda.min

# Elastic Net, Alpha = .25
fit3<- cv.glmnet(y = Y.original, x= Xfull.original, alpha=0.25, family='binomial', type='mse')
best.lambda3 = fit3$lambda.min

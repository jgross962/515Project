# Bayesian Logistic Regression
# Jon Gross

########### Libraries ###########
library(readxl)
library(arm)
library(caret)


############ Import Files ####################
###### Bring in Price Data

# set directory
setwd("C:/Users/jgros/Documents/GitHub/515Project/data")
# Get Files Names
temp = list.files(pattern="*.csv")

#Get list of just ticker Symbols
getTickers = function(x){
  return(
    substring(x,1,nchar(x)-4)
  )
}
ticker.symbols = sapply(temp, getTickers)


price.data = matrix(nrow = 30, ncol = 124)
# Import Price Data
for (i in 1:length(temp)){
  price.data[i,] = read.csv(temp[i])$Adj.Close
}
rownames(price.data) = ticker.symbols

######## Bring in Ratings
# set directory for ratings
setwd("C:/Users/jgros/Documents/GitHub/515Project/Ratings")
# Get File Names
tempRat = list.files(pattern="*.xlsx")

# Import Ratings Data
ratings.data = matrix(nrow = 30, ncol = 5)
for (i in 1:length(tempRat)){
  ratings.data[i,] = read_excel(tempRat[i])[[5]]
}
#ratings.data = as.data.frame(ratings.data)
rownames(ratings.data) = ticker.symbols
colnames(ratings.data) = c("5","4","3","2","1")

########## Bring in Market Data
setwd("C:/Users/jgros/Documents/GitHub/515Project/")
market.return = read.csv("NASDAQ.csv")$Return[2:124]

######################## Data Pre-Processing ####################

###### Calculate Returns Vectors

returns.data = matrix(nrow = 30, ncol = 123)
for (i in 1:123){
  returns.data[,i] = (price.data[,i+1]-price.data[,i])/price.data[,i] # In decimal Format
}


###### Calculate Avg Ratings

avg.rating = apply(ratings.data, 1, mean)
ratings.values = (apply(ratings.data,1, function (values){ values*5:1}))
ratings.values = t(ratings.values)
avg.rating = apply(ratings.values,1,sum)/apply(ratings.data,1,sum)
avg.rating
scaledAvgRating =avg.rating/5
scaledAvgRating


##### Std of ratings
ratings.raw = list()
scaled.ratings.raw = list()
for (i in 1:30){
 ratings.raw[[i]] = numeric()
 for (j  in 5:1){
   ratings.raw[[i]] = c(ratings.raw[[i]],rep(j,ratings.data[i,6-j]))
 } 
 scaled.ratings.raw[[i]] = ratings.raw[[i]]/5
}

sd.ratings = sapply(scaled.ratings.raw,sd)
var.ratings = sapply(scaled.ratings.raw,var)

###### Split to Training Testing Data

training.index = 1:83
price.training = price.data[,training.index]
price.testing = price.data[,-training.index]
returns.training = returns.data[,training.index]
returns.testing = returns.data[,-training.index]



###########  Simple Frequentist -- Count how often stock beat market by # days ############
#### Beat Market By Number of Days (Training)
adj.return= matrix(nrow = 30, ncol = 123)
for (i in 1:30){
  adj.return[i,] = returns.data[i,]-market.return
  
}
beat.market = (adj.return>0)
beat.market[which(beat.market==0)] = -1

beat.market.training = beat.market[,1:83]
beat.market.testing = beat.market[,84:123]

cum.beat.market = apply(beat.market,1,sum)
cum.beat.market.train = apply(beat.market.training,1,sum)
cum.beat.market.testing = apply(beat.market.testing,1,sum)

days.cum.beat.market = cum.beat.market>0
days.cum.beat.market.train = cum.beat.market.train>0
days.cum.beat.market.test = cum.beat.market.testing>0

sum(days.cum.beat.market) ## Note 11/30 companies beat market on most days
sum(days.cum.beat.market.train) # 15 beat in training
sum(days.cum.beat.market.test) # 7 beat in testing

confusionMatrix(as.factor(days.cum.beat.market.train),as.factor(days.cum.beat.market.test))
# 53.333% Accuracy


########### Also simple frequenst - based on cumlative return ######################
#### Beat Maraket Across Period (Training)

adj.return.train = adj.return[,1:83]
adj.return.test = adj.return[,84:123]


period.beat.market = apply(adj.return,1,sum)
period.beat.market.train = apply(adj.return.train,1,sum)
period.beat.market.test = apply(adj.return.test,1,sum)

period.beat.market.train.tf = period.beat.market.train>0
period.beat.market.test.tf = period.beat.market.test>0
period.beat.market.tf = period.beat.market>0
sum(period.beat.market.tf) # 14/30 Copmanies had a total return better than the market
sum(period.beat.market.train.tf) # 14 beat in training period
sum(period.beat.market.test.tf) # 7 beat in testing period

confusionMatrix(as.factor(period.beat.market.train.tf),as.factor(period.beat.market.test.tf))
# 63% Accuracy


################## Simple Bayes #################

### Daily Data
## Beta Prior (Since Conjguate Prior For Bernoulli/Binomial)
estBetaParams <- function(mu, var) {
  alpha = -mu * (mu^2-mu+var)/var
  beta = (mu-1)*(mu^2-mu+var)/var
 # alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
 #  beta <- alpha * (1 / mu - 1)
  params = c(alpha,beta)
  
  return(c(alpha, beta))
}

beta.params = matrix(nrow = 30, ncol = 2)
for (i in 1:30){
  beta.params[i,] = estBetaParams(scaledAvgRating[i],var.ratings[i])
}
colnames(beta.params) = c("alpha","beta")
beta.params

n.train = 83
successes.train = apply((adj.return.train>0),1,sum)

alpha.posterior = successes.train+beta.params[,1]
beta.posterior= beta.params[,2] + n.train - successes.train

prob.beat.market = numeric(30)
for ( i in 1:30){
  prob.beat.market[i] = pbeta(.5,alpha.posterior[i],beta.posterior[i])
}
prob.beat.market
tf.prob.beat.market = prob.beat.market>.5
confusionMatrix(as.factor(tf.prob.beat.market),as.factor(period.beat.market.test.tf))


####### Using all historical (simple.cum.beat.market) data as one observation from bernoulli distribution


#################### Bayesian Logistic Regression ###########################

##Logistic Function
logist= function(x){
  ff= 1/(1 + exp(-x))
  return(ff)
}

indexes = 1:30
oos.preds = numeric(30)
for (j in 1:10){
  test.indexes = (3*j-2):(3*j)
  train.indexes = indexes[-test.indexes]
  x.train = beat.market.training[train.indexes,]
  y.train = as.numeric(period.beat.market.test.tf[train.indexes])
  x.test = beat.market.training[test.indexes,]
  bayesModel= bayesglm(y.train~x.train, family=gaussian(link =identity),
                     prior.mean =  mean(scaledAvgRating), prior.scale = mean(var.ratings))
  bayesModel.LogistPredict = logist(x.test%*%bayesModel$coefficients[2:84]+bayesModel$coefficients[1])
  oos.preds[test.indexes] = bayesModel.LogistPredict
  
}

logist.preds = oos.preds>.5
confusionMatrix(as.factor(logist.preds),as.factor(period.beat.market.test.tf))


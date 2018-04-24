# Bayesian Logistic Regression
# Jon Gross

########### Libraries ###########
library(readxl)
library(arm)



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

#### Bring in Market Data
setwd("C:/Users/jgros/Documents/GitHub/515Project/")



######################## Data Pre-Processing ####################

returns.data = data.matrix()

###### Calculate Returns Vectors

returns.data = matrix(nrow = 30, ncol = 123)
for (i in 1:123){
  returns.data[,i] = (price.data[,i+1]-price.data[,i])/price.data[,i] # In decimal Format
}


###### Calculate Avg Ratings

avg.rating = apply(ratings.data, 1, mean)

scaledAvgRating =avg.rating/5


###### Split to Training Testing Data

training.index = 1:83
price.training = price.data[,training.index]
price.testing = price.data[,-training.index]
returns.training = returns.data[,training.index]
returns.testing = returns.data[,-training.index]

beat.market = retur



#################### Logistic Regression ###########################

##Logistic Function
logist= function(x){
  ff= 1/(1 + exp(-x))
  return(ff)
}
# install.packages("arm")

# ?bayesglm

for (i in 1:30){
  bayesModel= bayesglm(y.train~x.train, family=gaussian(link =identity),
                     prior.mean = 0, prior.scale = NULL)
  bayes.train.MSE = mean((y.train-bayesModel$fitted.values)^2)
  bayesModel.LogistPredict = logist(x.test%*%bayesModel$coefficients)
  bayesModel.test.MSE = mean((bayesModel.LogistPredict-y.test)^2)

}





### Probably Delete This


########### Libraries ###########
library(readxl)
library(arm)



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


# Create Variable Names
makeNames = function(x){
  return(
    paste(substring(x,1,nchar(x)-4),"_data",sep = "")
  )
}
data.names = sapply(temp, makeNames)

price.data = matrix(nrow = 124, ncol = 30)
# Import Price Data
for (i in 1:length(temp)){
  price.data[,i] = read.csv(temp[i])$Adj.Close
  assign(data.names[i], read.csv(temp[i]))
}

######## Bring in Ratings
# set directory for ratings
setwd("C:/Users/jgros/Documents/GitHub/515Project/Ratings")
# Get File Names
tempRat = list.files(pattern="*.xlsx")

# Create Variable Names
makeRatingsNames = function(x){
  return(
    paste(substring(x,1,nchar(x)-4),"_ratings",sep = "")
  )
}
rating.names = sapply(temp, makeRatingsNames)

# Import Ratings Data

for (i in 1:length(tempRat)){
  assign(rating.names[i], read_excel(tempRat[i]))
}



######################## Split Data into Training/Testing ####################
setwd("C:/Users/jgros/Documents/GitHub/515Project/")

## Split to Test and Training
for (i in 1:length(ticker.symbols)){
  assign(c(ticker.symbols[i],"trainingdata"),numeric(123))
  for (j in 2:124){
    te
  }
  assign(c(ticker.symbols[i],"trainingdata"),PG_data[])
}


##Logistic Function
logist= function(x){
  ff= 1/(1 + exp(-x))
  return(ff)
}
# install.packages("arm")

# ?bayesglm
bayesModel= bayesglm(y.train~x.train, family=gaussian(link =identity),
                     prior.mean = 0, prior.scale = NULL)

#Alternate
# bayesModel= bayesglm(y.train~x.train, family=family=binomial(link=logit),
#                      prior.mean = 0, prior.scale = NULL)
bayes.train.MSE = mean((y.train-bayesModel$fitted.values)^2)
bayesModel.LogistPredict = logist(x.test%*%fit6$coefficients)
bayesModel.test.MSE = mean((bayesModel.LogistPredict-y.test)^2)





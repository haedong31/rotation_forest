#########################################################
#Neighbor preseving embedding
#########################################################
install.packages("proxy", dependencies = TRUE)

library(proxy)
##################
#Data preparation#
##################
#Load data
trialData = datasets::iris

#Select a set of training indices
trainingIdx <- sample(1:nrow(trialData), round(0.7*nrow(trialData)))

#Split the data into training data and test data
trainingData <- trialData[trainingIdx, ]
testData <- trialData[-trainingIdx, ]

#make training data be unique cause t-sne working with unique data only
uniqTr <- unique(trainingData)

#Select only independent variables, remove target variable in other words
trX <- subset(uniqTr, select = -c(Species))
teX <- subset(testData, select = -c(Species))

#Build a distace matrix between the trX and teX
#check that what similarty and distance measures I got
summary(pr_DB)

#distance matrix 
distMx <- dist(x = trX, y = teX, method = "Euclidean")
invDistMx <- 1/distMx
norDistMx <- apply(invDistMx, 2, function(x){x/sum(x)}) 

######################
#Non-linear embedding#
######################
tsneOut_tr <- Rtsne(uniqTr[, 1:4], dim = 2)$Y
plot(tsneOut_tr, col = uniqTr[, 5])

ebd_te <- t(norDistMx) %*% tsneOut_tr
plot(ebd_te, col = testData[, 5])


plot(trainingData[, 1:2], col = trainingData[, 5])
plot(testData[, 1:2], col = testData[, 5])


#AGENDA: 
#Test running on 'iris' dataset
#1. Random Forest
#2. Rotation Forest
#3. Twisted Forest

install.packages('randomForest', dependencies = TRUE)
install.packages('rotationForest', dependencies = TRUE)
install.packages('vegan', dependencies = TRUE)
install.packages('Rtsne', dependencies = TRUE)
install.packages('lle', dependencies = TRUE)
install.packages('caret', dependencies = TRUE)

library(randomForest)
library(rotationForest)
library(vegan)
library(Rtsne)
library(lle)
library(caret)


#importing 'iris' dataset
irisData = datasets::iris
str(irisData)

setwd('/Users/haedongkim/Google 드라이브/R16RF/data')
data4 = read.csv('data4.csv')
data4x = data4[ , 1:7]

#testing t-sne on categorical variables; working

tsneData4 = Rtsne(data4x)
tsneData4$Y

plot(tsneData4$Y, col = data4$Y)

#testing lle on categorical variables; not workin g
lleData4 = lle(data4x, k=2)

#testing isomap on categorical variables
isoData4 = isomap(data4x)

#for-loop
#Step 1. split the dataset into training data and test data
#Step 2. fit a model of random forest 
#Step 3. put a result of the model into a confusionMatrix function 
#Step 4. save the each result for doing a   t-test after the iterations (30 iter)

#initialization
i =1 
rfContainerAcc = list()
rfContainerOther = list()

irisData_x = irisData[ , 1:4]
irisPca = prcomp(irisData_x, center = TRUE, scale. = TRUE)
irisPca[[2]][ ,1]
irisPca_reduced = irisPca[[2]][ , 1:2]
as.matrix(irisData_x) %*% as.matrix(irisPca_reduced)

for(i in 1:30){#for-loop begin
  trainIdx = sample(1:nrow(irisData), round(0.7*nrow(irisData)))
  irisTrain = irisData[trainIdx, ]
  irisTest = irisData[-trainIdx, ]
  
  #1. Valina random forest
  rfm = randomForest(Species ~ ., data = irisTrain)
  prtd = predict(rfm, irisTest)
  
  cfmx = confusionMatrix(prtd, irisTest[ , 'Species'])
  
  rfContainerAcc[[i]] = cfmx$overall
  rfContainerOther[[i]] = cfmx$overall
  
  #2. Rotation forest
  rotatedModel = rotationForest(irisTrain, irisTrain$Species)
  rotationForest
  #3. Twisted forest with Isomap
  
  #4. Twisted forest with t-sne
  
  #5. Twisted forest with LLE
}#for-loop end
?randomForest



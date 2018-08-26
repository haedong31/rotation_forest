#Rotation forest
#Writer: Haedong Kim

install.packages("Matrix", dependencies = TRUE)
install.packages("vegan", dependencies = TRUE)
install.packages("Rtsne", dependencies = TRUE)
install.packages("lle", dependencies = TRUE)
install.packages("randomForest", dependencies = TRUE)
install.packages("caret", dependencies = TRUE)

library(Matrix)
library(vegan)
library(Rtsne)
library(lle)
library(randomForest)
library(caret)

#function 1: get random k subset of origin data X
#input: 
#1. independent variables
#2. dependent variable (should be factor)
#3. number of features which in each subset
#output: List of subsets with randomly selected features
#declaration & initialization
fn1_getRandomSubset = function(x, y, k){
  numSubset <- round(length(x)/k)
  subsetList <- list()
  subsetIdx <- 0
  
  #check that number of subserts is proper
  if(numSubset <= length(x)){
    subsetIdx <- numSubset
  }else{
    subsetIdx <- length(x)
  }
  
  #give features ordering numbers for indentifing the original sequence of features
  colnames(x) <- 1:length(x) 
  #shuffling features randomly
  x <- subset(x, select = sample(1:length(x))) 
  
  #pick out k-1 subsets
  i <-  1
  while(i<numSubset){
    subsetList[[i]] <- cbind(x[ , 1:numSubset], y)
    x <- x[ , -(1:numSubset)]
    i <- i+1
  }
  #pick out last subset (kth subset)
  # The last kth subset may include more or less records than the other subsets 
  # when the number of subets is not an integer
  subsetList[[numSubset]] <- cbind(x, y)
  return(subsetList)
}



#function 2: bootstrapping
#input: 
#1. list of randomly generated subsets
#2. bootstrapping rate
#output: bootstrapped subset list
fn2_bootstrap = function(subsetList, bootstrapRate){
  bootIdxList <- list()
  bootSubsetList <- list()
  
  for(i in 1:length(subsetList)){#for-loop begins
    #bootstrap sampling
    numRecord <- nrow(data.frame(subsetList[[i]]))
    bootIdxList[[i]] <- sample(1:numRecord, round(bootstrapRate*numRecord), replace = TRUE)
    
    #save a bootstrapped subset in the 'bootSubsetList'
    bootSubsetList[[i]] <- subset(data.frame(subsetList[[i]][bootIdxList[[i]], ]), select = c(-y))
    names(bootSubsetList)[i] <- paste0("bootstraped", i)
  }#for-loop ends
  return(bootSubsetList)   
}



#function 3: PCA
#input:
#1. bootstrapped subset list
#2. original training data x
#output: matrix constructed by projecting original data X onto a rotation matrix
fn3_PCA = function(bootSubsetList, x, y){
  PCAcomp <- lapply(bootSubsetList, function(x){prcomp(x, center = TRUE)$rotation})
  PCAcompT <- lapply(PCAcomp, t)
  PCArrangement <- as.vector(unlist(lapply(PCAcompT, colnames)))
  #'bdiag' is a function included in the R package 'Matrix'
  #It returns block-shaped diagonal matrix which is sparse
  rotationMatrix <- as.matrix(do.call(bdiag, lapply(PCAcompT, as.matrix)))
  colnames(rotationMatrix) <- PCArrangement
  rotationMatrix <- subset(rotationMatrix, select = sort(PCArrangement))
  x <- as.matrix(x)
  RmxPCA <- cbind(data.frame(x %*% rotationMatrix), y)
  return(RmxPCA)
}


#function 4: non-linear embedding
#input: 
#1. subset list
#2. original dependant variable of a training dataset
#3. name of a non-linear embedding
#output: matrix embedded by non-linear methods; LLE, t-SNE, ISOMAP
fn4_nonlinear = function(subsetList, y, dimRdcMethod){
  subsetListRd <- lapply(subsetList, function(x){subset(x, select = c(-y))})
  varArrangement <- as.vector(unlist(lapply(subsetListRd, colnames)))
  
  switch (dimRdcMethod,
  LLE = {
    LLEresult <- lapply(subsetListRd, function(x){
      lle(x, m = length(x), k = 10, reg = 2, id = TRUE)})
    LLEcomp <- lapply(LLEresult, function(x){x$Y})
    rotationMatrix <- do.call(cbind, lapply(LLEcomp, as.matrix))
    colnames(rotationMatrix) <- varArrangement
    rotationMatrix <- subset(rotationMatrix, select = sort(varArrangement))
    RmxNonlinear <- cbind(data.frame(rotationMatrix), y)
  },#LLE ends
  tSNE = { 
    #make duplicated records unique before running t-SNE
    while (sum(unlist(lapply(subsetListRd, duplicated))) != 0) {
      for (i in 1:length(subsetListRd)) {
        dupliIdx <- which(duplicated(subsetListRd[[i]]))
        subsetListRd[[i]][dupliIdx, ] <- apply(subsetListRd[[i]][dupliIdx, ], 2, function(x){
          x <- x + runif(n = length(dupliIdx), min = min(x), max = max(x)) })
      }#for ends
    }#while ends
    tsneOut <- lapply(subsetListRd, function(x){Rtsne(x, dims = 2, perplexity = TRUE)})
    tsneComp <- lapply(tsneOut, function(x){x$Y})
    rotationMatrix <- do.call(cbind, lapply(tsneComp, as.matrix))
    colnames(rotationMatrix) <- varArrangement
    rotationMatrix <- subset(rotationMatrix, select = sort(varArrangement))
    RmxNonlinear <- cbind(data.frame(rotationMatrix), y)
  },#tSNE ends
  ISOMAP = {
    subsetDist <- lapply(subsetListRd, dist)
    nDim <- lapply(subsetListRd, length)
    
    isomapOut <- list()
    for(i in 1:length(subsetListRd)){
      isomapOut[[i]] <- isomap(dist = subsetDist[[i]], ndim = nDim[[i]], k = 10)
    }
    isomapOut <- lapply(subsetDist, function(x){isomap(dist = x, k = 5,ndim = length(x), fragmentedOK = TRUE)})
    trial <- subsetDist[[1]]
    
    length(trial)
    isomap(dist = trial, ndim = length())
    length(subsetListRd[[1]])
    
    originalX <- subset(originalX, select = sort(varArrangement))
    xDist <- dist(originalX)
    isomapOut <- isomap(dist = xDist, k = 5,ndim = length(varArrangement), fragmentedOK = TRUE)
    rotationMatrix <- isomapOut$points
  }#ISOMAP ends
  )#switch ends
  RmxNonlinear <- cbind(data.frame(rotationMatrix), y)
  return(RmxNonlinear)
}



#function 5: PCA rotation forest
#input: x, y, k, bootstrap rate
#output: 
fn5_PCArf = function(x, y, k, bootstrapRate){
  subsetList <- fn1_getRandomSubset(x = x, y = y, k = k)
  bootSubsetList <-  fn2_bootstrap(subsetList = subsetList, bootstrapRate = bootstrapRate)
  RmxPCA <- fn3_PCA(bootSubsetList = bootSubsetList, x = x, y = y)
  fn5Train <- cbind(data.frame(x), y)
  PCArf <- randomForest(as.factor(y) ~., fn5Train)
  return(PCArf)
}
#function 6: non-linear rotation forest 
fn6_NLrf = function(x, y, k, dimRdcMethod){
  subsetList <- fn1_getRandomSubset(x = x, y = y, k = k)
  RmxNL <- fn4_nonlinear(subsetList = subsetList, y = y, dimRdcMethod = dimRdcMethod)
  fn6Train <- cbind(data.frame(x), y)
  NLrf <- randomForest(as.factor(y) ~., fn6Train)
  return(NLrf)
}

#main
trialData = datasets::iris
trialX = trialData[ , 1:4]
trialY = trialData[ , 5]
str(trialData)

#initialize
accMx <- matrix(0, nrow = 4, ncol = 1)
rownames(accMx) <- c("randomForest", "PCA", "LLE", "tSNE")
colnames(accMx) <- "Accuracy"
basicClassMeasure <- 0
PCAClassMeasure <- 0
LLEClassMeasure <- 0
tSNEClassMeasure <- 0 
i = 1
for(i in 1:30){
  trainingIdx <- sample(1:nrow(trialData), round(0.7*(nrow(trialData))))
  trainingData <- trialData[trainingIdx, ]
  trainingX <- trialX[trainingIdx, ]
  trainingY <- trialY[trainingIdx]
  
  trialTest <- trialData[-trainingIdx, ]
  
  #training
  basicrf <-  randomForest(as.factor(Species) ~., data = trainingData)
  PCArf <- fn5_PCArf(x = trainingX, y = as.factor(trainingY), k = 2, bootstrapRate = 0.75)
  LLErf = fn6_NLrf(x = trainingX, y = trainingY, k = 2, dimRdcMethod = "LLE")
  tSNErf = fn6_NLrf(x = trainingX, y = trainingY, k = 2, dimRdcMethod = "tSNE")
  
  #predicting
  basicPrtd <- predict(basicrf, trialTest)
  PCAPrtd <- predict(PCArf, trialTest)
  LLEPrtd <- predict(LLErf, trialTest)
  tSNEPrtd <- predict(tSNErf, trialTest)
  
  #performance evaluation
  basicCfmx <- confusionMatrix(basicPrtd, trialTest[ , "Species"])
  PCACfmx <- confusionMatrix(PCAPrtd, trialTest[ , "Species"])
  LLECfmx <- confusionMatrix(LLEPrtd, trialTest[ , "Species"])
  tSNECfmx <- confusionMatrix(tSNEPrtd, trialTest[ , "Species"])
  
  #summurize results of evaluation
  accMx[1] <- accMx[1] + basicCfmx$overall[1]
  accMx[2] <- accMx[2] + PCACfmx$overall[1]
  accMx[3] <- accMx[3] + LLECfmx$overall[1]
  accMx[4] <- accMx[4] + tSNECfmx$overall[1]
  
  basicClassMeasure <-  basicClassMeasure + basicCfmx$byClass
  PCAClassMeasure <- PCAClassMeasure + PCACfmx$byClass
  LLEClassMeasure <- LLEClassMeasure + LLECfmx$byClass
  tSNEClassMeasure <- tSNEClassMeasure + tSNECfmx$byClass
  
  cat(i, "th loop out of 30", "\n")
}
 
accMx/30
basicClassMeasure/30
PCAClassMeasure/30
LLEClassMeasure/30
tSNEClassMeasure/30


# subsetName <- paste0('bootSubset', i)
# assign(subsetName, data.frame(subsetList[[i]])[bootIdx, ]) 

######trials...####
accMx <- matrix(0, nrow = 4, ncol = 1)
rownames(accMx) <- c("randomForest", "PCA", "LLE", "tSNE")
colnames(accMx) <- "Accuracy"
basicClassMeasure <- 0
PCAClassMeasure <- 0
LLEClassMeasure <- 0
tSNEClassMeasure <- 0 
#i = 1
for(i in 1:30){
  trainingIdx <- sample(1:nrow(trialData), round(0.7*(nrow(trialData))))
  trainingData <- trialData[trainingIdx, ]
  trainingX <- trialX[trainingIdx, ]
  trainingY <- trialY[trainingIdx]
  
  trialTest <- trialData[trainingIdx, ]
  
  #training
  basicrf <-  randomForest(as.factor(Species) ~., data = trainingData)
  PCArf <- fn5_PCArf(x = trainingX, y = as.factor(trainingY), k = 2, bootstrapRate = 0.75)
  LLErf = fn6_NLrf(x = trainingX, y = trainingY, k = 2, dimRdcMethod = "LLE")
  tSNErf = fn6_NLrf(x = trainingX, y = trainingY, k = 2, dimRdcMethod = "tSNE")
  
  #predicting
  basicPrtd <- predict(basicrf, trialTest)
  PCAPrtd <- predict(PCArf, trialTest)
  LLEPrtd <- predict(LLErf, trialTest)
  tSNEPrtd <- predict(tSNErf, trialTest)
  
  #performance evaluation
  basicCfmx <- confusionMatrix(basicPrtd, trialTest[ , "Species"])
  PCACfmx <- confusionMatrix(PCAPrtd, trialTest[ , "Species"])
  LLECfmx <- confusionMatrix(LLEPrtd, trialTest[ , "Species"])
  tSNECfmx <- confusionMatrix(tSNEPrtd, trialTest[ , "Species"])
  
  #summurize results of evaluation
  accMx[1] <- accMx[1] + basicCfmx$overall[1]
  accMx[2] <- accMx[2] + PCACfmx$overall[1]
  accMx[3] <- accMx[3] + LLECfmx$overall[1]
  accMx[4] <- accMx[4] + tSNECfmx$overall[1]
  
  basicClassMeasure <-  basicClassMeasure + basicCfmx$byClass
  PCAClassMeasure <- PCAClassMeasure + PCACfmx$byClass
  LLEClassMeasure <- LLEClassMeasure + LLECfmx$byClass
  tSNEClassMeasure <- tSNEClassMeasure + tSNECfmx$byClass
  
  cat(i, "th loop out of 30", "\n")
}

accMx/30
basicClassMeasure/30
PCAClassMeasure/30
LLEClassMeasure/30
tSNEClassMeasure/30


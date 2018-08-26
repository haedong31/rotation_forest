#Rotation forest with various embedding
#Writer: Haedong Kim
#Begin: ?
#Fixed 25 AUG 16: non-linear rotation matrix, add the bootstrapping process
#

library(rotationForest)

install.packages("Matrix", dependencies = TRUE)
install.packages("vegan", dependencies = TRUE)
install.packages("Rtsne", dependencies = TRUE)
install.packages("lle", dependencies = TRUE)
install.packages("rpart", dependencies = TRUE)
install.packages("randomForest", dependencies = TRUE)
install.packages("caret", dependencies = TRUE)

library(Matrix)
library(vegan)
library(Rtsne)
library(lle)
library(rpart)
library(randomForest)
library(caret)

#########################################################
#function 1 
#get random k subsets from the original data X
#subsets are differ in features but have same number of records
#input 
#x: independent variables
#y: dependent variable (should be a factor)
#k: number of features which are in each subset             
#output: List of subsets with randomly selected features
#########################################################

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
  
  #give features names for indentifing the original sequence of features
  colnames(x) <- paste0("X", 1:length(x))
  
  #shuffling features randomly
  x <- subset(x, select = sample(1:length(x))) 
  
  #pick out k-1 subsets
  i <-  1
  while(i<numSubset){
    subsetList[[i]] <- cbind(x[ , 1:numSubset], y)
    x <- x[ , -(1:numSubset)]
    i <- i+1
  }
  #pick out the last subset (kth subset)
  # The last kth subset may include more or less records than the other subsets 
  # when the number of subets is not an integer
  subsetList[[numSubset]] <- cbind(x, y)
  return(subsetList)
}


#########################################################
#function 2: bootstrapping
#input
#subsetList: list of randomly generated subsets
#bootstrapRate: bootstrapping rate
#output: bootstrapped subset list
#########################################################
fn2_bootstrap = function(subsetList, bootstrapRate){
  bootIdxList <- list()
  bootSubsetList <- list()
  
  for(i in 1:length(subsetList)){#for-loop begins
    #bootstrap sampling
    numRecord <- nrow(data.frame(subsetList[[i]]))
    bootIdxList[[i]] <- sample(1:numRecord, round(bootstrapRate*numRecord), replace = TRUE)
    
    #save a bootstrapped subset in the 'bootSubsetList'
    bootSubsetList[[i]] <- data.frame(subsetList[[i]][bootIdxList[[i]], ])
    names(bootSubsetList)[i] <- paste0("bootstraped", i)
  }#for-loop ends
  return(bootSubsetList)   
}


#########################################################
#function 3: PCA
#input
#bootSubsetList: bootstrapped subset list
#x training x of the original data
#y training y of the original data
#output: matrix constructed by projecting original data X onto a rotation matrix
#########################################################

fn3_RmxPCA = function(bootSubsetList){
  bootSubsetListRd <- lapply(bootSubsetList, function(x){subset(x, select = -y)})
  PCAcomp <- lapply(bootSubsetListRd, function(x){prcomp(x, center = TRUE)$rotation})
  PCAcompT <- lapply(PCAcomp, t)
  PCArrangement <- as.vector(unlist(lapply(PCAcompT, colnames)))
  #'bdiag' is a function included in the R package 'Matrix'
  #It returns block-shaped diagonal matrix which is sparse
  rotationMatrix <- as.matrix(do.call(bdiag, lapply(PCAcompT, as.matrix)))
  colnames(rotationMatrix) <- PCArrangement
  rotationMatrix <- subset(rotationMatrix, select = sort(PCArrangement))
  return(as.matrix(rotationMatrix))
}

#########################################################
#function 4: non-linear embedding
#input
#subetList: subset list
#y: training y of the original data
#dimRdcMethod: name of a non-linear embedding
#output: matrix embedded by non-linear methods; LLE, t-SNE, ISOMAP
#########################################################
fn4_NL = function(bootSubsetList, dimRdcMethod){
  bootSubsetListx <- lapply(bootSubsetList, function(x){subset(x, select = c(-y))})
  bootSubsetListy <- lapply(bootSubsetList, function(x){subset(x, select = c(y))})
  
  #saving variable arrangement for rearranging as the original sequence after dimensionality reduction
  varArrangement <- lapply(bootSubsetListx, colnames)

  switch (dimRdcMethod,
          LLE = {
            LLEresult <- lapply(bootSubsetListx, function(x){
              lle(x, m = length(x), k = 10, reg = 2, id = TRUE)})
            NLcomp <- lapply(LLEresult, function(x){x$Y})
          },#LLE ends
          tSNE = { 
            #make duplicated records unique before running t-SNE 
            while(sum(unlist(lapply(bootSubsetListx, duplicated))) != 0) {
              for(i in 1:length(bootSubsetListx)) {
                dupliIdx <- which(duplicated(bootSubsetListx[[i]]))
                bootSubsetListx[[i]][dupliIdx, ] <- apply(bootSubsetListx[[i]][dupliIdx, ], 2, function(x){
                  x <- x + rnorm(n = length(dupliIdx), mean = 0)}) #add a white noise
              }#for ends
            }#while ends
            
            #determin value of perplexity
            #If the number of rows is less than three times of dafault perplexity (30), it give an error
            #pick out the number of first bootSubset's rows because every bootstrapped subsets has same number of rows
            if(nrow(bootSubsetList[[1]])-1 < 3*30){ 
              tsneOut <- lapply(bootSubsetListx, function(x){Rtsne(x, dims = length(x), perplexity = floor((nrow(x)-1)/3))})
            } else{
              tsneOut <- lapply(bootSubsetListx, function(x){Rtsne(x, dims = length(x))})
            }
            NLcomp <- lapply(tsneOut, function(x){x$Y})
          }#tSNE ends
        )#switch ends
  
  #put variable names on non-linearly embedded data (i.e. NLcomp)
  #and dependent variable (i.e. y) also
  for(i in 1:length(NLcomp)){
    colnames(NLcomp[[i]]) <- varArrangement[[i]]
    NLcomp[[i]] <- cbind(data.frame(NLcomp[[i]]), bootSubsetListy[[i]])
  }
  return(NLcomp)
}

#########################################################
#function 5: PCA rotation forest
#input: x, y, k, bootstrap rate
#output: 
#########################################################
fn5_PCArf = function(x, y, k, bootstrapRate, numTrees, testX, testY){
  PCArfList <- list()
  RmxList <- list()
  resultsList <- list()
  
  colnames(testX) <- paste0("X", 1:length(testX))
  for(i in 1:numTrees){
    #generate a rotation matrix
    subsetList <- fn1_getRandomSubset(x = x, y = y, k = k)
    bootSubsetList <-  fn2_bootstrap(subsetList = subsetList, bootstrapRate = bootstrapRate)
    RmxPCA <- fn3_RmxPCA(bootSubsetList)
    
    #build a tree model and then put it in the list; PCArfList
    xRy <- cbind(data.frame(as.matrix(x) %*% RmxPCA), y)
    PCArfList[[i]] <- rpart(y ~., method = "class", data = xRy)
    RmxList[[i]] <- RmxPCA
    
    #prediction
    resultsList[[i]] <- predict(PCArfList[[i]], newdata = data.frame(as.matrix(testX) %*% RmxList[[i]]), type = "prob")
  }
  #aggregating the results of each number of trees; mean of probability of classes
  aggResults <- Reduce("+", resultsList)/numTrees
  #pick out the class which has the highest probability result from the aggregating
  prdtResult <- factor(apply(aggResults, 1, function(x){colnames(aggResults)[which(x == max(x))]}),
                  levels = colnames(aggResults))
  
  return(confusionMatrix(prdtResult, testY))
}

#########################################################
#function 6: non-linear rotation forest 
#########################################################
fn6_NLrf = function(x, y, k, dimRdcMethod, numTrees, testX, testY){
  #initialization
  NLrfList <- list()
  resultsList <- list()
  modelContainer <- list()
  resultContainer <- list()
  
  #preparing test data
  colnames(testX) <- paste0("X", 1:length(testX))
  testData <- cbind(data.frame(testX), y = testY)
  NLtestData <- data.frame(fn4_NL(list(testData), dimRdcMethod))
  NLtestDataX <- subset(NLtestData, select = c(-y))
  NLtestDataY <- as.factor(unlist(subset(NLtestData, select = c(y))))
  
  for(i in 1:numTrees){
    subsetList <- fn1_getRandomSubset(x, y, k)
    bootSubsetList <- fn2_bootstrap(subsetList, bootstrapRate = 1)
    #xRy is the list containing embedded data which are generated from the bootstrapped subets
    xRy <- fn4_NL(bootSubsetList, dimRdcMethod)
    for(j in 1:length(xRy)){
       #fitting a tree model
       modelContainer[[j]] <- rpart(y ~ ., data = xRy[[j]], method = "class")
       #saving an immediate result
       resultContainer[[j]] <- predict(modelContainer[[j]], newdata = NLtestDataX, type = "prob")
    }#2nd for-loop end
    #Save "trees" getting from one bootstrapped subsets (i.e. one iteration)
    #It is different with the rotation forest with PCA such that PCA one fit only one tree per each iteration
    NLrfList[[i]] <- modelContainer
    aggResultTree <- Reduce('+', resultContainer)/length(xRy)
    resultsList[[i]] <- aggResultTree
  }#1st for-loop ends
  aggResultForest <- Reduce('+', resultsList)/numTrees
  prtdResult <- factor(apply(aggResultForest, 1, function(x){
    colnames(aggResultForest)[which(x == max(x))]}), levels = colnames(aggResultForest))
  
  #conveting testY into a factor
  NLtestDataY <- factor(NLtestDataY, levels = colnames(aggResultForest))
  return(confusionMatrix(prtdResult, NLtestDataY))
}


#########################################################
#main function
#########################################################
#Data preparation
trialData = datasets::iris
X = trialData[ , 1:4]
Y = trialData[ , 5]

#How many do you like to repeat the experiment?
itrExp <- 30
i <- 1
PCAout <- list()
tSNEout <- list()
LLEout <- list()
for(i in 1:itrExp){
  #get indices for training data
  trainingIdx <- sample(1:nrow(trialData), round(0.7*(nrow(trialData))))
  
  #training data
  trainingData <- trialData[trainingIdx, ]
  trainingX <- X[trainingIdx, ]
  trainingY <- Y[trainingIdx]
  
  #test data
  testData <- trialData[-trainingIdx, ]
  testX <- testData[, 1:4]
  testY <- testData[, 5]
  
  #RF with PCA
  # cat(i, "th PCA", "\n")
  # PCAout[[i]] <- fn5_PCArf(x = trainingX, y = trainingY, k = 2, bootstrapRate = 0.75,
  #           numTrees = 30, testX, testY)

  #RF with NL
  # cat(i, "th tSNE", "\n")
  # tSNEout[[i]] <- fn6_NLrf(x = trainingX, y = trainingY, k = 2, dimRdcMethod = "tSNE",
  #          numTrees = 30, testX, testY)
  
  cat(i, "th LLE", "\n")
  LLEout[[i]] <- fn6_NLrf(x = trainingX, y = trainingY, k = 2, dimRdcMethod = "LLE",
           numTrees = 30, testX, testY)
}
PCAacc <- lapply(PCAout, function(x){x$overall})
Reduce('+', PCAacc)[1]/30
PCAexp <- Reduce(rbind, PCAacc)

tSNEacc <- lapply(tSNEout, function(x){x$overall})
Reduce('+', tSNEacc)[1]/30
tSNEexp <- Reduce(rbind, tSNEacc)

LLEacc <- lapply(LLEout, function(x){x$overall})
Reduce('+', LLEacc)[1]/30
LLEexp <- Reduce(rbind, LLEacc)

#setwd()
write.csv(PCAexp, "PCAacc30.csv", row.names = FALSE)
write.csv(tSNEexp, "tSNEacc30.csv", row.names = FALSE)
write.csv(LLEexp, "LLEacc30.csv", row.names = FALSE)


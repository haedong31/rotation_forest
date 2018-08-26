#Rotation forest with various embedding
#Writer: Haedong Kim
#Begin: ?
#Fixed: SEP 1, 2016
#Fixed: SEP 11, 2016
#Fixed (RF5): SEP 18, 2016

#library(rotationForest)

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
library(proxy)

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
#bootSubsetList <- fn1_getRandomSubset(x = trainingX, y = trainingY, k = 2)
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
            for(i in 1:length(NLcomp)){
              #colnames(NLcomp[[i]]) <- varArrangement[[i]]
              NLcomp[[i]] <- cbind(data.frame(NLcomp[[i]]), y = bootSubsetListy[[i]])
            }
          },#LLE ends
          tSNE = { 
            bootSubsetListxUni <- list()
            bootSubsetListyUni <- list()
            #make duplicated records unique before running t-SNE
            i <- 1
            if(sum(unlist(lapply(bootSubsetListx, duplicated))) != 0){
              for(i in 1:length(bootSubsetListx)){
                dupliIdx <- which(duplicated(bootSubsetListx[[i]]))
                bootSubsetListxUni[[i]] <- bootSubsetListx[[i]][-dupliIdx, ]
                bootSubsetListyUni[[i]] <- bootSubsetListy[[i]][-dupliIdx, ]
              }
            } else{
              bootSubsetListxUni[[i]] <- bootSubsetListx[[i]]
              bootSubsetListyUni[[i]] <- bootSubsetListy[[i]]
            }
            #determin value of perplexity
            #If the number of rows is less than three times of dafault perplexity (30), it give an error
            #pick out the number of first bootSubset's rows because every bootstrapped subsets has same number of rows
            if(nrow(bootSubsetListxUni[[1]])-1 < 3*30){
              tsneOut <- lapply(bootSubsetListxUni, function(x){Rtsne(x, dims = length(x), perplexity = floor((nrow(x)-1)/3))})
            } else{
              tsneOut <- lapply(bootSubsetListxUni, function(x){Rtsne(x, dims = length(x))})
            }
            NLcomp <- lapply(tsneOut, function(x){x$Y})
            #put variable names on non-linearly embedded data (i.e. NLcomp)
            #and dependent variable (i.e. y) also
            for(i in 1:length(NLcomp)){
              #colnames(NLcomp[[i]]) <- varArrangement[[i]]
              NLcomp[[i]] <- cbind(data.frame(NLcomp[[i]]), y = bootSubsetListyUni[[i]])
            }
          }#tSNE ends
  )#switch ends
  return(NLcomp)
}


#########################################################
#function 5: PCA rotation forest
#input: x, y, k, bootstrap rate
#output: results of the prediction and a confusion matrix
#########################################################
fn5_PCArf = function(x, y, k, bootstrapRate, numTrees, testX, testY){
  PCArfList <- list()
  RmxList <- list()
  resultsList <- list()
  xRyList <- list()
  RmxTestList <- list()
  
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
    
    #for plotting
    xRyList[[i]] <- xRy
    RmxTestList[[i]] <- as.matrix(testX) %*% RmxList[[i]]
    
    #prediction
    resultsList[[i]] <- predict(PCArfList[[i]], newdata = data.frame(as.matrix(testX) %*% RmxList[[i]]), type = "prob")
  }
  #aggregating the results of each number of trees; mean of probability of classes
  aggResults <- Reduce("+", resultsList)/numTrees
  #pick out the class which has the highest probability result from the aggregating
  prdtResult <- factor(apply(aggResults, 1, function(x){colnames(aggResults)[which(x == max(x))]}),
                       levels = colnames(aggResults))
  
  lastOut <- list(confusionMatrix(prdtResult, testY), xRyList)
  return(lastOut)
}

#########################################################
#function 6: non-linear rotation forest 
#########################################################
x <- trainingX
y <- trainingY
dimRdcMethod <- "tSNE"
numTrees <- 30

fn6_NLrf = function(x, y, k, dimRdcMethod, numTrees, testX, testY){
  #initialization
  NLrfList <- list()
  resultsList <- list()
  modelContainer <- list()
  resultContainer <- list()
  
  #prepare a training data
  colnames(x) <- paste0("X", 1:length(x))
  trainingX <- as.matrix(x)
  
  #remove duplicated records in the training data for the tSNE
  if(sum(duplicated(trainingX)) == 0){
    trainingX_uniq <- trainingX
    trainingY_uniq <- y
    trainingData_uniq <- cbind(data.frame(trainingX_uniq), y = trainingY_uniq)
  }else{
    dupliIdx <- which(duplicated(trainingX))
    trainingX_uniq <- trainingX[-dupliIdx, ]
    trainingY_uniq <- y[-dupliIdx]
    trainingData_uniq <- cbind(data.frame(trainingX_uniq), y = trainingY_uniq)
  }
  
  
  #trasform a training data into the new space; non-linear embedding
  NLtraining <- data.frame(fn4_NL(list(trainingData_uniq), dimRdcMethod))
  NLtrainingX <- subset(NLtraining, select = c(-y))
  NLtrainingY <- as.factor(unlist(subset(NLtraining, select = c(y))))
  
  #prepare a test data
  colnames(testX) <- paste0("X", 1:length(testX))
  testData <- cbind(data.frame(testX), y = testY)
  
  #build a distance matrix and thus a weight of nbhd matrix
  distMx <- dist(x = trainingX_uniq, y = as.matrix(testX), method = "Euclidean")
  invDistMx <- 1/(distMx + 0.001)
  norDistMx <- as.matrix(apply(invDistMx, 2, function(x){x/sum(x)}))
  
  #transform a test data into the space build in embedding of the training data
  
  
  NLtestX <- data.frame(t(norDistMx) %*% as.matrix(NLtrainingX))
  NLtestY <- as.factor(testY)
  
  
  #build a rotation forest
  for(iTree in 1:numTrees){
    subsetList <- fn1_getRandomSubset(x = NLtrainingX, y = NLtrainingY, k = 2)
    bootSubsetList <- fn2_bootstrap(subsetList, bootstrapRate = 1)
    
    for(j in 1:length(bootSubsetList)){
      #fitting a tree model
      modelContainer[[j]] <- rpart(y ~ ., data = bootSubsetList[[j]], method = "class")
      #saving an immediate result
      resultContainer[[j]] <- predict(modelContainer[[j]], newdata = NLtestX, type = "prob")
    }#2nd for-loop end
    #Save "trees" getting from one bootstrapped subsets (i.e. one iteration)
    #It is different with the rotation forest with PCA such that PCA one fit only one tree per each iteration
    NLrfList[[iTree]] <- modelContainer
    aggResultTree <- Reduce('+', resultContainer)/length(bootSubsetList)
    resultsList[[iTree]] <- aggResultTree
  }#1st for-loop ends
  aggResultForest <- Reduce('+', resultsList)/numTrees
  prtdResult <- factor(apply(aggResultForest, 1, function(x){
    colnames(aggResultForest)[which(x == max(x))]}), levels = colnames(aggResultForest))
  
  #conveting testY into a factor
  NLtestDataY <- factor(NLtestY, levels = colnames(aggResultForest))
  lastOut <- list(confusionMatrix(prtdResult, NLtestY), NLtraining, NLtestX)
  return(lastOut)
}





#########################################################
#main function
#########################################################
#Data preparation
#Trial 1: Iris data
trialData = datasets::iris
#Trial 2: Binary taget data
# setwd("C:\\Users\\DSBA-0\\Google Drive\\R16RF\\data")
# trialData = read.csv("data13.csv", stringsAsFactors = FALSE)
# str(trialData)
# head(trialData, 5)

#How many times do you like to repeat the experiment?
itrExp <- 30
i <- 1
PCAout <- list()
tSNEout <- list()
#LLEout <- list()
for(i in 1:itrExp){
  #get indices for training data
  trainingIdx <- sample(1:nrow(trialData), round(0.7*(nrow(trialData))))
  
  #training data
  trainingData <- trialData[trainingIdx, ]
  trainingX <- trainingData[, 1:4]
  trainingY <- as.factor(trainingData[, 5])
  
  #test data
  testData <- trialData[-trainingIdx, ]
  testX <- testData[, 1:4]
  testY <- as.factor(testData[, 5])
  
  # RF with PCA
  cat(i, "th PCA", "\n")
  PCAout[[i]] <- fn5_PCArf(x = trainingX, y = trainingY, k = 2, bootstrapRate = 0.75,
                           numTrees = 30, testX, testY)
  
  #RF with NL
  cat(i, "th tSNE", "\n")
  tSNEout[[i]] <- fn6_NLrf(x = trainingX, y = trainingY, k = 2, dimRdcMethod = "tSNE",
                           numTrees = 30, testX, testY)
  
  # cat(i, "th LLE", "\n")
  # LLEout[[i]] <- fn6_NLrf(x = trainingX, y = trainingY, k = 2, dimRdcMethod = "LLE",
  #                         numTrees = 30, testX, testY)
}

PCAacc <- lapply(PCAout, function(x){x[[1]]$overall})
Reduce('+', PCAacc)[1]/30
PCAexp <- Reduce(rbind, PCAacc)

tSNEacc <- lapply(tSNEout, function(x){x[[1]]$overall})
Reduce('+', tSNEacc)[1]/30
tSNEexp <- Reduce(rbind, tSNEacc)

LLEacc <- lapply(LLEout, function(x){x[[1]]$overall})
Reduce('+', LLEacc)[1]/30
LLEexp <- Reduce(rbind, LLEacc)

#export the results
setwd("C:\\Users\\DSBA-0\\Google Drive\\R16RF\\results\\RF4_NLwholeData")
save(PCAout, file = "PCAout_data13.rda")
save(tSNEout, file = "tSNEout_data13.rda")
save(LLEout, file = "LLEout.rda")

write.csv(PCAexp, "PCAacc30_data13.csv", row.names = FALSE)
write.csv(tSNEexp, "tSNEacc30_data13.csv", row.names = FALSE)
write.csv(LLEexp, "LLEacc30.csv", row.names = FALSE)


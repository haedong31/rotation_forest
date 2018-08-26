#Rotation forest with various embedding methods
#Writer: Haedong Kim

install.packages("data.table", dependencies = TRUE)
install.packages("Matrix", dependencies = TRUE)
install.packages("lle", dependencies = TRUE)
install.packages("Rtsne", dependencies = TRUE)
install.packages("rpart", dependencies = TRUE)
install.packages("caret", dependencies = TRUE)
install.packages("FNN", dependencies = TRUE)

library(data.table)
library(Matrix)
library(Rtsne)
library(lle)
library(rpart)
library(caret)
library(FNN)

###############
#Main function
###############

#Data preparation
#Trial 1: Iris data
myData = datasets::iris

#Trial 2: Binary taget data
setwd("C:\\Users\\H.Kim\\Google Drive\\RF")
myData = read.csv("data13.csv", stringsAsFactors = FALSE)
myData <- myData[ , -1]

#How many times do you want to repeat the experiment?
itrExp <- 30

#Initialization for looping
i <- 1
#PCAout <- list()
LLEout <- list()
#tSNEout <- list()
for(i in 1:itrExp){#for begins
  #Get indices for sampling training data;
  trnIdx <- sample(1:nrow(myData), round(0.7*(nrow(myData))))
  
  #Training data
  trnX <- myData[trnIdx, 1:4]
  trnY <- as.factor(myData[trnIdx, 5])
  trnData <- data.frame(trnX, Y = trnY)
  
  #Test data
  teX <- myData[-trnIdx, 1:4]
  teY <- as.factor(myData[-trnIdx, 5])
  
  #Bootstrapping
  numRcd <- nrow(trnX)
  bootIdx <- sample(1:numRcd, round(0.75 * numRcd), replace = TRUE)
  bootX <- trnX[bootIdx, ]
  bootY <- trnY[bootIdx]
  
  # #RF with PCA
  # cat(i, "th PCA", "\n")
  # PCAout[[i]] <- fn3_PCArf(trnX, trnY, numVar = 2, bootstrapRate = 0.75, numTrees = 30, teX, teY)
  
  #RF with NL
  # cat(i, "th tSNE", "\n")
  # tSNEout[[i]] <- fn6_NLrf(x = trainingX, y = trainingY, k = 2, dimRdcMethod = "tSNE",
  #                          numTrees = 30, testX, testY)
  
  cat(i, "th LLE", "\n")
  LLEout[[i]] <- fn_embdRf(bootX, bootY, numVar = 2, teX, teY, embdMethod = "LLE", numTrees = 30)
}

PCAacc <- lapply(PCAout, function(x){x[[1]]$overall})
Reduce('+', PCAacc)[1]/30
PCAexp <- Reduce(rbind, PCAacc)

# tSNEacc <- lapply(tSNEout, function(x){x[[1]]$overall})
# Reduce('+', tSNEacc)[1]/30
# tSNEexp <- Reduce(rbind, tSNEacc)

LLEacc <- lapply(LLEout, function(x){x[[1]]$overall})
Reduce('+', LLEacc)[1]/30
LLEexp <- Reduce(rbind, LLEacc)
lapply(LLEout[[1]][[3]], function(x){x$overall})

#export the results
setwd("C:\\Users\\H.Kim\\Google Drive\\RF\\shceme2")
#save(PCAout, file = "PCAout_data13.rda")
#save(tSNEout, file = "tSNEout_data13.rda")
save(LLEout, file = "LLEout.rda")

#write.csv(PCAexp, "PCAacc30_data13.csv", row.names = FALSE)
#write.csv(tSNEexp, "tSNEacc30_data13.csv", row.names = FALSE)
write.csv(LLEexp, "LLEacc30.csv", row.names = FALSE)



#########################################################
#function 1 
#Split the feature set (variable set) into several subsets
#
#input 
#@x: independent variables
#@numVar: number of features in each subset             
#output
#@ls_subsets: A list of subsets
#########################################################
fn1_getRandomSubset <- function(x, numVar){
  #Check numVar is proper
  if(numVar > length(x)) {
    numVar <- length(x)
  }
  
  #Find the number of subsets
  numSubset <- round(length(x)/numVar)
  
  #check the number of subsets 
  if(numSubset > length(x)){
    numSubset <- length(x)
  }
  
  #give features the names for indentifing the original sequence of features
  colnames(x) <- paste0("X", 1:length(x))
  
  #shuffling features randomly
  x <- subset(x, select = sample(1:length(x))) 
  
  #pick out k-1 subsets
  ls_subsets <- list()
  i <- 1
  while(i <= numSubset - 1){
    if (is.null(dim(x)) == TRUE) {
      ls_subsets[[i]] <- x
    } else {
      ls_subsets[[i]] <- x[ , 1:numVar]
      x <- x[ , -(1:numVar)]
    }
    i <- i + 1
  }
  
  #pick out the last subset (kth subset)
  # The last kth subset may include more or less variables than the other subsets 
  # when the numVar is not a factor of the length(x)
  ls_subsets[[numSubset]] <- x
  
  return(ls_subsets)
}





#########################################################
#function 3: Nonlinear rotation forest 
#########################################################
fn_embdRf = function(trnX, trnY, numVar = 2, teX, teY, embdMethod, numTrees){
  #Check dependent variables are facotors
  if(class(trnY) != "factor"){
    stop("trnY is not a factor")
  }
  
  if(class(teY) != "factor"){
    stop("teY is not a factor")
  }
  
  fn_NLembd = function(ls_trnX, ls_varArrg, dimRdcMethod){
    switch (dimRdcMethod,
            LLE = {#LLE begins
              ls_LLE <- lapply(ls_trnX, function(x){
                lle(x, m = length(x), k = 10, reg = 2, id = TRUE)})
              ls_LLEcomp <- lapply(ls_LLE, function(x){x$Y})
              
              i <- 1
              ls_NLcomp <- list()
              for(i in 1:length(ls_LLEcomp)){#for beins
                colnames(ls_LLEcomp[[i]]) <- ls_varArrg[[i]]
                ls_NLcomp[[i]] <- ls_LLEcomp[[i]]
              }#for ends
              
              embX <- as.data.frame(Reduce(cbind, ls_NLcomp))
            },#LLE ends
            
            tSNE = {#tSNE begins
              ls_uniqX <- list()
              ls_uniqY <- list()
              ls_sameIdx <- list()
              
              #Jittering the training data until it is unique
              i <- 1
              if(sum(unlist(lapply(ls_trnX, duplicated))) != 0){
                ls_uniqX <- lapply(ls_trnX, function(x){x[!duplicated(x), ]})
                
                ?identical
                for(i in 1:length(ls_trnX)){
                  ls_trnX[[i]]
                  ls_uniqX[[i]] <- ls_trnX[[i]][!duplicated(ls_trnX[[i]]), ]
                  
                  
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
    
    return(embX)
  }
  
  #initialization
  iTree <- 1
  resultContainer <- list()
  ls_inteRlt <- list()
  for(iTree in 1:numTrees){#for begins
    
    ##############################
    #1. Neighborhood information
    ##############################
    
    #Get neighborhood information of each test data point; Index and distance
    nbhdInfo <- get.knnx(data = trnX, query = teX, k = 3)
    nbhdIdx <- nbhdInfo$nn.index #Neighborhood index
    nbhdDist <- nbhdInfo$nn.dist #Neighborhood distance
    
    #Caculate weight of nbhd of each test data point
    nbhdWeight <- t(apply(nbhdDist, 1, FUN = function(x){x/sum(x)}))
    
    
    
    ##############################
    #2. Embed the training and test data to the new space
    ##############################
    
    #Separate the training data and test data into several subsets
    ls_trnX <- fn1_getRandomSubset(trnX, numVar)
    
    #Save the variable arrangement for rearranging a sequence as the original sequence after the embedding
    ls_varArrg <- lapply(ls_trnX, colnames)
    
    #Embed the training data
    embdTrnX <- fn_NLembd(ls_trnX, ls_varArrg, embdMethod)
    
    #Rearrange the sequence of features
    embdTrnX <- embdTrnX[ , order(colnames(embdTrnX))]
    
    #Merge X and Y
    embdTrn <- data.frame(embdTrnX, Y = as.factor(trnY))
    
    #Embed the test data by utilizing local neighbors of the training data
    i <- 1
    ls_tmp <- list()
    for(i in 1:nrow(teX)){#for begins
      nbhd <- embdTrnX[nbhdIdx[i, ], ]
      newTestPt <- nbhdDist[i, ] %*% as.matrix(nbhd)
      ls_tmp[[i]] <- as.data.frame(newTestPt)
    }#for ends
    
    newTestX <- as.data.frame(rbindlist(ls_tmp))
    
    ##############################
    #3. Build trees
    ##############################
    
    #Build a tree
    oneTree <- rpart(Y ~ ., data = embdTrn, method = "class")
    #modelContainer[[iTree]] <- oneTree
    
    #Make a prediction
    tmpRlt <- predict(oneTree, newdata = newTestX, type = "prob")
    resultContainer[[iTree]] <- tmpRlt
    
    #For analysis...
    tmpRlt2 <- predict(oneTree, newdata = newTestX, type = "class")
    ls_inteRlt[[iTree]] <- confusionMatrix(tmpRlt2, teY)
  }#for ends
  
  #Aggregate the results
  aggRlt <- Reduce('+', resultContainer)
  
  #Make a prediction
  prtd <- factor(apply(aggRlt, 1, function(x){
    colnames(aggRlt)[which(x == max(x))]}), levels = colnames(aggRlt))
  
  confMx <- confusionMatrix(prtd, teY)
  
  ls_output <- list(confMx, resultContainer, ls_inteRlt)
  
  return(ls_output)
}
#confusionMatrix((predict(modelContainer[[10]], newdata = newTestX, type = "class")), teY)

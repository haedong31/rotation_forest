num_col <- 17
K <- 3

# Spliting the feature set
sets <- rep(floor(num_col / K), K)
overflow <- rep(1, num_col - sum(sets)) # remainder
overflow <- c(overflow, rep(0, K - length(overflow)))
overflow <- overflow[sample(1:length(overflow), length(overflow), FALSE)] # random shuffling
subsetsizes <- sets + overflow
fact <- factor(rep(1:K, subsetsizes))

# Declare
predicted <- list()
fit <- numeric()
Ri <- list()
Ria <- list()
fit <- list()
predicted <- matrix(NA,nrow=nrow(x),ncol=L)
subsets <- list()
SelectedClass<- list()
IndependentsClassSubset<- list()
IndependentsClassSubsetBoot<- list()
pcdata <- list()
loadings <- list()

# Fitting
for (i in 1:L) {

  #############prepare the rotation matrix RAi
  #Split F(the feature set) randomly into K subsets: Fi,j (for j= 1...K)

  #randomize order of variables
  # Independents <- x[,sample(1:ncol(x),ncol(x))]

  # n <- 0
  i <- 1

  subsets[[i]] <- list()

  SelectedClass[[i]]<- list()
  IndependentsClassSubset[[i]]<- list()
  IndependentsClassSubsetBoot[[i]]<- list()
  pcdata[[i]] <- list()
  loadings[[i]] <- list()

  varIndPerSubset <- split(sample(1:num_col, num_col, FALSE), fact)

  #For j=1...K (this is n in subsets[[n]]
  j <- 1
  for (j in seq(1,K)) {

    #let Xi,j be the dataset X for the feature set Fi,j

    # n <- n + M

    # subsets[[i]][[j]] <- data.frame(Independents[,(n-(M-1)):n],y)
    # subsets[[i]][[j]] <- data.frame(x[,(n-(M-1)):n],y)
    x <- iris[, 1:4]
    x <- cbind(x, x, x, x, iris$Sepal.Width)
    y <- iris$Species
    y <- as.character(y)
    y[y == 'setosa'] <- 1
    y[y == 'versicolor'] <- 2
    y[y == 'virginica'] <- 3
    y <- as.factor(y)

    subsets[[i]][[j]] <- data.frame(x[,varIndPerSubset[[j]]],y)

    #Eliminate from Xi,j a random subset of classes
    SelectedClass[[i]][[j]]<- as.integer(sample(levels(as.factor(y)),1))
    IndependentsClassSubset[[i]][[j]] <- subsets[[i]][[j]][subsets[[i]][[j]]$y == SelectedClass[[i]][[j]],]

    #Select a bootstrap sample from Xi,j of size 75% of the number of objects in Xi,j. Denote the new set by XijBoot
    IndependentsClassSubsetBoot[[i]][[j]] <- IndependentsClassSubset[[i]][[j]][sample(1:dim(IndependentsClassSubset[[i]][[j]])[1], round(0.75*nrow(IndependentsClassSubset[[i]][[j]])), replace = TRUE),]

    #Apply PCA on XijBoot to obtain the coefficients in a matrix Ci,j (here called loadings)
    # pcdata[[i]][[j]] <- princomp(IndependentsClassSubsetBoot[[i]][[j]][,!colnames(IndependentsClassSubsetBoot[[i]][[j]]) %in% "y"])
    pcdata[[i]][[j]] <- prcomp(IndependentsClassSubsetBoot[[i]][[j]][,!colnames(IndependentsClassSubsetBoot[[i]][[j]]) %in% "y"])
    # loadings[[i]][[j]] <- pcdata[[i]][[j]]$loadings[,]
    loadings[[i]][[j]] <- pcdata[[i]][[j]]$rotation

    #rename columns with coefficients so they can be merged properly
    #colnames(loadings[[i]][[j]]) <- gsub("V", "a",dimnames(loadings[[i]][[j]])[[1]])
    colnames(loadings[[i]][[j]]) <- dimnames(loadings[[i]][[j]])[[1]]
    #add unique ID and name it rowID (for merging)
    loadings[[i]][[j]] <- data.frame(dimnames(loadings[[i]][[j]])[[1]],loadings[[i]][[j]])
    colnames(loadings[[i]][[j]])[1] <- "rowID"

  }



  #Arrange the Ci,j, for j=1...K in a Rotation matrix Ri
  #pregrow data frame C (Coefficients = loadings)
  #because the number of columns and rows can differ we should do a loop across all loading subsets and sum the nrow's and ncol's
  #Ri <- data.frame(matrix(nrow=(n*dim(loadings[[j]])[[1]]), ncol=(2*dim(loadings[[j]])[[2]]-(j-1))) )



  Ri[[i]] <- Reduce(function(x, y) merge(x, y, by='rowID',all=TRUE),loadings[[i]])

  #Construct Ria by rearranging the columns of Ri so as to match the order of features in F (Feature set)
  #replace NA's with 0
  Ri[[i]][is.na(Ri[[i]])] <- 0


  #at this point we have to sort everything (rows and columns) for subsequent multiplication

  #sort rows and columns
  Ria[[i]] <- Ri[[i]][order(match(Ri[[i]]$rowID,colnames(x))),order(match(colnames(Ri[[i]]),colnames(x)))]

  rownames(Ria[[i]]) <- Ria[[i]]$rowID
  Ria[[i]]$rowID <- NULL


  final <- data.frame(as.matrix(x) %*% as.matrix(Ria[[i]]),y)




  #############build classifier Di using (X Ria, y) as the training set

  fit[[i]] <- rpart(y~ ., method="class", data=final,...)


}
res <- list(models=fit, loadings=Ria, columnnames=colnames(x))
class(res) <- "rotationForest"
res
}

install.packages("FNN")

library(FNN)

X <- matrix(runif(25), 5, 5)

Y <- matrix(runif(10), 2, 5)
  
get.knnx(X, Y[1, , drop = FALSE], 2)

sqrt(sum((Y[1, ] - X[2, ])^2))

?knnx.dist()

get.knn(X, k = 2)

X
dist(X, method = "euclidean")


a <- c(0, 1, 2, 3, 4, 1, 1, 4, 2, 3, 5)

?match
a[duplicated(a)]
match(a[duplicated(a)], a)

duplicated(ls_trnX[[1]])


b <- c(1, 2, 3)
b[duplicated(b)]
match(b[duplicated(b)], b)
which(duplicated(b))

predict_rotation_forest <- function(object, newdata, predict_all = FALSE) {

  # checkt the class of the given object?
}


# 0. toy example -------------------------------------------------------
df <- iris
x <- cbind(df[, 1:4])
y <- as.factor(df[['Species']])
k <- 2
boot_rate <- 0.75

#Iris data set
str(iris)

iris_subset <- which(1:length(iris[,1])%%5==0)
iris_test <- iris[iris_subset,]
iris_train <- iris[-iris_subset,]

install.packages("neuralnet")
library(neuralnet)

nnet_iris_train <-iris_train

#Binarize the categorical output
nnet_iris_train <- cbind(nnet_iris_train, iris_train$Species == "setosa")
nnet_iris_train <- cbind(nnet_iris_train, iris_train$Species == "versicolor")
nnet_iris_train <- cbind(nnet_iris_train, iris_train$Species == "virginica")

names(nnet_iris_train)[6] <- "setosa"
names(nnet_iris_train)[7] <- "versicolor"
names(nnet_iris_train)[8] <- "virginica"

NN_Model <- neuralnet(setosa+versicolor+virginica ~ Sepal.Length + Sepal.Width +
                       Petal.Length + Petal.Width, data=nnet_iris_train, hidden=c(3))
plot(NN_Model)

NN_predict <- compute(NN_Model, iris_test[-5])$net.result

# Consolidate multiple binary output back to 
#categorical output
maxidx <- function(arr) {
 return(which(arr == max(arr)))
}

idx <- apply(NN_predict, c(1), maxidx) #classify as 1, 2 or 3
prediction <- c("setosa", "versicolor", "virginica")[idx] #convert to species
table(prediction, iris_test$Species) # Confusion matrix


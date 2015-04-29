positive = read.csv('Tyrosine/positive_train5.csv')
negative = read.csv('Tyrosine/negative_train5.csv')

ppm = positive[,1] # First column are the variable names
train = t(cbind(positive[,-1],negative[,-1])) # combine them except the first column
train = as.data.frame(train) # Now convert it to a data frame
train$label = 0 # Now add in a label column initially set to all 0's
train$label[1:94] = 1 # Now set the first 94 samples to 1 to indicate they are positive
train$label = factor(train$label)

colnames(train) = c(ppm,'label') # Now correct the column names
rownames(train) = 1:nrow(train) # Now add the row names

#### This only needs to run once ####
# One way to handle the data is to divide into 5 regions
n.regions = 1
regions.start = c()
regions.end = c()
regions.start[n.regions] = 1
dist.cutoff = 0.01
for (i in 2:length(ppm)) {
  if (ppm[i-1]-ppm[i] > dist.cutoff) {
    regions.end[n.regions] = i-1  
    n.regions = n.regions + 1
    regions.start[n.regions] = i
  }
}
regions.end[n.regions] = ncol(train)-1
####



engineer_features = T
if (engineer_features == T) {
  random.order = sample(1:nrow(train),nrow(train))
  train = train[random.order,]
  
  # ensure the results are repeatable
  set.seed(7)
  # load the library
  library(mlbench)
  library(caret)
  library(lattice)
  library(ggplot2)
  library(e1071)
  # load the data
  #data(train)
  # define the control using a random forest selection function
  print('before rfeControl')
  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  
  for (j in 5:5) {
    # run the RFE algorithm
    print('before rfe')
    #results <- rfe(train[,1:10], train[,length(train)], sizes=c(1:10), rfeControl=control)
    results <- rfe(train[,regions.start[j]:regions.end[j]], train[,length(train)], sizes=c(1:(regions.end[j]-regions.start[j]+1)), rfeControl=control)
    #results <- rfe(train[50:150,1:199], train[50:150,length(train)], sizes=c(1:199), rfeControl=control)
    # summarize the results
    print(results)
    # list the chosen features
    predictors(results)
    # plot the results
    plot(results, type=c("g", "o"))
    #features = results.data.frame
    #colnames(features) = cnames
    #features$label = train$label
    ####
  }
  write.csv(results$optVariables, file="5Reg.csv")
#   
#   features = as.data.frame(features)
#   colnames(features) = cnames
#   features$label = train$label
  ####
} else {
  features = train
}  




# k-fold cross-validation loop
library(rpart)
library(randomForest)
k = 3
positive_accuracy = 0
negative_accuracy = 0
train_accuracy = 0
accuracy = 0
random.order = sample(1:nrow(features),nrow(features))
features = features[random.order,]
for (j in 1:(ncol(features)-1)) {
  colnames(features)[j] = paste('X',j,sep='')
}
size = round(nrow(features)/k)
for (i in 1:k) {
  start = 1 + size*(i-1)
  #end = min(nrow(train),size + size*(i-1))
  if (i == k) {
    end = nrow(features)
  } else {
    end = size + size*(i-1)
  }
  test.set = features[start:end,]
  train.set = features[-c(start:end),]
  #fit <- rpart(label ~ ., data=train.set)
  fit <- randomForest(label ~ ., data=train.set)
  #predictions = predict(fit,test.set,type='vector')-1 # They use 1 to indicate class 1 which is actually 0
  predictions = predict(fit,test.set,type='response')
  correct_predictions = test.set$label
  accuracy = accuracy + length(which(predictions == correct_predictions))/length(predictions)
  if (length(which(correct_predictions==1)) > 0) {
    positive_accuracy = positive_accuracy + length(which(predictions == correct_predictions & correct_predictions == 1))/length(which(correct_predictions==1))
  }
  negative_accuracy = negative_accuracy + length(which(predictions == correct_predictions & correct_predictions == 0))/length(which(correct_predictions==0))

  #predictions = predict(fit,train.set,type='vector')-1 # They use 1 to indicate class 1 which is actually 0
  predictions = predict(fit,train.set,type='response') # They use 1 to indicate class 1 which is actually 0
  correct_predictions = train.set$label
  train_accuracy = train_accuracy + length(which(predictions == correct_predictions))/length(predictions)
}
accuracy = accuracy / k
train_accuracy = train_accuracy / k
positive_accuracy = positive_accuracy / k
negative_accuracy = negative_accuracy / k
print('The training accuracy is')
print(train_accuracy)
print('The accuracy is')
print(accuracy)
print('The negative accuracy is')
print(negative_accuracy)
print('The positive accuracy is')
print(positive_accuracy)


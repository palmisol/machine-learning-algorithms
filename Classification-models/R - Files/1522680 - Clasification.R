### Load Data ####
irisData = read.csv("/Users/mps/Desktop/YEAR 3/CS3002 - Artificial Intelligence/CS3002 Labs/CS3002 Lab2/Data/iris.csv")
irisRealData = read.csv("/Users/mps/Desktop/YEAR 3/CS3002 - Artificial Intelligence/CS3002 Labs/CS3002 Lab2/Data/iris_real.csv")

library(class)
library(rpart)
library(rpart.plot)

# Methods ####

# The number of correct predictions made divided by the total number of predictions made. 
getAccuracy <- function(dataPrediction, dataClass ){
  n = length(dataClass) #the number of test cases
  ncorrect = sum(dataPrediction == dataClass) #the number of correctly predicted
  accuracy=ncorrect/n
  return(accuracy)
}
printTree <- function(fit, type, title){
  rpart.plot(x = fit, type = type, main = title, box.palette = list("#552586","#804FB3","#B589D6"))  
}
splitScreen <- function(x ,y){
  par(mfrow=c(y,x))
}

# 1. Learn a decision tree for the 3 classes of iris. Remember to save some data for testing. #### 
#    You will also need to mix up the ordering of the rows – use iris_rand=iris[sample(150,150),] to do this. ####

newIrisData <- cbind(irisRealData,irisData)

newIrisData = newIrisData[sample(149,149),] #Randomise data

irisClass = newIrisData[,1] #Extract the first column
irisValues = newIrisData[,-1] #All but the first column

#set Training values 
irisClassTrain = irisClass [1:100]
irisValuesTrain = irisValues [1:100,]

#and testset
irisClassTest = irisClass [101:149]
irisValuesTest = irisValues [101:149,]

# rpart predict the classes according to the values and creates the tree
# Method = classification

fit <- rpart(formula = irisClassTrain~., data = irisValuesTrain, method="class") 

# Adavantages of decision trees
# 1. comprenhsive: shows all possible outcomes 
# 2. Specific: clears up amibiguity
# 3. Easy to use: visually presents all decision alternatives
# 4. Versatile: multitud of business problems

rpart.plot(x = fit, type = 4, main = "Decision Tree for IrisData", 
           box.palette = list("#552586","#804FB3","#B589D6"))  

# 2. Test it by scoring the accuracy on test data using different degrees of pruning using prune.####

IrisPrediction <- predict(fit, irisValuesTest, type = 'class')

# Create a data frame with the real and predicted values. 
predcitDataFrame = data.frame(irisValuesTest, irisClassTest, IrisPrediction)

# get the percentage of predcited values that are right  
accuracy = getAccuracy(IrisPrediction, irisClassTest)
print(accuracy)    

table_mat = table(irisClassTest, IrisPrediction)

# Confussion matrix 
print(table_mat)  

# Complexity Parameter and Prunning

# Select the complexity parameter value for pruning the tree which has lowest cross valiadation error represented as #‘xerror’

printcp(fit)
plotcp(fit)

# The Complexity Parameter, it is employed to control the size of the decision tree and to select the optimal tree size.If the cost of adding another variable to the decision tree from the current node is above the value of cp, then tree building does not continue

accuracyVector <- vector( mode = "numeric", length = 3)
cpArray = c(0.04, 0.5, 0.8)

splitScreen(2, 2)

printTree(fit, 4, "Unpruned Decision Tree")

# Pruning prevents overfitting, reduce complexity and increases accuracy
# Reduce the size of the tree by removing sections that provide little influence to the classify instances. 
# 1. Goes through each decision node
# 2. Consider converting into a leaf node
# 3. If this does not reduce classification accuracy then the prunning is carried out

for ( i in 1:3) {
  
prunedFit <- prune(fit, cp = cpArray[i])
printTree(prunedFit, 4, sprintf('Decision tree with cp = %g', cpArray[i]))
accuracyVector[i] = getAccuracy(predict(prunedFit, irisValuesTest, type = 'class'), irisClassTest)  
}

# 3. Scatterplot 2 selected variables of the data (and colour code according to the decision
#    tree output), display the learnt tree and calculate the accuracy. ####

splitScreen(1,1)

printTree(fit, 4, "Decision Tree for IrisData")

plot(x = irisValuesTrain$X5.1, y = irisValuesTrain$X3.5, col = c(irisClassTrain))  

print(accuracy)   

splitScreen(1,2)

plot(x = irisValuesTest$X5.1, y = irisValuesTest$X3.5, 
     col = c(irisClassTest), main = "Test values")

plot(x = irisValuesTest$X5.1, y = irisValuesTest$X3.5, 
     col = c(IrisPrediction), main = "Predicted values")


# 4. Compare the accuracy of the different pruned trees to KNN with different values of k #### 

# Knn looks for K number of neigbour using one of the distance metrics

rep = 10

accuracyKnnVector <- vector( mode = "numeric", length = rep)
Kvector <- vector( mode = "integer", length = rep)

for (i in 1 : rep) {

  knnPrediction = knn(irisValuesTrain, irisValuesTest, irisClassTrain, k = i + 1)
  accuracyKnnVector[i] = getAccuracy(knnPrediction, irisClassTest)
  Kvector[i] = i + 1
}
df = data.frame("K" = Kvector, "KNN" = accuracyKnnVector, 
                "CP 0.04" = accuracyVector[1], "CP 0.5" = accuracyVector[2], "CP 0.8" = accuracyVector[3])

print(df)








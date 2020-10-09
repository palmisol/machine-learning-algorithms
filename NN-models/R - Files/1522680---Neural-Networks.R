library(neuralnet)

# Methods ####

getNN <- function (formula, data, hidden, rep){

  NN = neuralnet(formula, data, hidden,
                 threshold = 0.001, stepmax = 1e+05, linear.output = FALSE, rep)
  return(NN)
}
getPrediction <- function (NN, dataIn, n){
  predictNN = compute(NN, dataIn) 
  predictNN$neurons
  predictNN$net.result
  predict_out = as.numeric(predictNN$net.result > n) # if the number is >= n then round up or down
  return(predict_out)
}
getAccuracy <- function(dataPrediction, dataClass ){
  n = length(dataClass) #the number of test cases
  ncorrect = sum(dataPrediction == dataClass) #the number of correctly predicted
  accuracy=ncorrect/n
  return(accuracy)
}

## Now try some other inputs and targets are the test set. See if you can set up the XOR problem (below) and see what happens when you try and learn the weights. ####

### TRAIN ###
dataIn = rbind(c(-1,-1), c(1,-1), c(-1,1), c(1,1)) # XOR gate input data
dataOut = rbind(0, 1, 1, 0) # XOR gate output data

# Fit neural network with no hidden layers
XORdat <- cbind(dataOut,dataIn)
NN = getNN(XORdat[,1]~.,XORdat[,-1], 0, 1)

plot(NN)
NN$weights

### TEST ####
# Set up the input sequence
dataIn = rbind(c(-1,-1),c(1,-1),c(-1,1), c(1,1)) # INCORRECT it should be (0,1,1,0)
print(getPrediction(NN, dataIn, 0.5))

# IT DOES NOT LEARN THE PROBLEM CORRECTLY

## Multilayer NNs ####

### TRAIN ###
dataIn = rbind(c(1,1), c(1,-1), c(-1,1), c(-1,-1)) #XOR gate input data
dataOut = rbind(0, 1, 1, 0) #XOR gate output data

XORdat <- cbind(dataOut,dataIn)
  
NN = getNN(XORdat[,1]~.,XORdat[,-1], c(2,2,2), 1)

plot(NN)

### TEST ###
dataIn = rbind(c(1,1), c(1,-1), c(-1,1), c(-1,-1)); # Now simulate using the same inputs used to train it 
#dataOut = rbind(0,1,1,0)

prediction = getPrediction(NN, dataIn, 0.5)
print(prediction) #It should be 0,1,1,0

# Repeat the training a few times (remember to initialize the networks beforehand). ####
# Try changing the number of epochs and the goal parameter to see what effect it has. #

# Epochs are the number of repetitions

XORdat <- cbind(dataOut,dataIn) # Change Goal parameter

v0000 = vector(mode = "logical", length = 5)
v0001 = vector(mode = "logical", length = 5)
v0011 = vector(mode = "logical", length = 5)
v0111 = vector(mode = "logical", length = 5)
v0 = vector(mode = "logical", length = 5)

output = data.frame(matrix(vector(), 16, 16),
                    stringsAsFactors=F)



for (j in 1:16) {
  
  dataOut = sample(c(0,1), replace=TRUE, size = 4)
  output[1,j] =   toString(dataOut)
  
  # for (i in 1:5) {
  #   
  #    fit = neuralnet(formula = XORdat[,1]~., data = XORdat[,-1], hidden = c(2,2,2),
  #                     threshold = 0.001, stepmax = 1e+05, linear.output = FALSE, rep = i)
  #    
  # }

}




predictionVec = vector(mode = "character", length = 3)
epochsVec = vector(mode = "character", length = 3)

dataIn = rbind(c(1,1), c(1,-1), c(-1,1), c(-1,-1));
dataOut = sample(c(0,1), replace=TRUE, size = 4) # Random output data
print(dataOut)

NN = getNN(XORdat[,1]~.,XORdat[,-1], c(3,3,3))

for (i in 1:3) {
  epochsVec[i] = epochs[i]
  predictionVec[i] = toString(getPrediction(NN, dataIn, epochs[i]))
}
resultDf = data.frame(epochsVec, predictionVec)
resultDf

### ASSESSED EXERCISE ####

wineData = read.csv("/Users/mps/Desktop/YEAR 3/CS3002 - Artificial Intelligence/CS3002 Labs/CS3002 Lab3/Data/winedata2.csv")

# 1. Build the architecture of your neural network. The output must be between one and zero ####

wineClass = wineData[,1] # Extract the first column
wineValues = wineData[,-1] # All but the first column

# Replace values
wineClass = replace(wineClass, wineClass == 1, 0)
wineClass = replace(wineClass, wineClass == 2, 1)

#Normalise the attributes
wineValues <- (wineValues - min(wineValues))/(max(wineValues)- min(wineValues))

# TRAIN VALUES
wineClassTrain = wineClass [1:65]
wineValuesTrain = wineValues [1:65,]

wineTrain = cbind(wineClassTrain, wineValuesTrain)
wineTrain = wineTrain[sample(65,65),] # Randomise data

# TEST Values
wineClassTest = wineClass [66:130]
wineValuesTest = wineValues [66:130,]

# 2. Using any two variables from the wine data, set up the data as you did for the ####
# linear classifier with a train and test set #
# 3. Train the neural network on half of the data and test it on the remaining. ####
# 4. Calculate the accuracy ####

## TRAIN ##

values <- cbind(wineClassTrain, wineValuesTrain$Alcohol, wineValuesTrain$Malic.acid)
NN = getNN(values[,1]~., values[,-1], c(3,3)) 

## TEST ##

test_subset <- subset(wineValuesTest, select = c("Alcohol", "Malic.acid"))
prediction = getPrediction(NN, test_subset, 0.5)
accuracy = getAccuracy(prediction, wineClassTest)
accuracy

resultDataFrame = data.frame(wineClassTest, prediction)
print(resultDataFrame)

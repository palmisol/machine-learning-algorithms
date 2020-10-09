library(neuralnet)

# Methods ####

getNN <- function (formula, data, hidden, rep){

#Formula = a symbolic description of the model to be fitted.

#Data = a data frame containing the variables specified in formula

#Threshold (umbral) = It determines, based on the inputs, whether the perceptron fires or not. Basically, the perceptron takes all of the weighted input values and adds them together. If the sum is above or equal to this value (called the threshold) then the perceptron fires and takes the activated value (typically 1); otherwise it takes the deactivated value (typically -1).# 

#Stepmax = the maximum steps for the training of the neural network. Reaching this maximum leads to a stop of the neural network's training process.

#Linear.output = This is used to specify whether we want to do regression linear.output=TRUE or classification linear.output=FALSE

set.seed(2) #setting the sequence 
  NN = neuralnet(formula, data, hidden,
                 threshold = 0.001, stepmax = 1e+05, linear.output = FALSE, rep)
return(NN)
}
getPrediction <- function (NN, dataIn, n){
  #Mathematical function that allows you to determine the ouput of the prediction
  predictNN = compute(NN, dataIn) 
  
  print("Neurons")
  print(predictNN$neurons)
  
  print("Neurons result")
  print(predictNN$net.result)
  
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
dataIn = rbind(c(-1,-1),c(1,-1),c(-1,1), c(1,1)) 
print(getPrediction(NN, dataIn, 0.5)) # INCORRECT it should be (0,1,1,0)

# IT DOES NOT LEARN THE PROBLEM CORRECTLY

## Multilayer NNs ####

### TRAIN ###
dataIn = rbind(c(1,1), c(1,-1), c(-1,1), c(-1,-1)) #XOR gate input data
dataOut = rbind(0, 1, 1, 0) #XOR gate output data

XORdat <- cbind(dataOut,dataIn)
  
NN = getNN(XORdat[,1]~.,XORdat[,-1], c(3,3), 1)

plot(NN)

### TEST ###
dataIn = rbind(c(1,1), c(1,-1), c(-1,1), c(-1,-1)); # Now simulate using the same inputs used to train it 
#dataOut = rbind(0,1,1,0)

prediction = getPrediction(NN, dataIn, 0.5)
print(prediction) #It should be 0,1,1,0

# Repeat the training a few times (remember to initialize the networks beforehand). ####
# Try changing the number of epochs and the goal parameter to see what effect it has. #

# Epochs are the number of repetitions

output = data.frame(matrix(vector(), 16, 16), stringsAsFactors=F) #create a data frame to present the data

## Create 16 random values
for (j in 1:16) {
  dataOut = sample(c(0,1), replace=TRUE, size = 4)
  output[1,j] = toString(dataOut)
  
}

for (i in 3:16) {
  for (j in 1:16) {

    dataOut = as.numeric(unlist(strsplit(output[1,j], ",")))
    XORdat <- cbind(dataOut,dataIn)
    
    fit = neuralnet(formula = XORdat[,1]~., data = XORdat[,-1], hidden = c(3,3),
                    threshold = 0.001, stepmax = 1e+05, linear.output = FALSE, rep = i)
    
    prediction = getPrediction(fit, dataIn, 0.5)
    output[i,j] = toString(getAccuracy(dataOut, prediction));
    
  }
}

### ASSESSED EXERCISE ####

wineData = read.csv("/Users/mps/Desktop/YEAR 3/CS3002 - Artificial Intelligence/CS3002 Labs/CS3002 Lab3/Data/winedata2.csv")

# 1. Build the architecture of your neural network. The output must be between one and zero ####

wineClass = wineData[,1]-1 # Extract the first column and minus al values
wineValues = wineData[,-1] # All but the first column

#Normalise the attributes
#Change the values of numeric columns in the dataset to a common scale, without distorting differences in the ranges of values.

wineValues <- (wineValues - min(wineValues))/(max(wineValues)- min(wineValues))

wineData = cbind(wineClass, wineValues)
wineData = wineData[sample(130,130),] # Randomise data

#wineDataRand = wineData[,c("wineClass","Proline", "Magnesium")]

#TRAIN
wineTrain = wineData[1:100,]

# TEST
wineTest = wineData[101:130,]

# 2. Using any two variables from the wine data, set up the data as you did for the ####
# linear classifier with a train and test set #
# 3. Train the neural network on half of the data and test it on the remaining. ####
# 4. Calculate the accuracy ####

## TRAIN ##

NN = getNN(formula =wineTrain[,1]~., 
           data = cbind(wineTrain$Proline, wineTrain$Magnesium), hidden = c(3,3), rep = 1)

plot(NN)

## TEST ##

dataIn <- cbind(wineTest$Proline, wineTest$Magnesium)

prediction = getPrediction(NN, dataIn, 0.5)
accuracy = getAccuracy(prediction, wineTest[,1])
accuracy

resultDataFrame = data.frame(wineTest[,1], prediction)
print(resultDataFrame)

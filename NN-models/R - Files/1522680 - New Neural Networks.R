library(neuralnet)

# Methods ####
getNN <- function (formula, data, hidden, threshold, stepmax, rep){
  set.seed(2)
  NN = neuralnet(formula, data, hidden,
                 threshold, stepmax, linear.output = FALSE, rep)
  return(NN)
}
getPrediction <- function (NN, dataIn, n){
  predictNN = compute(NN, dataIn) 
  predict_out = as.numeric(predictNN$net.result > n) # if the number is >= n then round up or down
  return(predict_out)
}
getAccuracy <- function(dataPrediction, dataClass ){
  n = length(dataClass) #the number of test cases
  ncorrect = sum(dataPrediction == dataClass) #the number of correctly predicted
  accuracy=ncorrect/n
  return(accuracy)
}

## Assessed exercise ####
 
wineData = read.csv("/Users/mps/Desktop/YEAR 3/CS3002 - Artificial Intelligence/CS3002 Labs/CS3002 Lab3/Data/winedata2.csv")

wineClass = wineData[,1] # Extract the first column
wineValues = wineData[,-1] # All but the first column

# Replace values
wineClass = replace(wineClass, wineClass == 1, 0)
wineClass = replace(wineClass, wineClass == 2, 1)

#Normalise the attributes
wineValues <- (wineValues - min(wineValues))/(max(wineValues)- min(wineValues))

#TRAIN SET 
wineClassTrain = wineClass [1:78]
wineValuesTrain = wineValues [1:78,]

wineTrain = cbind(wineClassTrain, wineValuesTrain)
wineTrain = wineTrain[sample(78,78),] # Randomise data

#DEV SET 
wineClassDev = wineClass [79:104]
wineValuesDev = wineValues [79:104,]

wineDev = cbind(wineClassDev, wineValuesDev)
wineDev = wineTrain[sample(104,104),] # Randomise data

#TEST SET
wineClassTest = wineClass [105:130]
wineValuesTest = wineValues [105:130,]

# Set dev the Neural Network
values <- cbind(wineClassDev, wineValuesDev$Alcohol, wineValuesDev$Malic.acid)

v1 = vector(mode="logical", length = 1000000)
stepsVector = vector(mode="logical", length = 1000000)

#Rep = epoch

for (i in 1000:length(v1)) {
  NN =  neuralnet(formula = values[,1]~., data = values[,-1], hidden = c(3,3),
                  threshold = 0.0001, linear.output = TRUE, rep = 1, stepmax = 1e+05)
  stepsVector[i] = i
  test_subset <- cbind(wineValuesTest$Alcohol, wineValuesTest$Malic.acid)
  prediction = getPrediction(NN, test_subset, 0.6)
  accuraccy = getAccuracy(prediction, wineClassTest)
  v1[i] = accuraccy
}

output = data.frame(stepsVector, v1)
output








wineData = read.csv("/Users/mps/Desktop/YEAR 3/CS3002 - Artificial Intelligence/CS3002 Labs/CS3002 Lab3/Data/winedata2.csv")

library(neuralnet)

# Methods ####
getPredictionNN <- function(dataIn, dataOut, testIn, h, n = 0){
  
  #NN
  XORdat = cbind(dataOut,dataIn)
  set.seed(2)
  NN = neuralnet(XORdat[,1]~., XORdat[,-1], hidden = h ,
                 threshold = 0.001, stepmax = 1e+05, linear.output = FALSE)
  
  #Prediction
  predictNN = compute(NN, testIn) 
  predictNN$neurons
  predictNN$net.result
  predict_out = as.numeric(predictNN$net.result > n)
  resultList = list(predict_out, NN)
  return(resultList)
}

## Now try some other inputs and targets are the test set. See if you can set up the XOR problem (below) and see what happens when you try and learn the weights. ####

### TRAIN ###

dataIn = rbind(c(-1,-1), c(1,-1), c(-1,1), c(1,1)) # XOR gate input data
dataOut = rbind(0, 1, 1, 0) # XOR gate output data










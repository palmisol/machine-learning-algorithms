library(neuralnet)

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



wineData = read.csv("/Users/mps/Desktop/YEAR 3/CS3002 - Artificial Intelligence/CS3002 Labs/CS3002 Lab3/Data/winedata2.csv")

wineClass = wineData[,1]-1 # Extract the first column
wineValues = wineData[,-1] # All but the first column

wineValues <- (wineValues - min(wineValues))/(max(wineValues)- min(wineValues))

wineData = cbind(wineClass, wineValues)
wineData = wineData[sample(130,130),] # Randomise data


wineDataRand = wineData[,c("wineClass","Proline", "Magnesium")]

wineTrain = wineDataRand[1:100,]
wineTest = wineDataRand[101:130,]


set.seed(2)
NN_2 = neuralnet(wineClass ~ Proline + Magnesium, data = wineTrain, hidden = c(3,3),
               threshold = 0.001, stepmax = 1e+05, linear.output = FALSE)

plot(NN_2)

test_subset_ <- subset(wineTest, select = c("Proline", "Magnesium"))
predict_test = compute(NN_2, test_subset_)
predict_out = as.numeric(predict_test$net.result > 0.5)
print(predict_out)

n = length(wineTest[,1])
ncorrect = sum(predict_out == wineTest[,1])
accuracy = ncorrect/n
print(accuracy)

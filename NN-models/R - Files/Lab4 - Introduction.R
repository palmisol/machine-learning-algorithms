WineData = read.csv("/Users/mps/Desktop/YEAR 3/CS3002 - Artificial Intelligence/CS3002 Labs/CS3002 Lab3/Data/winedata.csv")

install.packages("neuralnet")
library(neuralnet)

## Perceptron ####

#OR gate input data
trainin = rbind(c(1,1), c(1,-1), c(-1,1), c(-1,-1)); 

#OR gate output data
trainout = rbind(1, 1, 1, 0);

#Combined OR gate data
ORdat=cbind(trainout,trainin)

# fit neural network with no hidden layers
set.seed(2)
NN = neuralnet(ORdat[,1]~., ORdat[,-1], hidden = 0 , threshold = 0.001, stepmax = 1e+05, 
               linear.output = FALSE)
#visualise the NN
plot(NN)
NN$weights

testin= rbind(c(1,1))
predict_testNN = compute(NN, testin)

predict_testNN$net.result

predict_out = as.numeric(predict_testNN$net.result>0.5)
print(predict_out)

#set up the input sequence
testin=rbind(c(1,1),c(1,-1),c(-1,1), c(-1,-1))

predict_testNN = compute(NN, testin)
predict_testNN$neurons
predict_testNN$net.result
predict_out = as.numeric(predict_testNN$net.result>0.5) 
predict_out



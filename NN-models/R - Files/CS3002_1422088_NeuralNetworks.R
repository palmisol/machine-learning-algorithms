library(neuralnet)

### Q1.1 - Try Now try some other inputs and targets are the test set. See if you can set up the XOR problem (below) and see what happens when you try and learn the weights. ####

# Does it work ? NO - XOR problem can not be classified linearly/with straight line

# XOR Gate input data - only false if both values are -1
traindata_in <- rbind(c(1 , 1), c(1 , -1), c(-1 , 1), c(-1 , -1))
# OR Gate output data
traindata_out <- rbind(0, 1, 1, 0)
# combined data
xor_data <- cbind(traindata_out, traindata_in)

# fit neural network with no hidden layers
set.seed(2) # setting the seed allows for the same random numbers to be used as the weights for the neurons on the network

# creating a single layer perceptron - multi-layer becomes a neural network

# All inputs are multiplied by their weights, summed together and then apply this value to the activation function to determine the output

# Activation function are mathematical functions that are used to determine what the output of the neural network or perceptron is - can either be linear or non-linear 

# the use of backpropagation to train the neural network - adjusting the weights based on the predictions and whether they were correct or not 

# neuralnet(the_labels_in_first_column ~ . means_all_attributes, data_frame_to_be_fitted_all_but_first_row, hidden neurons in the layer - hidden = c(2, 1, 3) would mean 3 layers with 2 1 and 3 neurons, threshold is set to 0.001, meaning that if the change in error during an iteration is less than 0.1%, then no further optimisation will be carried out by the model - stopping criteria, the maximum steps for the training of the neural network - will control how long your neural network trains - default is 100,000 iterations - 1e+05 is 30 seconds, if linear.output is set to true then differentible function is applied to the results from the neurons and weights)
NN = neuralnet(xor_data[,1] ~ ., xor_data[,-1], hidden = 0, threshold = 0.001, stepmax = 1e+05, linear.output = FALSE)

# A bias node acts like a regression intercept.  It is a constant learned outside of your input data. It allows you to shift the learned model. Without it, any zero or negative inputs would give an output of zero.
plot(NN)
NN$weights # (1) is bias and (2)+ are weights of the nodes

test_in = rbind(c(1, 1),c(1, -1),c(-1, 1), c(-1, -1))

# if using compute method to test the NN
predict_test_NN = compute(NN, test_in)
# The activation of the output neuron is here
predict_test_NN$net.result
# To calculate the discrete class we threshold it at 0.5:
predict_out = as.numeric(predict_test_NN$net.result > 0.5)
print(predict_out)

#### Q1.2 - Set up a 2 layer Neural network to solve the XOR problem with three hidden layers, each with 2 neurons in, and one output layer using neuralnet ####

# XOR Gate input data - only false if both values are -1
traindata_in <- rbind(c(1 , 1), c(1 , -1), c(-1 , 1), c(-1 , -1))
# OR Gate output data
traindata_out <- rbind(0, 1, 1, 0)
# combined data
xor_data <- cbind(traindata_out, traindata_in)

# fit neural network with no hidden layers
set.seed(2) # setting the seed allows for the same random numbers to be used as the weights for the neurons on the network

# neuralnet(the_labels_in_first_column ~ . means_all_attributes, data_frame_to_be_fitted_all_but_first_row, hidden neurons in the layer - hidden = c(2, 1, 3) would mean 3 layers with 2 1 and 3 neurons, threshold is set to 0.001, meaning that if the change in error during an iteration is less than 0.1%, then no further optimisation will be carried out by the model - stopping criteria, the maximum steps for the training of the neural network - will control how long your neural network trains - default is 100,000 iterations - 1e+05 is 30 seconds, if linear.output is set to true then differentible function is applied to the results from the neurons and weights, rep is the number of epochs/repetitions to be processed)
NN = neuralnet(xor_data[,1] ~ ., xor_data[,-1], hidden = c(3, 3), threshold = 0.001, stepmax = 1e+05, linear.output = FALSE, rep = 5)

# A bias node acts like a regression intercept.  It is a constant learned outside of your input data. It allows you to shift the learned model. Without it, any zero or negative inputs would give an output of zero.
plot(NN)
NN$weights # (1) is bias and (2)+ are weights of the nodes

test_in = rbind(c(1, 1),c(1, -1),c(-1, 1), c(-1, -1))

# if using compute method to test the NN
predict_test_NN = compute(NN, test_in)
# The activation of the output neuron is here
predict_test_NN$net.result
predict_test_NN$neurons
# To calculate the discrete class we threshold it at 0.5:
predict_out = as.numeric(predict_test_NN$net.result > 0.5)
print(predict_out)

### Q2 - Build a Neural network classifier of the wine or face data ####
### Q2.1 - Data Organisation ###

wine_data = read.csv("/Users/mps/Desktop/YEAR 3/CS3002 - Artificial Intelligence/CS3002 Labs/CS3002 Lab3/Data/winedata2.csv")

### Q2.2 - Build Build the architecture of your neural network. The output must be between one and zero. ####

wine_class <- wine_data[,1] - 1
wine_attrib <- wine_data[,-1]

wine_attrib = (wine_attrib-min(wine_attrib))/(max(wine_attrib)-min(wine_attrib)) #to normalise the data between 0 and 1 --> z(i) = x(i) - min(x) / max(x) - min(x) <-- where x is the data we want to normalise

# sorting data
wine_data_2 = cbind(wine_class, wine_attrib)
wine_data_2 = wine_data_2[sample(130, 130),]

# training data
training_data = wine_data_2[1:100,]

# test set 
wineclasstest = wine_data_2[101:130, 1]
wineattributestest = wine_data_2[101:130, -1]

### Q2.3 & 4 - Using any two variables from the wine data, set up the data as you did for the linear classifier with a train and test set ####

# fit neural network with no hidden layers
set.seed(2) # The seed is a number that controls whether the Random Number Generator produces a new set of random numbers or repeats a particular sequence of random numbers. 

NN_2 = neuralnet(wine_class ~ Proline + Nonflavanoid.phenols, data = training_data, hidden = c(3, 3), threshold = 0.001, stepmax = 1e+05, linear.output = FALSE, rep = 3)

plot(NN_2)

wineattributestest <- subset(wineattributestest, select = c("Proline", "Nonflavanoid.phenols"))
# # compute function depreciated, predict() is replacement
# predict_test_NN_2 = predict(NN_2, wineattributestest)
# # view the output neuron and prediction - set the threshold at 0.5, all values above are true and below are false
# table(predict_test_NN_2[,1], predict_test_NN_2[,1] > 0.5)

# if using compute method to test the NN
predict_test_NN_2 = compute(NN_2, wineattributestest)
# The activation of the output neuron is here
predict_test_NN_2$net.result

# To calculate the discrete class we threshold it at 0.5:
predict_out = as.numeric(predict_test_NN_2$net.result > 0.5)
message("Predicted = ", predict_out, "\n", "Actual =    ", wineclasstest)

### Q2.5 - Calculate the accuracy ####

num_cases = length(wineclasstest) # the number of test cases
number_correct = sum(predict_out == wineclasstest) # the number of correctly predicted classifications
accuracy_score = number_correct / num_cases # calculating the emperical error rate based on sample / true is based on an infinite sample
message("Correct = ", number_correct, " ... Number of test cases = ", num_cases, " ... Accuracy score = ", accuracy_score)

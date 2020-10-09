#Assessed Exercises 

#___________________________________________________________

#Load iris files 

irisData = read.csv("C:\\Users\\Mario\\iCloudDrive\\Desktop\\YEAR 3\\CS3002 - Artificial Intelligence\\CS3002 Labs\\CS3002 Lab2\\Data\\iris.csv" , sep=",")
irisRealData = read.csv("C:\\Users\\Mario\\iCloudDrive\\Desktop\\YEAR 3\\CS3002 - Artificial Intelligence\\CS3002 Labs\\CS3002 Lab2\\Data\\iris_real.csv" , sep=",")

#___________________________________________________________

# Detele missing data + Standarize variables

irisData  = na.omit(irisData) 
irisData = scale(irisData) 

irisRealData  = na.omit(irisRealData)
irisRealData = scale(irisRealData ) 

#___________________________________________________________

#Distance matrix ( middle diaginal is "0")

irisDistance <- dist(irisData, method = "euclidean") 
irisRealDistance <- dist(irisRealData, method = "euclidean") 

k = 3

##############################################################################
### K-means clustering #######################################################
##############################################################################

#Cluster Solution

irisFit <- kmeans(irisData, k)
irisRealFit <- kmeans(irisRealData, k)

#Obtain mean 

aggregate(irisData,by=list(irisFit$cluster),FUN=mean)
aggregate(irisRealData,by=list(irisRealFit$cluster),FUN=mean)

#K-means clustering 

Kiris = irisFit$cluster
KirisReal = irisRealFit$cluster

---------------------------------------------------------

irisFit2 <- kmeans(irisData, 2)
irisFit3 <- kmeans(irisData, 3)

aggregate(irisData,by=list(irisFit2$cluster),FUN=mean)
aggregate(irisData,by=list(irisFit3$cluster),FUN=mean)

Kiris2 = irisFit2$cluster
Kiris3 = irisFit3$cluster

###################################################################################
### Hierarchical Clustering #######################################################
###################################################################################

# Matrix of Euclidean distances

distanceIris <- dist(irisData, method = "euclidean")
distanceIrisReal <- dist(irisRealData, method = "euclidean")

# Clustering 

irisFitH <- hclust(distanceIris, method = "average")
irisRealFitH <- hclust(distanceIrisReal, method = "average")

# Groups

Hiris <- cutree(irisFitH, k = k)
HirisReal <- cutree(irisRealFitH, k = k)


###################################################################################################
### Calculate the WK for K means clustering with different values of K ############################
###################################################################################################

# Load the WK file 
source("C:\\Users\\Mario\\iCloudDrive\\Desktop\\YEAR 3\\CS3002 - Artificial Intelligence\\CS3002 Labs\\CS3002 Lab2\\R - Files\\WK_R.r")

source("/Users/mps/Desktop/YEAR 3/CS3002 - Artificial Intelligence/CS3002 Labs/CS3002 Lab2/R - Files/WK_R.R")

wk1_K3 = WK_R(Kiris, HirisReal)
wk1_K2 = WK_R(Kiris, HirisReal)

wk2_K3 = WK_R(KirisReal, Hiris)
wk2_K2 = WK_R(KirisReal, Hiris)

vector1 <- c(wk1_K2, wk1_K3)
vector2 <- c(wk2_K2, wk2_K3)

wkIris_K2 = WK_R(Kiris, Hiris)
wkIris_K3 = WK_R(Kiris, Hiris)

plot(c(wkIris_K2, wkIris_K3))

#par(mfrow=c(1,1))

windows()
  par(mfrow=c(2,1))
  plot(vector1 , c(1 , 1))
  plot(vector2 , c(1 , 1))
  
plot(wk1_K3)
plot(wk2_K3)

plot(wk1_K2)
plot(wk2_K2)

###################################################################################################
### Try Hierarchical with different linkage measures: single, complete and average ################
###################################################################################################

irisFitAverage <- hclust(distanceIris, method = "average")
irisFitComplete <- hclust(distanceIris, method = "complete")
irisFitSingle <- hclust(distanceIris, method = "single")

HirisAverage2 <- cutree(irisFitAverage, 2)
HirisComplete2 <- cutree(irisFitComplete, 2)
HirisSingle2 <- cutree(irisFitSingle, 2)

HirisAverage3 <- cutree(irisFitAverage, 3)
HirisComplete3 <- cutree(irisFitComplete, 3)
HirisSingle3 <- cutree(irisFitSingle, 3)

wkAverage_K2 = WK_R(Kiris2, HirisAverage2)
wkAverage_K3 = WK_R(Kiris3, HirisAverage3)

wkComplete_K2 = WK_R(Kiris2, HirisComplete2)
wkComplete_K3 = WK_R(Kiris3, HirisComplete3)

wkSingle_K2 = WK_R(Kiris2, HirisSingle2)
wkSingle_k3 = WK_R(Kiris3, HirisSingle3)


###################################################################################################
### Use scatterplots to illustrate the different clusterings. #####################################
###################################################################################################

windows()
  par(mfrow=c(6,1))
  plot(irisData, col=HirisAverage2, main = "Average 2")
  plot(irisData, col=HirisAverage3, main = "Avergae 3")
  
  plot(irisData, col=HirisComplete2, main = "Compelte 2")
  plot(irisData, col=HirisComplete3, main = "Complete 3")
  
  plot(irisData, col=HirisSingle2, main = "Single 2")
  plot(irisData, col=HirisSingle3, main = "Single 3")

  
###################################################################################################
### Plot the dendrograms where necessary.##########################################################
###################################################################################################  

plot(irisFitAverage, main = "Average")
plot(irisFitComplete, main = "Complete")
plot(irisFitSingle, main = "Single")
  
  
###################################################################################################
### Plot the different Weighted Kappa values on an appropriate graph.##############################
################################################################################################### 
  
xAverage = c(wkAverage_K2, wkAverage_K3)
xComplete = c(wkComplete_K2, wkComplete_K3)
xSingle = c(wkSingle_K2, wkSingle_k3)

y = c(2, 3)

windows()
  par(mfrow=c(3,1))
  plot(xAverage, y, col = "red", type = "b", main = "Average")
  plot(xComplete, y, col = "blue", type = "b", main = "Complete")
  plot(xSingle, y, col = "green", type = "b", main = "Single")
  


 




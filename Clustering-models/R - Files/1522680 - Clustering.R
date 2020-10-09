### CLUSTER LAB 2019 ###

### LOAD DATA #### 

irisData = read.csv("/Users/mps/Desktop/YEAR 3/CS3002 - Artificial Intelligence/CS3002 Labs/CS3002 Lab2/Data/iris.csv")
irisRealData = read.csv("/Users/mps/Desktop/YEAR 3/CS3002 - Artificial Intelligence/CS3002 Labs/CS3002 Lab2/Data/iris_real.csv")
source("/Users/mps/Desktop/YEAR 3/CS3002 - Artificial Intelligence/CS3002 Labs/CS3002 Lab2/R - Files/WK_R.R")
library(ggplot2)

### Methods #### 

deleteData <- function(data){
  data  = na.omit(data) 
  data = scale(data) 
}
kMeansClustering <- function(data, k){
  
  fit <- kmeans(data, k)

  #Obtain mean 
  aggregate(data,by=list(fit$cluster),FUN=mean)

  #K-means clustering 
  Kgroups = fit$cluster
  
  output <- list("fit" = fit, "groups" = Kgroups)


}
HirearchicalClustering <- function(data, k, measure){
  
  # Matrix of Euclidean distances
  d <- dist(data, method = "euclidean")
  
  # Clustering 
  fit <- hclust(d, method = measure)
  
  # Groups
  Hgroups <- cutree(fit, k = k)
  
  output <- list("fit" = fit, "groups" = Hgroups)
  
}
getAgreementStrength <- function(wk){
  
  if(wk >= -1  & wk <= 0) {
    return = "Very Poor"
  }else if (wk > 0 & wk <= 0.2) {
    return = "Poor"
  }else if (wk > 0.2 & wk <= 0.4) {
    return = "Fair"
  }else if (wk > 0.4 & wk <= 0.6) {
    return = "Moderate"
  }else if (wk > 0.6 & wk <= 0.8) {
    return = "Good"
  }else if (wk > 0.6 & wk <= 1) {
    return = "Very Good"
  }
}
compareWK <- function(algorithm1, K, data1, data2, measure = NULL){
  
  #Declare vectors
  WKvector <- vector(mode = "integer", length = K)
  Kvector <- vector(mode = "integer", length = K)
  AgreementVector <- vector(mode = "character", length = K)

  for ( i in 1 : K) {
    
    if (algorithm1 == "kmeans") {
      alg1 = kMeansClustering(data1, i+1)$groups
    }else if (algorithm1 == "hirearchical"){
      alg1 = HirearchicalClustering(data1, i+1, measure)$groups
    }

    #calculate Weighted Kappa 
    wk = WK_R(alg1, data2$X1)
    AS = getAgreementStrength(wk)
    
    #Store Weighted Kappa
    WKvector[i] = wk
    AgreementVector[i] = AS
    Kvector[i]= i+1
  }
  output = data.frame(Kvector, WKvector, AgreementVector)
}
plotLinearRange <- function(df, colour){
  ggplot() + geom_linerange(data=df, 
                            mapping=aes(x=WKvector, ymin=upperKvector, ymax = lowerKvector), 
                            size=0.5, colour = colour)
}
splitScreen <- function(x ,y){
  par(mfrow=c(y,x))
}

### Calculate the WK for K means clustering with different values of K ###
### Plot the different Weighted Kappa values on an appropriate graph ####

# It calculates the Weighted Kappa and store it into a data frame. 
df1 <- compareWK("kmeans", 6, irisData, irisRealData)

df2 <- compareWK("hirearchical", 6, irisData, irisRealData, "single")
df3 <- compareWK("hirearchical", 6, irisData, irisRealData, "average")
df4 <- compareWK("hirearchical", 6, irisData, irisRealData, "complete")

print(df1)

print(df2)
print(df3)
print(df4)

quartz() # Open a new window on MAC
splitScreen(1, 4)

plot( y = df1$WKvector, x = df1$Kvector, 
      xlab = "value of K", ylab = "Weighted Kappa", main = "Kmeans")

plot( y = df2$WKvector, x = df2$Kvector, 
      xlab = "value of K", ylab = "Weighted Kappa", main = "Hirearchical Single")
plot( y = df3$WKvector, x = df3$Kvector, 
      xlab = "value of K", ylab = "Weighted Kappa", main = "Hirearchical Average")
plot( y = df3$WKvector, x = df3$Kvector, 
      xlab = "value of K", ylab = "Weighted Kappa", main = "Hirearchical Complete")


# Plots the line range of Weighted Kappa on the Y axis you find the upper and lower K, 
# while the Weigthed kappa is on the X axis

plot(df1[-2])

### Try Hierarchical with different linkage measures: single, complete and average ####
### Use scatterplots to illustrate the different clusterings ##########################
### Plot the dendrograms where necessary ##############################################

K = 5

quartz()
splitScreen(1, 1)  

  #Hirearchical
  singleGroup <- HirearchicalClustering(irisData, K, "single")
  completeGroup <- HirearchicalClustering(irisData, K, "complete")
  averageGroup <- HirearchicalClustering(irisData, K, "average")
  
  #Scatterplots
  plot(singleGroup$groups, main = "single")
  plot(completeGroup$groups, main = "complete")
  plot(averageGroup$groups, main = "average")

  #Dendrograms
  plot(singleGroup$fit, main = "single")
  plot(completeGroup$fit, main = "complete")
  plot(averageGroup$fit, main = "average")
  
  #Kmeans Scatterplots
  plot(kMeansClustering(irisData, 5)$groups)
  plot(kMeansClustering(irisData, 8)$groups)
  plot(kMeansClustering(irisData, 3)$groups)
  
  
  wk = WK_R(kMeansClustering(irisData, 5)$groups, irisRealData$X1)
  print(wk)
  
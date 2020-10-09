
irisRealData = read.csv("/Users/mps/AI/Lab2/iris_real.csv" , sep=",")

irisRealData  = na.omit(irisRealData) # Detele missing data
irisRealData = scale(irisRealData ) # Standarize variables

# loadData <- function() {
#   irisRealData = read.csv("/Users/mps/AI/Lab2/iris_real.csv" , sep=",")
#   
#   irisRealData  = na.omit(irisRealData) # Detele missing data
#   irisRealData = scale(irisRealData ) # Standarize variables
# }

plot(main="iris real", irisRealData)

irisRealFit <- kmeans(irisRealData, 4) 

aggregate(irisRealData,by=list(irisRealFit$cluster),FUN=mean)

irisRealGroup = irisRealFit$cluster

plot(irisRealData, col=irisRealGroup)








